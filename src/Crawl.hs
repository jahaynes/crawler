{-# LANGUAGE OverloadedStrings #-}

module Crawl where

import CountedQueue
import Communication                    (CrawlerStatus(..))
import qualified PoliteQueue as PQ
import Fetch
import Forms                            (emptyFormActions, selectFormOptions)
import Parse                            (parsePage, findPageRedirect)
import Settings
import Shared
import Types

import Control.Applicative              ((<$>), (<*>))
import Control.Concurrent               (ThreadId, myThreadId)
import Control.Concurrent.STM           (STM, atomically, readTVar, modifyTVar', newTVarIO)
import Control.Monad                    (replicateM_, when)
import Data.ByteString.Char8            (ByteString, isInfixOf)
import Data.List                        ((\\))
import Data.Maybe                       (isJust)
import Data.Time
import Network.HTTP.Conduit             (Cookie, newManager, tlsManagerSettings, mkManagerSettings)
import Network.Connection               (TLSSettings (TLSSettingsSimple))
import qualified ListT             as L
import qualified STMContainers.Set as S
import qualified STMContainers.Map as M
import Text.HTML.TagSoup.Fast   (parseTags)

createCrawler :: IO Crawler
createCrawler = do
    formInstructions <- newTVarIO emptyFormActions
    crawlerStatus <- newTVarIO RunningStatus
    urlQueue <- PQ.newIO
    storeQueue <- newQueueIO (Bounded 32)
    loggingQueue <- newQueueIO (Bounded 128)
    manager <- newManager (if ignoreBadHttpsCerts
                               then mkManagerSettings (TLSSettingsSimple True False False) Nothing
                               else tlsManagerSettings)
    cookieList <- newTVarIO []
    urlPatterns <- S.newIO
    urlsInProgress <- S.newIO
    urlsCompleted <- S.newIO
    urlsFailed <- M.newIO
    return $ Crawler formInstructions crawlerStatus urlQueue storeQueue loggingQueue manager cookieList urlPatterns urlsInProgress urlsCompleted urlsFailed

setNumCrawlers :: Crawler -> Workers -> Int -> IO ()
setNumCrawlers crawlerState workers desiredNum = do

    threadsToAdd <- atomically $ do
        currentNumCrawlers <- getActiveCrawlerCount
        case desiredNum - currentNumCrawlers of
            0 -> return 0
            threadDelta ->
                if threadDelta > 0
                    then return threadDelta
                    else do
                        L.traverse_ (`S.insert` getCrawlerThreadsToStop workers)
                            . L.take (-threadDelta)
                            . S.stream
                            . getCrawlerThreads
                            $ workers
                        return 0
    replicateM_ threadsToAdd addCrawler

    where
    addCrawler :: IO ()
    addCrawler = forkIO_ $ do
        threadId <- myThreadId
        atomically $ S.insert threadId (getCrawlerThreads workers)
        crawlUrls workers crawlerState threadId

    getActiveCrawlerCount :: STM Int
    getActiveCrawlerCount = (-) <$> sizeOfSet (getCrawlerThreads workers) <*> sizeOfSet (getCrawlerThreadsToStop workers)

crawlUrls :: Workers -> Crawler -> ThreadId -> IO ()
crawlUrls workers crawlerState threadId =

    whileActive threadId (getCrawlerThreads workers) (getCrawlerThreadsToStop workers) $ do

        nextUrl <- atomically $ PQ.readQueue threadId (getUrlQueue crawlerState)
        cookiesToSend <- atomically . readTVar . getCookieList $ crawlerState

        resetThreadClock nextUrl

        (mBodyData, redirects) <- getWithRedirects (getManager crawlerState) cookiesToSend (GetRequest nextUrl)

        case mBodyData of
            Left err -> do
                putStrLn $ "Failed to download (thread " ++ show threadId ++ ")"
                print err
                atomically $ failedDownload nextUrl
            Right (bodyData, responseCookies) ->
                processResponse bodyData redirects responseCookies nextUrl cookiesToSend

    where
    processResponse :: ByteString -> [CanonicalUrl] -> [Cookie] -> CanonicalUrl -> [Cookie] -> IO ()
    processResponse bodyData redirects responseCookies nextUrl cookiesSent = do

        let parsedTags = parseTags bodyData

        --Give meta refresh a chance to fire
        case findPageRedirect parsedTags of
            Just metaRefreshUrl -> do
                {- Chance of crawler trap here. Perhaps we should 
                    check that metaRefreshUrl hasn't already been visited -}
                let moreCookies = responseCookies ++ cookiesSent
                _ {- formResponse -} <- getWithRedirects (getManager crawlerState) moreCookies (GetRequest metaRefreshUrl)
                processResponse undefined undefined undefined nextUrl moreCookies

            Nothing -> do
                let (hrefErrors, nextHrefs, forms) = parsePage redirects parsedTags
                formInstructions <- atomically . readTVar . getFormInstructions $ crawlerState
                case selectFormOptions formInstructions forms of

                    Just formRequest -> do

                        let moreCookies = responseCookies ++ cookiesSent
                        _ {- formResponse -} <- getWithRedirects (getManager crawlerState) moreCookies formRequest
                        processResponse undefined undefined undefined nextUrl moreCookies

                    Nothing -> storeResponse nextHrefs hrefErrors

        where
        storeResponse nextHrefs hrefErrors = do
            included <- atomically $ checkAgainstIncludePatterns crawlerState (head redirects)
            when included $ do
                atomically $ shareCookies (responseCookies \\ cookiesSent)
                mapM_ (atomically . writeQueue (getLogQueue crawlerState)) hrefErrors
                atomically $ successfulDownload nextUrl redirects bodyData
                mapM_ (processNextUrl crawlerState) nextHrefs  

    shareCookies :: [Cookie] -> STM ()
    shareCookies responseCookiesToshare =
        let cookiesToAdd = filter shareCookie responseCookiesToshare
        in modifyTVar' (getCookieList crawlerState) (\x -> cookiesToAdd ++ x)

    failedDownload :: CanonicalUrl -> STM ()
    failedDownload attemptedUrl = do
        S.delete attemptedUrl (getUrlsInProgress crawlerState)
        M.insert "Couldn't get data!" attemptedUrl (getUrlsFailed crawlerState)
        writeQueue (getLogQueue crawlerState) (LoggableError attemptedUrl "placeholder error msg")

    successfulDownload :: CanonicalUrl -> [CanonicalUrl] -> ByteString -> STM ()
    successfulDownload attemptedUrl redirects bodyData = do
        S.delete attemptedUrl (getUrlsInProgress crawlerState)
        mapM_ (\u -> S.insert u (getUrlsCompleted crawlerState)) redirects
        let crawledDocument = CrawledDocument
                            { getRedirectChain = redirects
                            , getContent = bodyData
                            , getThreadId = threadId
                            }
        writeQueue (getStoreQueue crawlerState) crawledDocument

    resetThreadClock nextUrl = getCurrentTime >>= \c -> atomically . M.insert (c, nextUrl) threadId . getThreadClocks $ workers
        
processNextUrl :: Crawler -> CanonicalUrl -> IO Success
processNextUrl crawlerState url = do
    isAcceptable <- atomically $ checkAgainstIncludePatterns crawlerState url
    if isAcceptable
        then
            atomically $ do
                completed <- S.lookup url (getUrlsCompleted crawlerState)
                inProgress <- S.lookup url (getUrlsInProgress crawlerState)
                failed <- isJust <$> M.lookup url (getUrlsFailed crawlerState)

                case (completed, inProgress, failed) of
                    (True,_,_) -> return $ Failure "Already completed URL"
                    (_,True,_) -> return $ Failure "URL already in progress"
                    (_,_,True) -> return $ Failure "URL previously failed"
                    _          -> do
                        S.insert url (getUrlsInProgress crawlerState)
                        PQ.writeQueue url (getUrlQueue crawlerState)
        else return $ Failure "URL wasn't acceptable"

checkAgainstIncludePatterns :: Crawler -> CanonicalUrl -> STM Bool
checkAgainstIncludePatterns crawlerState (CanonicalUrl url) =
    any (`isInfixOf` url) <$> setAsList (getUrlPatterns crawlerState)
