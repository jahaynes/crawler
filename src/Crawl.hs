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

import Control.Concurrent               (ThreadId, myThreadId)
import Control.Concurrent.STM           (STM, atomically, readTVar, modifyTVar', newTVarIO)
import Control.Monad                    (replicateM_, when)
import Data.ByteString.Char8      as C8 (ByteString, concat, pack, isInfixOf)
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
    crawlerSettings <- createCrawlerSettings
    return $ Crawler crawlerStatus urlQueue storeQueue loggingQueue manager cookieList urlPatterns urlsInProgress urlsCompleted urlsFailed crawlerSettings

createCrawlerSettings :: IO CrawlerSettings
createCrawlerSettings = do
    formInstructions <- newTVarIO emptyFormActions
    proxySettings <- newTVarIO Nothing
    return $ CrawlerSettings formInstructions proxySettings

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

        when stepMode $ do
            putStrLn $ "(Step Mode)... " ++ show nextUrl
            putStrLn "Enter to continue"
            const () <$> getLine

        cookiesToSend <- atomically . readTVar . getCookieList $ crawlerState

        resetThreadClock nextUrl

        eDownloadResult <- runWebIO $ fetch (getManager crawlerState) (getCrawlerSettings crawlerState) cookiesToSend (GetRequest nextUrl)

        case eDownloadResult of
            Left err -> do
                putStrLn $ "Failed to download (thread " ++ show threadId ++ ")"
                print err
                atomically $ failedDownload nextUrl
            Right downloadResult -> processResult downloadResult nextUrl cookiesToSend

    where
    processResult :: DownloadResult -> CanonicalUrl -> [Cookie] -> IO ()
    processResult (DownloadResult bodyData responseCookies redirects) nextUrl cookiesSent = do

        let parsedTags = parseTags bodyData

        --Give meta refresh a chance to fire
        case findPageRedirect nextUrl parsedTags of
            Just metaRefreshUrl -> do
                {- Chance of crawler trap here. Perhaps we should 
                    check that metaRefreshUrl hasn't already been visited -}
                let moreCookies = responseCookies ++ cookiesSent
                eMetaRefreshResponse <- runWebIO $ fetch (getManager crawlerState) (getCrawlerSettings crawlerState) moreCookies (GetRequest metaRefreshUrl)
                case eMetaRefreshResponse of
                    Left e -> atomically . writeQueue (getLogQueue crawlerState) $ LoggableWarning nextUrl (C8.concat ["Failed to process meta refresh: ", C8.pack (show e)])
                    Right metaRefreshResponse -> processResult metaRefreshResponse nextUrl moreCookies

            Nothing -> do
                let (hrefErrors, nextHrefs, forms) = parsePage redirects parsedTags
                formInstructions <- atomically . readTVar . getFormInstructions . getCrawlerSettings $ crawlerState
                case selectFormOptions formInstructions forms of
                    Just formRequest -> do
                        let moreCookies = responseCookies ++ cookiesSent
                        eFormResponse <- runWebIO $ fetch (getManager crawlerState) (getCrawlerSettings crawlerState) moreCookies formRequest
                        case eFormResponse of 
                            Left e -> atomically . writeQueue (getLogQueue crawlerState) $ LoggableWarning nextUrl (C8.concat ["Failed to process form: ", C8.pack (show e)])
                            Right formResponse -> processResult formResponse nextUrl moreCookies
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
        let errMsg = "Couldn't get data for " ++ show attemptedUrl
        S.delete attemptedUrl (getUrlsInProgress crawlerState)
        M.insert errMsg attemptedUrl (getUrlsFailed crawlerState)
        writeQueue (getLogQueue crawlerState) (LoggableError attemptedUrl (C8.pack errMsg))

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
