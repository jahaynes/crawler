{-# LANGUAGE OverloadedStrings #-}

module Crawl where

import CountedQueue
import qualified PoliteQueue as PQ
import Fetch
import Forms                            (selectFormOptions)
import Parse (parsePage, findPageRedirect)
import Settings
import Shared
import Types

import Control.Applicative              ((<$>), (<*>))
import Control.Concurrent               (ThreadId, myThreadId)
import Control.Concurrent.STM           (STM, atomically, readTVar, modifyTVar')
import Control.Monad                    (replicateM_, when)
import Data.ByteString.Char8            (ByteString, isInfixOf)
import Data.List                        ((\\))
import Data.Maybe                       (isJust)
import Data.Time
import Network.HTTP.Conduit             (Cookie)
import qualified ListT             as L
import qualified STMContainers.Set as S
import qualified STMContainers.Map as M
import Text.HTML.TagSoup.Fast   (parseTags)

setNumCrawlers :: CrawlerState -> Workers -> Int -> IO ()
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

crawlUrls :: Workers -> CrawlerState -> ThreadId -> IO ()
crawlUrls workers crawlerState threadId = do

    whileActive threadId (getCrawlerThreads workers) (getCrawlerThreadsToStop workers) $ do

        nextUrl <- atomically $ PQ.readQueue threadId (getUrlQueue crawlerState)
        cookiesToSend <- atomically . readTVar . getCookieList $ crawlerState

        resetThreadClock nextUrl

        (mBodyData, redirects) <- getWithRedirects (getManager crawlerState) cookiesToSend (GetRequest nextUrl)

        processResponse (mBodyData, redirects) nextUrl cookiesToSend

    where
    processResponse :: (Either String (ByteString, [Cookie]), [CanonicalUrl]) -> CanonicalUrl -> [Cookie] -> IO ()
    processResponse (mBodyData, redirects) nextUrl cookiesSent =
        case mBodyData of
            Left err -> do
                putStrLn $ "Failed to download (thread " ++ show threadId ++ ")"
                print err
                atomically $ failedDownload nextUrl
            Right (bodyData, responseCookies) -> do

                let parsedTags = parseTags bodyData

                --Give meta refresh a chance to fire
                case findPageRedirect parsedTags of
                    Just metaRefreshUrl -> do
                        {- Chance of crawler trap here. Perhaps we should 
                           check that metaRefreshUrl hasn't already been visited -}
                        let moreCookies = responseCookies ++ cookiesSent
                        formResponse <- getWithRedirects (getManager crawlerState) moreCookies (GetRequest metaRefreshUrl)
                        processResponse formResponse nextUrl moreCookies

                    Nothing ->
                        let (hrefErrors, nextHrefs, forms) = parsePage redirects parsedTags
                        in
                        case selectFormOptions (getFormInstructions crawlerState) forms of

                            Just formRequest -> do

                                let moreCookies = responseCookies ++ cookiesSent
                                formResponse <- getWithRedirects (getManager crawlerState) moreCookies formRequest
                                processResponse formResponse nextUrl moreCookies

                            Nothing -> storeResponse bodyData responseCookies nextHrefs hrefErrors

        where
        storeResponse bodyData responseCookies nextHrefs hrefErrors = do
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
        writeQueue (getStoreQueue crawlerState) (threadId, redirects, bodyData)

    resetThreadClock nextUrl = getCurrentTime >>= \c -> atomically . M.insert (c, nextUrl) threadId . getThreadClocks $ workers
        
processNextUrl :: CrawlerState -> CanonicalUrl -> IO Success
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

checkAgainstIncludePatterns :: CrawlerState -> CanonicalUrl -> STM Bool
checkAgainstIncludePatterns crawlerState (CanonicalUrl url) =
    any (`isInfixOf` url) <$> setAsList (getUrlPatterns crawlerState)

