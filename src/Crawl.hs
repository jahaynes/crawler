{-# LANGUAGE OverloadedStrings #-}

module Crawl where

import CountedQueue
import Fetch
import Urls
import Workers
import Parse (parsePage)
import Shared
import Types

import Control.Applicative              ((<$>), (<*>))
import Control.Concurrent               (ThreadId, myThreadId)
import Control.Concurrent.STM           (STM, atomically)
import Control.Monad                    (replicateM_)
import Data.ByteString.Char8            (ByteString, isInfixOf)
import Data.Maybe                       (isJust)
import Data.Time
import Network.HTTP.Conduit             (newManager, tlsManagerSettings)
import qualified STMContainers.Set as S
import qualified STMContainers.Map as M

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
                        threadsToStop <- takeSet (-threadDelta) (getCrawlerThreads workers)
                        mapM_ (\t -> S.insert t (getCrawlerThreadsToStop workers)) threadsToStop
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

    man <- newManager tlsManagerSettings

    whileActive threadId (getCrawlerThreads workers) (getCrawlerThreadsToStop workers) $ do

        nextUrl <- atomically $ readQueue (getUrlQueue crawlerState)

        resetThreadClock nextUrl

        (mBodyData, redirects) <- getWithRedirects man nextUrl

        case mBodyData of
            Left err -> do
                putStrLn $ "Failed to download (thread " ++ show threadId ++ ")"
                print err
                atomically $ failedDownload nextUrl
            Right bodyData -> do

                {- Parse the page mid-crawl so we can change course and work
                   with forms if need be -}

                let (hrefErrors, nextHrefs, forms) = parsePage redirects bodyData

                mapM_ print forms

                mapM_ (atomically . writeQueue (getLogQueue crawlerState)) hrefErrors
                atomically $ successfulDownload nextUrl redirects bodyData
                mapM_ (processNextUrl crawlerState) nextHrefs

    where
    failedDownload :: CanonicalUrl -> STM ()
    failedDownload attemptedUrl = do
        S.delete attemptedUrl (getUrlsInProgress crawlerState)
        M.insert "Couldn't get data!" attemptedUrl (getUrlsFailed crawlerState)
        writeQueue (getLogQueue crawlerState) (LoggableError attemptedUrl "placeholder error msg")

    successfulDownload :: CanonicalUrl -> [CanonicalUrl] -> ByteString -> STM ()
    successfulDownload attemptedUrl redirects bodyData = do
        S.delete attemptedUrl (getUrlsInProgress crawlerState)
        mapM_ (\u -> S.insert u (getUrlsCompleted crawlerState)) redirects
        writeQueue (getStoreQueue crawlerState) (redirects, bodyData)

    resetThreadClock nextUrl = getCurrentTime >>= \c -> atomically . M.insert (c, nextUrl) threadId . getThreadClocks $ workers
        
processNextUrl :: CrawlerState -> CanonicalUrl -> IO Accepted
processNextUrl crawlerState url@(CanonicalUrl url') = do
    isAcceptable <- atomically checkAcceptable
    if isAcceptable
        then do
            accepted <- atomically $ do
                completed <- S.lookup url (getUrlsCompleted crawlerState)
                inProgress <- S.lookup url (getUrlsInProgress crawlerState)
                failed <- isJust <$> M.lookup url (getUrlsFailed crawlerState)

                case (completed, inProgress, failed) of
                    (True,_,_) -> return $ NotAccepted "Already completed URL"
                    (_,True,_) -> return $ NotAccepted "URL already in progress"
                    (_,_,True) -> return $ NotAccepted "URL previously failed"
                    _          -> do
                        S.insert url (getUrlsInProgress crawlerState)
                        writeQueue (getUrlQueue crawlerState) url
                        return Accepted
            return accepted
        else return $ NotAccepted "URL wasn't acceptable"

    where
    checkAcceptable :: STM Bool
    checkAcceptable = any (`isInfixOf` url') <$> setAsList (getUrlPatterns crawlerState)

