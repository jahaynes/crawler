module Crawl where

import Fetch
import Urls
import Workers
import Types

import Control.Applicative              ((<$>), (<*>))
import Control.Concurrent               (forkIO)
import Control.Concurrent.STM.TBQueue
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM           (atomically)
import Control.Monad                    (when, unless, replicateM_)
import Data.ByteString                  (ByteString)
import Data.Maybe                       (isJust)
import GHC.Conc                         (STM, myThreadId, ThreadId)
import qualified ListT             as L
import Network.HTTP.Conduit             (newManager, tlsManagerSettings)
import qualified STMContainers.Set as S
import qualified STMContainers.Map as M


setNumCrawlers :: CrawlerState -> Workers -> Int -> IO ()
setNumCrawlers crawlerState workers desiredNum = do

    threadsToAdd <- atomically $ do
        currentNumCrawlers <- getActiveCrawlerCount
        case desiredNum - currentNumCrawlers of
            0 -> return 0
            threadDelta -> do
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

        where
        sizeOfSet :: S.Set a -> STM Int
        sizeOfSet = L.fold (\a _ -> return (a + 1)) 0 . S.stream

    takeSet :: Int -> S.Set a -> STM [a]
    takeSet n = L.toList . L.take n . S.stream

crawlUrls :: Workers -> CrawlerState -> ThreadId -> IO ()
crawlUrls workers crawlerState threadId = do

    man <- newManager tlsManagerSettings

    whileActive $ do

        nextUrl <- atomically $ readTQueue (getUrlQueue crawlerState)

        (mBodyData, redirects) <- getWithRedirects man nextUrl

        case mBodyData of
            Nothing -> atomically $ failedDownload nextUrl
            Just bodyData -> atomically $ successfulDownload nextUrl redirects bodyData

    where
    whileActive :: IO () -> IO ()
    whileActive a = go
        where
        go = do
            toStop <- atomically $ do
                toStop <- S.lookup threadId (getCrawlerThreadsToStop workers)
                when toStop $ do
                    S.delete threadId (getCrawlerThreadsToStop workers)
                    S.delete threadId (getCrawlerThreads workers)
                return toStop
            unless toStop $ a >> go

    failedDownload :: CanonicalUrl -> STM ()
    failedDownload attemptedUrl = do
        S.delete attemptedUrl (getUrlsInProgress crawlerState)
        M.insert "Couldn't get data!" attemptedUrl (getUrlsFailed crawlerState)

    successfulDownload :: CanonicalUrl -> [CanonicalUrl] -> ByteString -> STM ()
    successfulDownload attemptedUrl redirects bodyData = do
        S.delete attemptedUrl (getUrlsInProgress crawlerState)
        mapM_ (\u -> S.insert u (getUrlsCompleted crawlerState)) redirects
        writeTQueue (getParseQueue crawlerState) (redirects, bodyData)
        writeTBQueue (getStoreQueue crawlerState) (redirects, bodyData)

processNextUrl :: CrawlerState -> CanonicalUrl -> IO ()
processNextUrl crawlerState url =
    when (isAcceptable url) $
        atomically $ do
            completed <- S.lookup url (getUrlsCompleted crawlerState)
            inProgress <- S.lookup url (getUrlsInProgress crawlerState)
            failed <- isJust <$> M.lookup url (getUrlsFailed crawlerState)
            unless (completed || inProgress || failed) $ do
                S.insert url (getUrlsInProgress crawlerState)
                writeTQueue (getUrlQueue crawlerState) url

forkIO_ :: IO () -> IO ()
forkIO_ a = do
    _ <- forkIO a
    return ()