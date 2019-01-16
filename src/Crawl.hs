{-# LANGUAGE OverloadedStrings #-}

module Crawl where

import CountedQueue.Bounded   as Bounded

import CountedQueue
import Communication                    (CrawlerStatus(..))
import Directions
import qualified PoliteQueue as PQ
import Errors                           (LogFunction)
import Fetch
import Forms                            (emptyFormActions, selectFormOptions)
import Parse                            (parsePage, findPageRedirect)
import Settings
import Shared
import Types

import Control.Concurrent               (ThreadId, myThreadId)
import Control.Concurrent.STM           (STM, atomically, writeTVar, readTVar, readTVarIO, modifyTVar', newTVarIO)
import Control.Monad                    (replicateM_, when, void)
import Control.Monad.Trans.Resource     (runResourceT)
import Data.ByteString.Char8      as C8 (ByteString, concat, isInfixOf)
import Data.List                        ((\\))
import Data.Maybe                       (isJust)
import Data.Time
import Network.HTTP.Conduit             (Cookie, newManager, tlsManagerSettings, mkManagerSettings)
import Network.Connection               (TLSSettings (TLSSettingsSimple))
import qualified ListT             as L
import qualified StmContainers.Set as S
import qualified StmContainers.Map as M
import Text.HTML.TagSoup.Fast   (parseTags)

createCrawler :: IO (Crawler BoundedCountedQueue)
createCrawler = do
    crawlerStatus <- newTVarIO RunningStatus
    urlQueue <- PQ.newIO
    storeQueue <- Bounded.newQueueIO 32
    numStored <- newTVarIO 0
    loggingQueue <- Bounded.newQueueIO 128
    manager <- newManager (if ignoreBadHttpsCerts
                               then mkManagerSettings (TLSSettingsSimple True False False) Nothing
                               else tlsManagerSettings)
    cookieList <- newTVarIO []
    urlPatterns <- S.newIO
    urlsInProgress <- S.newIO
    urlsCompleted <- S.newIO
    urlsFailed <- M.newIO
    crawlerSettings <- createCrawlerSettings
    return $ Crawler crawlerStatus urlQueue storeQueue numStored loggingQueue manager cookieList urlPatterns urlsInProgress urlsCompleted urlsFailed crawlerSettings

createCrawlerSettings :: IO CrawlerSettings
createCrawlerSettings = do
    crawlOutput <- newTVarIO Nothing
    formInstructions <- newTVarIO emptyFormActions
    hrefDirections <- newTVarIO emptyHrefDirections
    proxySettings <- newTVarIO Nothing
    crawlLimit <- newTVarIO Nothing
    return $ CrawlerSettings crawlOutput formInstructions hrefDirections proxySettings crawlLimit

setNumCrawlers :: CountedQueue bq => Crawler bq -> Workers -> LogFunction -> Int -> IO ()
setNumCrawlers crawlerState workers logFunc desiredNum = do

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
                            . S.listT
                            . getCrawlerThreads
                            $ workers
                        return 0
    replicateM_ threadsToAdd addCrawler

    where
    addCrawler :: IO ()
    addCrawler = forkIO_ $ do
        threadId <- myThreadId
        atomically $ S.insert threadId (getCrawlerThreads workers)
        crawlUrls workers crawlerState threadId logFunc

    getActiveCrawlerCount :: STM Int
    getActiveCrawlerCount = (-) <$> sizeOfSet (getCrawlerThreads workers) <*> sizeOfSet (getCrawlerThreadsToStop workers)

crawlUrls :: CountedQueue bq => Workers -> Crawler bq -> ThreadId -> LogFunction -> IO ()
crawlUrls workers crawlerState threadId logFunc =

    whileActive threadId (getCrawlerThreads workers) (getCrawlerThreadsToStop workers) $ do

        putStrLn $ "Thread " ++ show threadId ++ " running"

        status <- readTVarIO . getCrawlerStatus $ crawlerState

        when (status /= RunningStatus) $
            atomically $ S.insert threadId (getCrawlerThreadsToStop workers)

        when (status == RunningStatus) $ do

            nextUrl@(CanonicalUrl bs) <- atomically $ PQ.readQueue threadId (getUrlQueue crawlerState)

            when stepMode $ do
                logFunc . GeneralMessage . C8.concat $ ["(Step Mode)... ", bs, "\nEnter to continue"]
                void getLine

            cookiesToSend <- readTVarIO . getCookieList $ crawlerState

            resetThreadClock nextUrl

            downloadResult <- runResourceT $ fetch (getManager crawlerState) (getCrawlerSettings crawlerState) cookiesToSend (GetRequest nextUrl)

            processResult downloadResult nextUrl cookiesToSend

    where
    processResult :: DownloadResult -> CanonicalUrl -> [Cookie] -> IO ()
    processResult (DownloadResult bodyData responseCookies redirects) nextUrl cookiesSent = do --todo rename nextUrl - attemptedUrl?

        let parsedTags = parseTags bodyData

        --TODO - great place for some refactoring
        mDirection <- findDirection (head redirects) bodyData (getCrawlerSettings crawlerState) 
        
        case mDirection of
            (Just direction) -> do
                let moreCookies = responseCookies ++ cookiesSent
                directionResponse <- runResourceT $ fetch (getManager crawlerState) (getCrawlerSettings crawlerState) moreCookies (GetRequest direction)
                processResult directionResponse nextUrl moreCookies
            Nothing ->
                --Give meta refresh a chance to fire
                case findPageRedirect nextUrl parsedTags of --TODO - should this be head redirects
                    Just metaRefreshUrl -> do
                        {- Chance of crawler trap here. Perhaps we should 
                            check that metaRefreshUrl hasn't already been visited 
                        This isn't the best check yet.  Not sure what the best procedure is -}

                        eNotDone <- atomically $ checkNotDone crawlerState metaRefreshUrl

                        when (eNotDone == Right ()) $ do
                            let moreCookies = responseCookies ++ cookiesSent
                            metaRefreshResponse <- runResourceT $ fetch (getManager crawlerState) (getCrawlerSettings crawlerState) moreCookies (GetRequest metaRefreshUrl)
                            processResult metaRefreshResponse nextUrl moreCookies

                    Nothing -> do
                        let (hrefErrors, nextHrefs, forms) = parsePage redirects parsedTags
                        formInstructions <- readTVarIO . getFormInstructions . getCrawlerSettings $ crawlerState
                        case selectFormOptions formInstructions forms of
                            Just formRequest -> do
                                let moreCookies = responseCookies ++ cookiesSent
                                formResponse <- runResourceT $ fetch (getManager crawlerState) (getCrawlerSettings crawlerState) moreCookies formRequest
                                processResult formResponse nextUrl moreCookies
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

    successfulDownload :: CanonicalUrl -> [CanonicalUrl] -> ByteString -> STM ()
    successfulDownload attemptedUrl redirects bodyData = do
        numStored <- readTVar (getNumStored crawlerState)
        mCrawlLimit <- readTVar . getCrawlLimit . getCrawlerSettings $ crawlerState
        S.delete attemptedUrl (getUrlsInProgress crawlerState)
        mapM_ (\u -> S.insert u (getUrlsCompleted crawlerState)) redirects
        let crawledDocument = CrawledDocument
                            { getRedirectChain = redirects
                            , getContent = bodyData
                            , getThreadId = threadId
                            }

        when (maybe True (\cl -> numStored < cl) mCrawlLimit) $ do
            writeQueue (getStoreQueue crawlerState) crawledDocument
            writeTVar (getNumStored crawlerState) (numStored+1)

    resetThreadClock nextUrl = getCurrentTime >>= \c -> atomically . M.insert (c, nextUrl) threadId . getThreadClocks $ workers

processNextUrl :: Crawler bq -> CanonicalUrl -> IO (Either ByteString ())
processNextUrl crawler url = do
    isAcceptable <- atomically $ checkAgainstIncludePatterns crawler url
    if isAcceptable
        then atomically $ insertIfNotDone crawler url
        else return . Left $ "URL wasn't acceptable"

insertIfNotDone :: Crawler bq -> CanonicalUrl -> STM (Either ByteString ())
insertIfNotDone crawler url = do
    eNotDone <- checkNotDone crawler url
    case eNotDone of
        Left l -> return $ Left l
        Right () -> do
            S.insert url (getUrlsInProgress crawler)
            PQ.writeQueue url (getUrlQueue crawler)

checkNotDone :: Crawler bq -> CanonicalUrl -> STM (Either ByteString ())
checkNotDone crawler url = do
    completed <- S.lookup url (getUrlsCompleted crawler)
    inProgress <- S.lookup url (getUrlsInProgress crawler)
    failed <- isJust <$> M.lookup url (getUrlsFailed crawler)
    case (completed, inProgress, failed) of
        (True,_,_) -> return $ Left "Already completed URL"
        (_,True,_) -> return $ Left "URL already in progress"
        (_,_,True) -> return $ Left "URL previously failed"
        _          -> return $ Right ()

checkAgainstIncludePatterns :: Crawler bq -> CanonicalUrl -> STM Bool
checkAgainstIncludePatterns crawlerState (CanonicalUrl url) =
    any (`isInfixOf` url) <$> setAsList (getUrlPatterns crawlerState)
    where
    setAsList :: S.Set a -> STM [a]
    setAsList = L.toList . S.listT
