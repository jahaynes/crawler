{-# LANGUAGE OverloadedStrings #-}

module Crawl where

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
import Control.Concurrent.STM           (STM, atomically, readTVar, modifyTVar', newTVarIO)
import Control.Monad                    (replicateM_, when, void)
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
    crawlOutput <- newTVarIO Nothing
    formInstructions <- newTVarIO emptyFormActions
    hrefDirections <- newTVarIO emptyHrefDirections
    proxySettings <- newTVarIO Nothing
    return $ CrawlerSettings crawlOutput formInstructions hrefDirections proxySettings

setNumCrawlers :: Crawler -> Workers -> LogFunction -> Int -> IO ()
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
        crawlUrls workers crawlerState threadId logFunc

    getActiveCrawlerCount :: STM Int
    getActiveCrawlerCount = (-) <$> sizeOfSet (getCrawlerThreads workers) <*> sizeOfSet (getCrawlerThreadsToStop workers)

crawlUrls :: Workers -> Crawler -> ThreadId -> LogFunction -> IO ()
crawlUrls workers crawlerState threadId logFunc =

    whileActive threadId (getCrawlerThreads workers) (getCrawlerThreadsToStop workers) $ do

        nextUrl@(CanonicalUrl bs) <- atomically $ PQ.readQueue threadId (getUrlQueue crawlerState)

        when stepMode $ do
            logFunc . GeneralMessage . C8.concat $ ["(Step Mode)... ", bs, "\nEnter to continue"]
            void getLine

        cookiesToSend <- atomically . readTVar . getCookieList $ crawlerState

        resetThreadClock nextUrl

        eDownloadResult <- runWebIO $ fetch (getManager crawlerState) (getCrawlerSettings crawlerState) cookiesToSend (GetRequest nextUrl)

        case eDownloadResult of
            Left err -> do
                let errMsg = C8.pack . Prelude.concat $ [ "Failed to download (thread ", show threadId, ")\n", err]
                logFunc $ GeneralMessage errMsg
                atomically $ failedDownload nextUrl
            Right downloadResult -> processResult downloadResult nextUrl cookiesToSend

    where
    processResult :: DownloadResult -> CanonicalUrl -> [Cookie] -> IO ()
    processResult (DownloadResult bodyData responseCookies redirects) nextUrl cookiesSent = do --todo rename nextUrl - attemptedUrl?

        let parsedTags = parseTags bodyData

        --TODO - great place for some refactoring
        mDirection <- findDirection (head redirects) bodyData (getCrawlerSettings crawlerState) 
        
        case mDirection of
            (Just direction) -> do
                let moreCookies = responseCookies ++ cookiesSent
                eDirectionResponse <- runWebIO $ fetch (getManager crawlerState) (getCrawlerSettings crawlerState) moreCookies (GetRequest direction)
                case eDirectionResponse of
                    Left e -> atomically . writeQueue (getLogQueue crawlerState) $ CrawlWarning nextUrl (C8.concat ["Failed to process meta refresh: ", C8.pack (show e)])
                    Right directionResponse -> processResult directionResponse nextUrl moreCookies
            Nothing -> do
                --Give meta refresh a chance to fire
                case findPageRedirect nextUrl parsedTags of --TODO - should this be head redirects
                    Just metaRefreshUrl -> do
                        {- Chance of crawler trap here. Perhaps we should 
                            check that metaRefreshUrl hasn't already been visited 
                        This isn't the best check yet.  Not sure what the best procedure is -}

                        eNotDone <- atomically $ checkNotDone crawlerState metaRefreshUrl

                        when (eNotDone == Right ()) $ do
                            let moreCookies = responseCookies ++ cookiesSent
                            eMetaRefreshResponse <- runWebIO $ fetch (getManager crawlerState) (getCrawlerSettings crawlerState) moreCookies (GetRequest metaRefreshUrl)
                            case eMetaRefreshResponse of
                                Left e -> atomically . writeQueue (getLogQueue crawlerState) $ CrawlWarning nextUrl (C8.concat ["Failed to process meta refresh: ", C8.pack (show e)])
                                Right metaRefreshResponse -> processResult metaRefreshResponse nextUrl moreCookies

                    Nothing -> do
                        let (hrefErrors, nextHrefs, forms) = parsePage redirects parsedTags
                        formInstructions <- atomically . readTVar . getFormInstructions . getCrawlerSettings $ crawlerState
                        case selectFormOptions formInstructions forms of
                            Just formRequest -> do
                                let moreCookies = responseCookies ++ cookiesSent
                                eFormResponse <- runWebIO $ fetch (getManager crawlerState) (getCrawlerSettings crawlerState) moreCookies formRequest
                                case eFormResponse of 
                                    Left e -> atomically . writeQueue (getLogQueue crawlerState) $ CrawlWarning nextUrl (C8.concat ["Failed to process form: ", C8.pack (show e)])
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
        writeQueue (getLogQueue crawlerState) (CrawlError attemptedUrl (C8.pack errMsg))

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

processNextUrl :: Crawler -> CanonicalUrl -> IO (Either ByteString ())
processNextUrl crawler url = do
    isAcceptable <- atomically $ checkAgainstIncludePatterns crawler url
    if isAcceptable
        then atomically $ insertIfNotDone crawler url
        else return . Left $ "URL wasn't acceptable"

insertIfNotDone :: Crawler -> CanonicalUrl -> STM (Either ByteString ())
insertIfNotDone crawler url = do
    eNotDone <- checkNotDone crawler url
    case eNotDone of
        Left l -> return $ Left l
        Right () -> do
            S.insert url (getUrlsInProgress crawler)
            PQ.writeQueue url (getUrlQueue crawler)

checkNotDone :: Crawler -> CanonicalUrl -> STM (Either ByteString ())
checkNotDone crawler url = do
    completed <- S.lookup url (getUrlsCompleted crawler)
    inProgress <- S.lookup url (getUrlsInProgress crawler)
    failed <- isJust <$> M.lookup url (getUrlsFailed crawler)
    case (completed, inProgress, failed) of
        (True,_,_) -> return $ Left "Already completed URL"
        (_,True,_) -> return $ Left "URL already in progress"
        (_,_,True) -> return $ Left "URL previously failed"
        _          -> return $ Right ()

checkAgainstIncludePatterns :: Crawler -> CanonicalUrl -> STM Bool
checkAgainstIncludePatterns crawlerState (CanonicalUrl url) =
    any (`isInfixOf` url) <$> setAsList (getUrlPatterns crawlerState)
