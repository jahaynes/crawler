{-# LANGUAGE OverloadedStrings #-}

module Main where

import Fetch
import Parse
import Urls
import Settings
import Types
import Output
import Health

import Control.Applicative ((<$>))
import Control.Concurrent               (forkIO, threadDelay)
import Control.Concurrent.STM.TBQueue
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM           (atomically)
import Control.Monad                    (forever, when, unless)

import qualified Data.ByteString.Char8 as C8
import Data.Maybe                       (catMaybes, isJust)
import GHC.Conc                         (STM)
import Network.HTTP.Conduit             (newManager, tlsManagerSettings)

import qualified STMContainers.Set as S
import qualified STMContainers.Map as M

import System.Environment               (getArgs)

createCrawlerState :: IO CrawlerState
createCrawlerState = do
    urlQueue <- newTQueueIO
    parseQueue <- newTQueueIO
    storeQueue <- newTBQueueIO 32
    urlsInProgress <- S.newIO
    urlsCompleted <- S.newIO
    urlsFailed <- M.newIO
    return $ CrawlerState urlQueue parseQueue storeQueue urlsInProgress urlsCompleted urlsFailed

crawlNextUrl :: CrawlerState -> IO ()    
crawlNextUrl crawlerState = newManager tlsManagerSettings >>= \man -> 
    forever $ do

        nextUrl <- atomically $ readTQueue (getUrlQueue crawlerState)

        putStrLn $ "Grabbed: " ++ show nextUrl
        (mBodyData, redirects) <- getWithRedirects man nextUrl

        case mBodyData of
            Nothing -> atomically $ failedDownload nextUrl
            Just bodyData -> atomically $ successfulDownload nextUrl redirects bodyData
            
    where
    failedDownload :: CanonicalUrl -> STM ()
    failedDownload attemptedUrl = do
        S.delete attemptedUrl (getUrlsInProgress crawlerState)
        M.insert "Couldn't get data!" attemptedUrl (getUrlsFailed crawlerState)

    successfulDownload :: CanonicalUrl -> [CanonicalUrl] -> C8.ByteString -> STM ()
    successfulDownload attemptedUrl redirects bodyData = do
        S.delete attemptedUrl (getUrlsInProgress crawlerState)
        mapM_ (\u -> S.insert u (getUrlsCompleted crawlerState)) redirects
        writeTQueue (getParseQueue crawlerState) (redirects, bodyData)
        writeTBQueue (getStoreQueue crawlerState) (redirects, bodyData)

parsePages :: CrawlerState -> IO ()
parsePages crawlerState = forever $ do

    (redirects, dat) <- atomically $ readTQueue (getParseQueue crawlerState)
    let referenceUrl = head redirects 
        rawHrefs = getRawHrefs referenceUrl dat
        nextHrefs = catMaybes rawHrefs
    when (length rawHrefs /= length nextHrefs) (putStrLn "Warning, not all Hrefs were good!")
    mapM_ (processNextUrl crawlerState) nextHrefs

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

main :: IO ()
main = do

    crawlerState <- createCrawlerState
    mapM_ (\_ -> forkIO $ crawlNextUrl crawlerState) threadsPerJob
    mapM_ (\_ -> forkIO $ parsePages crawlerState) threadsPerJob
    forkIO $ storePages crawlerState

    getArgs >>=
        mapM_ (\a ->
            case canonicaliseString a of
                Just x -> processNextUrl crawlerState x
                Nothing -> putStrLn $ "Could not parse URL: " ++ a)
                         
    forever $ do
        threadDelay 1000000
        putStrLn "."

