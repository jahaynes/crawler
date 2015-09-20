{-# LANGUAGE OverloadedStrings #-}

module Main where

import Fetch
import Parse
import Urls
import Settings
import Types
import Output
import Errors
import Workers

import Control.Applicative ((<$>))
import Control.Concurrent               (threadDelay)
import Control.Concurrent.STM.TBQueue
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM           (atomically)
import Control.Monad                    (forever, when, unless)

import qualified Data.ByteString.Char8 as C8
import Data.Either                      (partitionEithers)
import Data.Maybe                       (isJust)
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
    loggingQueue <- newTBQueueIO 128
    urlsInProgress <- S.newIO
    urlsCompleted <- S.newIO
    urlsFailed <- M.newIO
    return $ CrawlerState urlQueue parseQueue storeQueue loggingQueue urlsInProgress urlsCompleted urlsFailed

crawlNextUrl :: CrawlerState -> IO ()    
crawlNextUrl crawlerState = newManager tlsManagerSettings >>= \man -> 
    forever $ do

        nextUrl <- atomically $ readTQueue (getUrlQueue crawlerState)

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
        (hrefErrors, nextHrefs) = partitionEithers . getRawHrefs referenceUrl $ dat
    mapM_ (atomically . writeTBQueue (getLogQueue crawlerState)) hrefErrors
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

    initialiseHealth >>= \health -> do

        mapM_ (\n -> forkHealth health ("crawler_" ++ show n) $ crawlNextUrl crawlerState) threadsPerJob
        
        mapM_ (\n -> forkHealth health ("parser_" ++ show n) $ parsePages crawlerState) threadsPerJob
        
        forkHealth health "storage" $ storePages crawlerState

        forkHealth health "logging" $ logErrors crawlerState

    getArgs >>=
        mapM_ (\a ->
            case canonicaliseString a of
                Just x -> processNextUrl crawlerState x
                Nothing -> putStrLn $ "Could not parse URL: " ++ a)
                         
    forever $ do
        threadDelay 1000000
        putStrLn "."

