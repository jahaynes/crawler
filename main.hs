{-# LANGUAGE OverloadedStrings #-}

module Main where

import Fetch
import Parse
import Urls
import Settings

import Control.Applicative ((<$>))
import Control.Concurrent               (forkIO, threadDelay)
import Control.Concurrent.STM.TBQueue
import Control.Concurrent.STM           (atomically)
import Control.Monad                    (forever, when, unless)

import qualified Data.ByteString.Char8 as C8
import Data.Maybe                       (catMaybes, isJust)

import Network.HTTP.Conduit             (newManager, tlsManagerSettings)

import qualified STMContainers.Set as S
import qualified STMContainers.Map as M

import System.Environment               (getArgs)

type Crawled = ([CanonicalUrl], C8.ByteString)

data CrawlerState = CrawlerState {
    getUrlQueue :: TBQueue CanonicalUrl,  
    getCrawledQueue :: TBQueue Crawled,
    getUrlsInProgress :: S.Set CanonicalUrl,
    getUrlsCompleted :: S.Set CanonicalUrl,
    getUrlsFailed :: M.Map CanonicalUrl String
}

createCrawlerState :: IO CrawlerState
createCrawlerState = do
    urlQueue <- newTBQueueIO 512
    crawledQueue <- newTBQueueIO 128
    urlsInProgress <- S.newIO
    urlsCompleted <- S.newIO
    urlsFailed <- M.newIO
    return $ CrawlerState urlQueue crawledQueue urlsInProgress urlsCompleted urlsFailed

processNextUrl :: CrawlerState -> CanonicalUrl -> IO ()
processNextUrl crawlerState url =
    when (isAcceptable url) $
        atomically $ do
            completed <- S.lookup url (getUrlsCompleted crawlerState)
            inProgress <- S.lookup url (getUrlsInProgress crawlerState)
            failed <- isJust <$> M.lookup url (getUrlsFailed crawlerState)
            unless (completed || inProgress || failed) $
                writeTBQueue (getUrlQueue crawlerState) url

crawlNextUrl :: CrawlerState -> IO ()    
crawlNextUrl crawlerState = newManager tlsManagerSettings >>= \man -> 
    forever $ do

        nextUrl <- atomically $ do
            nextUrl <- readTBQueue (getUrlQueue crawlerState)
            S.insert nextUrl (getUrlsInProgress crawlerState)
            return nextUrl

        putStrLn $ "Grabbed: " ++ show nextUrl
        (mBodyData, redirects) <- getWithRedirects man nextUrl

        case mBodyData of
            Nothing ->
                atomically $ do
                    S.delete nextUrl (getUrlsInProgress crawlerState)
                    M.insert "Couldn't get data!" nextUrl (getUrlsFailed crawlerState)
            Just bodyData ->
                atomically $ do
                    S.delete nextUrl (getUrlsInProgress crawlerState)
                    mapM_ (\u -> S.insert u (getUrlsCompleted crawlerState)) redirects
                    writeTBQueue (getCrawledQueue crawlerState) (redirects, bodyData)

parsePages :: CrawlerState -> IO ()
parsePages crawlerState = forever $ do
    (redirects, dat) <- atomically $ readTBQueue (getCrawledQueue crawlerState)
    let referenceUrl = head redirects 
        rawHrefs = getRawHrefs referenceUrl dat
        nextHrefs = catMaybes rawHrefs
    when (length rawHrefs /= length nextHrefs) (putStrLn "Warning, not all Hrefs were good!")
    mapM_ (processNextUrl crawlerState) nextHrefs

drainQueue :: CrawlerState -> IO ()
drainQueue crawlerState =
    forever $ do
        (urls, content) <- atomically $ readTBQueue (getCrawledQueue crawlerState)
        putStrLn $ "Finished: " ++ show urls 
        
main :: IO ()
main = do

    crawlerState <- createCrawlerState
    mapM_ (\_ -> forkIO $ crawlNextUrl crawlerState) threadsPerJob
    mapM_ (\_ -> forkIO $ parsePages crawlerState) threadsPerJob
    _ <- forkIO $ drainQueue crawlerState

    getArgs >>=
        mapM_ (\a ->
            case canonicaliseString a of
                Just x -> processNextUrl crawlerState x
                Nothing -> putStrLn $ "Could not parse URL: " ++ a)
                         
    forever $ do
        threadDelay 1000000
        putStrLn "."

