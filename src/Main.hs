{-# LANGUAGE OverloadedStrings #-}

module Main where

import Crawl
import Parse
import Urls
import Settings
import Types
import Output
import Errors
import Workers

import Control.Concurrent               (threadDelay)
import Control.Concurrent.STM.TBQueue
import Control.Concurrent.STM.TQueue
import Control.Monad                    (forever)

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

