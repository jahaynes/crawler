{-# LANGUAGE OverloadedStrings #-}

module Main where

import Communication
import CountedQueue
import Crawl
import Parse
import MessageHandler   (handleMessages)
import Shared
import Settings
import Types
import Output
import Errors
import Workers

import Data.Ord (comparing)
import Data.List (partition, sortBy)
import Data.Time

import Control.Applicative              ((<$>))
import Control.Concurrent               (threadDelay)
import Control.Concurrent.STM           (atomically, newTVarIO, readTVar)

import qualified STMContainers.Set as S
import qualified STMContainers.Map as M

createCrawlerState :: IO CrawlerState
createCrawlerState = do
    crawlerStatus <- newTVarIO RunningStatus
    urlQueue <- newQueueIO Unbounded
    parseQueue <- newQueueIO Unbounded
    storeQueue <- newQueueIO (Bounded 32)
    loggingQueue <- newQueueIO (Bounded 128)
    urlPatterns <- S.newIO
    urlsInProgress <- S.newIO
    urlsCompleted <- S.newIO
    urlsFailed <- M.newIO
    return $ CrawlerState crawlerStatus urlQueue parseQueue storeQueue loggingQueue urlPatterns urlsInProgress urlsCompleted urlsFailed

main :: IO ()
main = do

    crawlerState <- createCrawlerState

    workers <- initialiseWorkers

    setNumCrawlers crawlerState workers numStartCrawlers

    setNumParsers crawlerState workers numStartParsers

    forkWorker workers "Storage" $ storePages crawlerState

    forkWorker workers "Logging" $ logErrors crawlerState

    forkWorker workers "Message Handler" $ receiveMessagesWith (handleMessages crawlerState workers)

    go crawlerState workers
    where
    go crawlerState workers = do
        halted <- (== Halted) <$> (atomically . readTVar $ getCrawlerStatus crawlerState)
        if halted
            then return ()
            else do
                clockList <- atomically . mapAsList . getThreadClocks $ workers
                time <- getCurrentTime
                putStrLn "\n"
                mapM_ (\(a,(t,u)) -> putStrLn $ show a ++ "\t" ++ show (diffUTCTime time t) ++ "\t" ++ show u ) . sortBy (comparing snd) $ clockList
                threadDelay 1000000
                go crawlerState workers
