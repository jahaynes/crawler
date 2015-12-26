{-# LANGUAGE OverloadedStrings #-}

module Main where

import Communication
import CountedQueue
import Crawl
import MessageHandler   (handleMessages)
import qualified PoliteQueue    as PQ
import Shared
import Settings
import Types
import Output
import Errors
import Workers

import Data.Ord (comparing)
import Data.List (sortBy)
import Data.Time

import Control.Applicative              ((<$>))
import Control.Concurrent               (threadDelay)
import Control.Concurrent.STM           (atomically, newTVarIO, readTVar)
import Control.Monad                    (forever, unless)

import qualified STMContainers.Set as S
import qualified STMContainers.Map as M

createCrawlerState :: IO CrawlerState
createCrawlerState = do
    crawlerStatus <- newTVarIO RunningStatus
    urlQueue <- PQ.newIO
    storeQueue <- newQueueIO (Bounded 32)
    loggingQueue <- newQueueIO (Bounded 128)
    cookieList <- newTVarIO []
    urlPatterns <- S.newIO
    urlsInProgress <- S.newIO
    urlsCompleted <- S.newIO
    urlsFailed <- M.newIO
    return $ CrawlerState crawlerStatus urlQueue storeQueue loggingQueue cookieList urlPatterns urlsInProgress urlsCompleted urlsFailed

main :: IO ()
main = do

    crawlerState <- createCrawlerState

    workers <- initialiseWorkers

    setNumCrawlers crawlerState workers numStartCrawlers

    forkWorker workers "Storage" $ storePages crawlerState

    forkWorker workers "Logging" $ logErrors crawlerState

    forkWorker workers "Message Handler" $ receiveMessagesWith (handleMessages crawlerState workers)

    forever $ do
        halted <- (== Halted) <$> (atomically . readTVar $ getCrawlerStatus crawlerState)
        unless halted $ do
            {-clockList <- atomically . mapAsList . getThreadClocks $ workers
            time <- getCurrentTime
            putStrLn "\n"
            mapM_ (\(a,(t,u)) -> putStrLn $ show a ++ "\t" ++ show (diffUTCTime time t) ++ "\t" ++ show u ) . sortBy (comparing snd) $ clockList -}
            threadDelay 1000000

