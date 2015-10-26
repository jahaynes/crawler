{-# LANGUAGE OverloadedStrings #-}

module Main where

import Communication
import CountedQueue
import Crawl
import Parse
import MessageHandler   (handleMessages)
import Settings
import Types
import Output
import Errors
import Workers

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

    go crawlerState 
    where
    go crawlerState = do
        halted <- (== Halted) <$> (atomically . readTVar $ getCrawlerStatus crawlerState)
        if halted
            then return ()
            else threadDelay 1000000 >> go crawlerState
