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

import Control.Concurrent               (threadDelay)
import Control.Concurrent.STM           (atomically, newTVarIO)

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

    workers <- initialiseWorkers crawlerState

    setNumCrawlers crawlerState workers numStartCrawlers

    setNumParsers crawlerState workers numStartParsers

    forkWorker workers "storage" $ storePages crawlerState

    forkWorker workers "logging" $ logErrors crawlerState

    forkWorker workers "commandler" $ receiveMessagesWith (handleMessages crawlerState)

    go crawlerState False
    where
    go            _ True = return ()
    go crawlerState False = do

        finished <- atomically $ do
            a <- isEmpty . getUrlQueue $ crawlerState
            b <- isEmpty . getParseQueue $ crawlerState
            c <- S.null . getUrlsInProgress $ crawlerState
            return $ a && b && c

        putStrLn "."
        threadDelay $ 3000000

        go crawlerState False

