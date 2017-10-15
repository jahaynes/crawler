{-# LANGUAGE OverloadedStrings #-}

module Main where

import CountedQueue as CC
import Crawl
import Errors
import Initialisation
import PoliteQueue as PQ
import Types
import Workers

import Control.Concurrent               (threadDelay)
import Control.Concurrent.STM           (atomically, readTVar)
import Control.Monad                    (unless)

import System.Environment               (getArgs)
import System.IO                        (hPrint, stderr)
import System.Remote.Monitoring         (forkServer)


defaultLogging :: LogFunction
defaultLogging = hPrint stderr

main :: IO ()
main = do

    args <- getArgs

    _ <- forkServer "localhost" 8000

    defaultLogging $ GeneralMessage "Preparing crawler"
    crawler <- createCrawler

    defaultLogging $ GeneralMessage "Initialising logging"
    initialiseSettings crawler args defaultLogging

    defaultLogging $ GeneralMessage "Initialising workers"
    initialiseWorkers crawler defaultLogging

    defaultLogging $ GeneralMessage "Running crawler"
    run crawler

    where
    run crawler = do
        threadDelay oneSecond
        stop <- shouldStop
        unless stop $ run crawler

        where
        oneSecond = 1000000

        shouldStop = atomically $ do

          noWork <- (\a b c -> a && b && c) <$> ((==0) <$> (PQ.size . getUrlQueue $ crawler))
                                            <*> (isEmpty . getStoreQueue $ crawler)
                                            <*> (isEmpty . getLogQueue $ crawler)

          numStored <- readTVar . getNumStored $ crawler
          mCrawlLimit <- readTVar . getCrawlLimit . getCrawlerSettings $ crawler
          let quotaDone = case mCrawlLimit of
                              Nothing -> False
                              Just cl -> numStored >= cl

          return (noWork || quotaDone) 
