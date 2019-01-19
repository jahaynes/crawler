{-# LANGUAGE OverloadedStrings #-}

module Main where

import Communication
import CountedQueue as CC
import Crawl
import Initialisation
import PoliteQueue as PQ
import Types as T
import Workers

import Control.Concurrent       (threadDelay)
import Control.Concurrent.STM   (STM, atomically, readTVar, readTVarIO, writeTVar)
import Control.Monad            (unless, when)
import System.Environment       (getArgs)
import System.IO                (hPrint, stderr)
import System.Remote.Monitoring (forkServer)


defaultLogging :: Loggable -> IO ()
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
        atomically checkShutdown

        status <- readTVarIO . getCrawlerStatus $ crawler

        unless (status == Halted) $ run crawler

        where
        oneSecond = 1000000

        checkShutdown :: STM ()
        checkShutdown = do

          status <- readTVar . getCrawlerStatus $ crawler

          when (status == RunningStatus) $ do
            noWork <- (\a b c -> a && b && c) <$> ((==0) <$> (PQ.size . getUrlQueue $ crawler))
                                              <*> (isEmpty . getStoreQueue $ crawler)
                                              <*> (isEmpty . T.getLogQueue $ crawler)

            numStored <- readTVar . getNumStored $ crawler
            mCrawlLimit <- readTVar . getCrawlLimit . getCrawlerSettings $ crawler
            let quotaDone = case mCrawlLimit of
                                Nothing -> False
                                Just cl -> numStored >= cl

            when (noWork || quotaDone) $
              writeTVar (getCrawlerStatus crawler) HaltingStatus
