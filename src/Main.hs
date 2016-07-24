{-# LANGUAGE OverloadedStrings #-}

module Main where

import Communication
import Crawl
import Errors
import Initialisation
import Output
import Types
import Workers

import Control.Concurrent               (threadDelay)
import Control.Concurrent.STM           (atomically, readTVar)
import Control.Monad                    (unless)

import System.Environment               (getArgs)
import System.IO
import System.Remote.Monitoring         (forkServer)

defaultStorage :: StoreFunction
defaultStorage = print . head . getRedirectChain

defaultLogging :: LogFunction
defaultLogging = hPrint stderr

main :: IO ()
main = do

    optionMap <- optionMapFromArgs <$> getArgs

    _ <- forkServer "localhost" 8000

    crawler <- createCrawler

    initialiseSettings crawler optionMap

    initialiseWorkers crawler defaultStorage defaultLogging

    run (getCrawlerStatus crawler)

    where
    run crawlerStatus = do
        halted <- (== Halted) <$> (atomically . readTVar $ crawlerStatus)
        unless halted $ do
            threadDelay 1000000
            run crawlerStatus
