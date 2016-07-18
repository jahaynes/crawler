{-# LANGUAGE OverloadedStrings #-}

module Main where

import Communication
import Crawl
import Includes
import Settings
import Types
import Workers

import Control.Concurrent               (threadDelay)
import Control.Concurrent.STM           (atomically, readTVar)
import Control.Monad                    (unless)
import Data.List
import Data.Maybe                       (mapMaybe)
import qualified Data.Map               as Map

import Safe

import System.Environment               (getArgs)
import System.IO
import System.Remote.Monitoring         (forkServer)

optionMapFromArgs :: [String] -> OptionMap
optionMapFromArgs =   OptionMap
                  <$> Map.unionsWith union
                  .   mapMaybe (\gr -> Map.singleton <$> (OptionFlag <$> headMay gr) <*> tailMay gr)
                  .   filter (maybe False isFlag . headMay)
                  .   groupBy (\a b -> isFlag a && not (isFlag b))

    where
    isFlag x = length x > 1 && head x == '-'

defaultStorage :: StoreFunction
defaultStorage = print . head . getRedirectChain

defaultLogging :: LogFunction
defaultLogging = hPrint stderr

main :: IO ()
main = do

    optionMap <- optionMapFromArgs <$> getArgs

    _ <- forkServer "localhost" 8000

    crawler <- createCrawler

    initialiseFormInstructions crawler optionMap

    initialiseIncludes crawler optionMap

    initialiseWorkers crawler defaultStorage defaultLogging

    run (getCrawlerStatus crawler)

    where
    run crawlerStatus = do
        halted <- (== Halted) <$> (atomically . readTVar $ crawlerStatus)
        unless halted $ do
            threadDelay 1000000
            run crawlerStatus
