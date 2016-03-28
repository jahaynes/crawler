{-# LANGUAGE OverloadedStrings #-}

module Main where

import Communication
import CountedQueue
import Includes
import qualified PoliteQueue    as PQ
import Settings
import Types
import Workers

import Control.Applicative              ((<$>), (<*>))
import Control.Concurrent               (threadDelay)
import Control.Concurrent.STM           (newTVarIO, readTVar, atomically)
import Control.Monad                    (unless)
import Data.List
import Data.Maybe                       (mapMaybe)
import qualified Data.Map               as Map
import Network.HTTP.Conduit             (newManager, tlsManagerSettings, mkManagerSettings)
import Network.Connection               (TLSSettings (TLSSettingsSimple))
import Safe
import qualified STMContainers.Set as S
import qualified STMContainers.Map as M

import System.Environment               (getArgs)
import System.Remote.Monitoring         (forkServer)

createCrawlerState :: IO CrawlerState
createCrawlerState = do
    formInstructions <- newTVarIO (SuppliedFormActions Map.empty)
    crawlerStatus <- newTVarIO RunningStatus
    urlQueue <- PQ.newIO
    storeQueue <- newQueueIO (Bounded 32)
    loggingQueue <- newQueueIO (Bounded 128)
    manager <- newManager (if ignoreBadHttpsCerts
                               then mkManagerSettings (TLSSettingsSimple True False False) Nothing
                               else tlsManagerSettings)
    cookieList <- newTVarIO []
    urlPatterns <- S.newIO
    urlsInProgress <- S.newIO
    urlsCompleted <- S.newIO
    urlsFailed <- M.newIO
    return $ CrawlerState formInstructions crawlerStatus urlQueue storeQueue loggingQueue manager cookieList urlPatterns urlsInProgress urlsCompleted urlsFailed

optionMapFromArgs :: [String] -> OptionMap
optionMapFromArgs =   OptionMap
                  <$> Map.unionsWith union
                  .   mapMaybe (\gr -> Map.singleton <$> (OptionFlag <$> headMay gr) <*> tailMay gr)
                  .   filter (\gr -> maybe False isFlag (headMay gr))
                  .   groupBy (\a b -> isFlag a && not (isFlag b))

    where
    isFlag x = length x > 1 && head x == '-'

main :: IO ()
main = do

    args <- getArgs

    let optionMap = optionMapFromArgs args

    _ <- forkServer "localhost" 8000

    crawlerState <- createCrawlerState

    initialiseFormInstructions crawlerState optionMap

    initialiseIncludes crawlerState optionMap

    initialiseWorkers crawlerState

    run (getCrawlerStatus crawlerState)

    where
    run crawlerStatus = do
        halted <- (== Halted) <$> (atomically . readTVar $ crawlerStatus)
        unless halted $ do
            threadDelay 1000000
            run crawlerStatus
