{-# LANGUAGE OverloadedStrings #-}

module Main where

import Communication
import CountedQueue
import Crawl
import Data.ByteString.Char8            (ByteString, pack)
import MessageHandler                   (handleMessages)
import qualified PoliteQueue    as PQ
import Settings
import Types
import Urls
import Output
import Errors
import Workers

import Control.Applicative              ((<$>))
import Control.Concurrent               (threadDelay)
import Control.Concurrent.STM           (atomically, newTVarIO, readTVar)
import Control.Monad                    (unless)

import Network.HTTP.Conduit             (newManager, tlsManagerSettings)

import qualified STMContainers.Set as S
import qualified STMContainers.Map as M

import System.Environment               (getArgs)

createCrawlerState :: IO CrawlerState
createCrawlerState = do
    crawlerStatus <- newTVarIO RunningStatus
    urlQueue <- PQ.newIO
    storeQueue <- newQueueIO (Bounded 32)
    loggingQueue <- newQueueIO (Bounded 128)
    manager <- newManager tlsManagerSettings
    cookieList <- newTVarIO []
    urlPatterns <- S.newIO
    urlsInProgress <- S.newIO
    urlsCompleted <- S.newIO
    urlsFailed <- M.newIO
    return $ CrawlerState crawlerStatus urlQueue storeQueue loggingQueue manager cookieList urlPatterns urlsInProgress urlsCompleted urlsFailed

data StartMode = WithFrontEnd
               | Headless CanonicalUrl ByteString
               | FailStartup String

parseArgs :: [String] -> StartMode
parseArgs [strStartUrl, includePattern] =
    case canonicaliseString strStartUrl of
        Nothing -> FailStartup $ "Could not canonicalise url: " ++ strStartUrl
        Just startUrl -> Headless startUrl (pack includePattern)
parseArgs _ = WithFrontEnd

main :: IO ()
main = do

    crawlerState <- createCrawlerState

    workers <- initialiseWorkers

    setNumCrawlers crawlerState workers numStartCrawlers

    forkWorker workers "Storage" $ storePages crawlerState

    forkWorker workers "Logging" $ logErrors crawlerState

    forkWorker workers "Message Handler" $ receiveMessagesWith (handleMessages crawlerState workers)

    startMode <- parseArgs <$> getArgs

    case startMode of
        FailStartup msg -> error msg
        WithFrontEnd -> run (getCrawlerStatus crawlerState)
        Headless startUrl includePattern -> do
            atomically . S.insert includePattern . getUrlPatterns $ crawlerState
            launchSuccess <- processNextUrl crawlerState startUrl
            case launchSuccess of
                Success -> run (getCrawlerStatus crawlerState)
                Failure reason -> error reason

    where
    run crawlerStatus = do
        halted <- (== Halted) <$> (atomically . readTVar $ crawlerStatus)
        unless halted $ do
            threadDelay 1000000
            run crawlerStatus
