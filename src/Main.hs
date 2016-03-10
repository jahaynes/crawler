{-# LANGUAGE OverloadedStrings #-}

module Main where

import Communication
import CountedQueue
import Crawl
import Data.ByteString.Char8            (ByteString, pack)
import qualified PoliteQueue    as PQ
import Settings
import Types
import Urls
import Workers

import Control.Applicative              ((<$>))
import Control.Concurrent               (threadDelay)
import Control.Concurrent.STM           (atomically, newTVarIO, readTVar)
import Control.Monad                    (unless)

import Network.HTTP.Conduit             (newManager, tlsManagerSettings, mkManagerSettings)
import Network.Connection               (TLSSettings (TLSSettingsSimple))

import qualified STMContainers.Set as S
import qualified STMContainers.Map as M

import System.Environment               (getArgs)
import System.Remote.Monitoring         (forkServer)

createCrawlerState :: IO CrawlerState
createCrawlerState = do
    formInstructions <- loadFormInstructions "form_instructions.cfg"
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

    _ <- forkServer "localhost" 8000

    crawlerState <- createCrawlerState

    initialiseWorkers crawlerState

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
