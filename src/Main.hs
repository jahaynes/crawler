{-# LANGUAGE OverloadedStrings #-}

module Main where

import Communication
import CountedQueue
import Crawl
import Parse
import Urls
import Settings
import Types
import Output
import Errors
import Workers

import Control.Concurrent               (threadDelay)
import Control.Concurrent.STM           (atomically)

import qualified STMContainers.Set as S
import qualified STMContainers.Map as M

createCrawlerState :: IO CrawlerState
createCrawlerState = do
    urlQueue <- newQueueIO Unbounded
    parseQueue <- newQueueIO Unbounded
    storeQueue <- newQueueIO (Bounded 32)
    loggingQueue <- newQueueIO (Bounded 128)
    urlsInProgress <- S.newIO
    urlsCompleted <- S.newIO
    urlsFailed <- M.newIO
    return $ CrawlerState urlQueue parseQueue storeQueue loggingQueue urlsInProgress urlsCompleted urlsFailed

messageHandler :: CrawlerState -> Message -> IO Message
messageHandler crawlerState (CommandMessage c) = handleCommand c >>= return . AnswerMessage
    where
    handleCommand (AddUrl url) =
        case canonicaliseByteString url of
            Nothing -> return . Failure $ "Couldn't canonicalise url: " ++ show url
            Just x -> do
                processNextUrl crawlerState x
                putStrLn "Added url to frontier"
                return Confirmation

messageHandler crawlerState (QuestionMessage q) = handleQuestion q >>= return . AnswerMessage
    where
    handleQuestion (GetQueueSize UrlQueue) = do
        i <- atomically . size . getUrlQueue $ crawlerState
        return . QueueSize $ i

main :: IO ()
main = do

    crawlerState <- createCrawlerState

    workers <- initialiseWorkers crawlerState

    setNumCrawlers crawlerState workers numStartCrawlers

    setNumParsers crawlerState workers numStartParsers

    forkWorker workers "storage" $ storePages crawlerState

    forkWorker workers "logging" $ logErrors crawlerState

    forkWorker workers "commandler" $ receiveMessagesWith (messageHandler crawlerState)

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

