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
messageHandler crawlerState (CommandMessage c) = handleCommand c
    where
    handleCommand (AddUrl url) = do
        print url
        canonicaliseAndAdd crawlerState url
        return Confirmation

messageHandler crawlerState (QuestionMessage q) = handleQuestion q >>= return . AnswerMessage
    where
    handleQuestion GetNumCrawlers = do
        return $ NumCrawlers 3

canonicaliseAndAdd crawlerState a =
    case canonicaliseByteString a of
        Just x -> processNextUrl crawlerState x
        Nothing -> putStrLn $ "Could not parse URL: " ++ show a

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

