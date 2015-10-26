module Output where

import Communication
import CountedQueue                     (tryReadQueue)
import Types

import Control.Applicative              ((<$>))
import Control.Concurrent.STM           (atomically, readTVar, modifyTVar')
import Control.Concurrent               (threadDelay)

data Timing = Later | Now

storePages :: CrawlerState -> IO ()
storePages crawlerState = go Now

    where
    go :: Timing -> IO ()
    go timing = do
        halting <- atomically checkHalt
        if halting
            then flushOutput
            else grab timing

    grab Later = threadDelay 1000000 >> go Now
    grab Now = do
        mVals <- atomically $ tryReadQueue (getStoreQueue crawlerState)
        case mVals of
            Nothing -> go Later
            Just (redirectChain, content) -> do
                appendFile "stored.log" (show redirectChain ++ "\n")
                go Now

    checkHalt = (== HaltingStatus) <$> (readTVar $ getCrawlerStatus crawlerState)

    flushOutput = do
        --Do necessary flushing here
        atomically $ modifyTVar' (getCrawlerStatus crawlerState) (const Halted)
