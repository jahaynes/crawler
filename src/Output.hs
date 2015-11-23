module Output where

import Communication
import CountedQueue                     (tryReadQueue)
import Types

import Control.Applicative              ((<$>))
import Control.Concurrent.STM           (atomically, readTVar, modifyTVar')
import Control.Concurrent               (threadDelay)

import Data.ByteString (ByteString)
import Data.Smashy.Types
import qualified Data.Smashy.Map    as HM

data Timing = Later | Now

storePages :: CrawlerState -> IO ()
storePages crawlerState = do
    hm <- HM.new (Disk "stored")
    go hm Now

    where
    go :: HashMap ByteString ByteString -> Timing -> IO ()
    go hm timing = do
        halting <- atomically checkHalt
        if halting
            then flushOutput hm
            else grab hm timing

    grab hm Later = threadDelay 1000000 >> go hm Now
    grab hm Now = do
        mVals <- atomically $ tryReadQueue (getStoreQueue crawlerState)
        case mVals of
            Nothing -> go hm Later
            Just (redirectChain, content) -> do
                hm' <- HM.storeOne hm ((\(CanonicalUrl url) -> url) (last redirectChain), content)
                appendFile "stored.log" (show redirectChain ++ "\n")
                go hm' Now

    checkHalt = (== HaltingStatus) <$> readTVar (getCrawlerStatus crawlerState)

    flushOutput hm = do
        HM.close hm
        atomically $ modifyTVar' (getCrawlerStatus crawlerState) (const Halted)
