module Output where

import Types

import Control.Concurrent.STM           (atomically)
import Control.Monad                    (forever)

import Control.Concurrent.STM.TBQueue   (readTBQueue)

storePages :: CrawlerState -> IO ()
storePages crawlerState = forever $ do
    (redirectChain, content) <- atomically $ readTBQueue (getStoreQueue crawlerState)
    appendFile "stored.log" (show redirectChain ++ "\n")