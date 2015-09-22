module Output where

import Types

import Control.Concurrent.STM           (atomically)
import Control.Monad                    (forever)

import CountedQueue                     (readQueue)

storePages :: CrawlerState -> IO ()
storePages crawlerState = forever $ do
    (redirectChain, content) <- atomically $ readQueue (getStoreQueue crawlerState)
    appendFile "stored.log" (show redirectChain ++ "\n")