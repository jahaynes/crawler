{-# LANGUAGE OverloadedStrings #-}

module Output where

import CountedQueue                     (sourceQueue)
import Types
import Control.Monad.IO.Class           (liftIO)
import Control.Monad.Trans.Resource     (runResourceT)
import Data.Conduit

storePages :: CrawlerState -> StoreFunction -> IO ()
storePages crawlerState storeFunc =
    runResourceT
        $  sourceQueue (getStoreQueue crawlerState)
        $$ awaitForever $ liftIO . storeFunc
