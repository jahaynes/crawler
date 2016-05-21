{-# LANGUAGE OverloadedStrings #-}

module Output where

import CountedQueue                     (sourceQueue)
import Types
import Control.Monad.IO.Class           (liftIO)
import Control.Monad.Trans.Resource     (runResourceT)
import Data.Conduit

storePages :: CrawlerState -> (CrawledDocument -> IO a) -> IO ()
storePages crawlerState storeFunc = do
    x <- runResourceT
        $  sourceQueue (getStoreQueue crawlerState)
        $$ awaitForever $ liftIO . storeFunc

    return ()
