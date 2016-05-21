{-# LANGUAGE OverloadedStrings #-}

module Output where

import CountedQueue                     (sourceQueue)
import Types
import Control.Monad.IO.Class           (liftIO)
import Control.Monad.Trans.Resource     (runResourceT)
import Data.Conduit

storePages :: Crawler -> StoreFunction -> IO ()
storePages crawler storeFunc =
    runResourceT
        $  sourceQueue (getStoreQueue crawler)
        $$ awaitForever $ liftIO . storeFunc
