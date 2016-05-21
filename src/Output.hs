{-# LANGUAGE OverloadedStrings #-}

module Output where

import CountedQueue                     (sourceQueue)
import Types
import Control.Concurrent               (ThreadId)
import Control.Monad.IO.Class           (liftIO)
import Control.Monad.Trans.Resource     (runResourceT)
import Data.ByteString.Char8 as BS      (ByteString, pack, intercalate, concat)
import Data.Conduit
import Data.Conduit.Binary
import qualified Data.Conduit.List as CL

storePages :: CrawlerState -> (CrawledDocument -> IO a) -> IO ()
storePages crawlerState storeFunc = do
    x <- runResourceT
        $  sourceQueue (getStoreQueue crawlerState)
        $$ awaitForever $ liftIO . storeFunc

    return ()

    where
    redirectsLine :: (ThreadId, [ByteString]) -> ByteString
    redirectsLine (tid, redirects) = BS.concat [pack . show $ tid, " finished\t", intercalate " <- " redirects, "\n"]

