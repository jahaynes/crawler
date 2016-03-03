{-# LANGUAGE OverloadedStrings #-}

module Output where

import CountedQueue                     (sourceQueue)
import Types
import Control.Concurrent               (ThreadId)
import Control.Monad.Trans.Resource     (runResourceT)
import Data.ByteString.Char8 as BS      (ByteString, pack, intercalate, concat)
import Data.Conduit
import Data.Conduit.Binary
import qualified Data.Conduit.List as CL

storePages :: CrawlerState -> IO ()
storePages crawlerState =  runResourceT
                        $  sourceQueue (getStoreQueue crawlerState)
                        $$ CL.map (\(tid, redirects, contents) -> (tid, map (\(CanonicalUrl url) -> url) redirects))
                        =$ CL.map redirectsLine
                        =$ sinkFile "stored.log"

    where
    redirectsLine :: (ThreadId, [ByteString]) -> ByteString
    redirectsLine (tid, redirects) = BS.concat [pack . show $ tid, " finished\t", intercalate " <- " redirects, "\n"]

