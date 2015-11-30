{-# LANGUAGE OverloadedStrings #-}

module Output where

import CountedQueue                     (sourceQueue)
import Types

import Control.Monad.Trans.Resource     (runResourceT)
import Data.ByteString.Char8 as BS      (ByteString, intercalate, concat)
import Data.Conduit
import Data.Conduit.Binary
import qualified Data.Conduit.List as CL

storePages :: CrawlerState -> IO ()
storePages crawlerState =  runResourceT
                        $  sourceQueue (getStoreQueue crawlerState)
                        $$ CL.map (\(redirects, contents) -> map (\(CanonicalUrl url) -> url) redirects)
                        =$ CL.map redirectsLine
                        =$ sinkFile "stored.log"

    where
    redirectsLine :: [ByteString] -> ByteString
    redirectsLine redirects = BS.concat [intercalate " <- " redirects, "\n"]

