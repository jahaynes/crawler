{-# LANGUAGE OverloadedStrings #-}

module Output where

import CountedQueue                     (sourceQueue)
import Data.ByteString.Char8  as C8     (putStr, putStrLn)
import Types
import Control.Monad.IO.Class           (liftIO)
import Control.Monad.Trans.Resource     (runResourceT)
import Data.Conduit
import Data.Conduit.Binary              (sinkFile)
import Data.Conduit.List as L

import WarcDocument
import Data.Warc.WarcEntry

type StoreFunction = CrawledDocument -> IO ()

storePages :: Crawler -> StoreFunction -> IO ()
storePages crawler storeFunc =
    runResourceT
        $  sourceQueue (getStoreQueue crawler)
        $= L.map (\x -> fromCrawledDocument x :: WarcEntry) 
        $= L.map (\x -> toByteString x)
        $$ sinkFile "some.warc"

        -- $$ awaitForever $ liftIO . storeFunc

