{-# LANGUAGE OverloadedStrings #-}

module Output where

import CountedQueue                     (sourceQueue)
import Control.Concurrent.STM           (atomically, readTVar)
import Types
import Control.Monad.Trans.Resource     (runResourceT, ResourceT)
import Data.Conduit
import Data.Conduit.Binary              (sinkFile, sinkHandle)
import Data.Conduit.List as L           (map)
import System.IO                        (stdout)

import WarcDocument                     ()
import Data.Warc.WarcEntry

storePages :: Crawler -> IO ()
storePages crawler = do

    mOutput <- atomically . readTVar . getCrawlOutput . getCrawlerSettings $ crawler

    runResourceT
        $  sourceQueue (getStoreQueue crawler)
        $$ getSink mOutput

getSink :: Maybe Output -> Sink CrawledDocument (ResourceT IO) ()
getSink mOutput =  L.map (\x -> fromCrawledDocument x :: WarcEntry) 
                =$ L.map (\x -> toByteString x)
                =$ case mOutput of
                       Nothing -> sinkHandle stdout
                       (Just (WarcFile warcFile)) -> sinkFile warcFile