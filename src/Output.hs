module Output where

import Communication
import CountedQueue                     (CountedQueue, readQueue)
import Control.Concurrent.STM           (atomically, readTVar, writeTVar)
import Data.ByteString                  (ByteString)
import Types
import Control.Monad.IO.Class           (liftIO)
import Control.Monad.Trans.Resource     (runResourceT, MonadResource)
import Data.Conduit
import Data.Conduit.Binary              (sinkFile, sinkHandle)
import Data.Conduit.List as L           (map)
import System.IO                        (stdout)

import WarcDocument

storePages :: CountedQueue bq => Crawler bq -> IO ()
storePages crawler = do

    mOutput <- atomically . readTVar . getCrawlOutput . getCrawlerSettings $ crawler

    runResourceT $ fetchUpstream crawler
                =$ L.map (\x -> fromCrawledDocument x :: CrawledWarcEntry)
                =$ L.map toStorableDocument
                $$ getSink mOutput

    atomically $ writeTVar (getCrawlerStatus crawler) Halted
    
fetchUpstream :: CountedQueue bq => MonadResource r => Crawler bq -> Source r CrawledDocument
fetchUpstream crawler = do

  ma <- liftIO . atomically $ do
    status <- readTVar . getCrawlerStatus $ crawler
    if (status /= Halted)
        then Just <$> (readQueue . getStoreQueue $ crawler)
        else return Nothing

  case ma of
      Nothing -> return ()
      Just x -> do
        yield x
        fetchUpstream crawler

getSink :: MonadResource r => Maybe Output -> Sink ByteString r ()
getSink              Nothing = sinkHandle stdout
getSink (Just (WarcFile wf)) = sinkFile wf
