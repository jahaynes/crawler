module Output where

import Communication                    (CrawlerStatus (Halted))
import CountedQueue                     (CountedQueue, readQueue)
import Types
import WarcDocument                     (CrawledWarcEntry)

import Control.Concurrent.STM           (TVar, atomically, readTVar, readTVarIO, writeTVar)
import Control.Monad.IO.Class           (liftIO)
import Control.Monad.Trans.Resource     (runResourceT, MonadResource)
import Data.ByteString                  (ByteString)
import Data.Conduit
import Data.Conduit.Binary              (sinkFile, sinkHandle)
import Data.Conduit.List as L           (map)
import System.IO                        (stdout)

data PageStore bq =
    PageStore { getCrawlerStatus :: TVar CrawlerStatus
              , getOutputType    :: TVar (Maybe Output)
              , getStoreQueue    :: bq CrawledDocument
              }

storePages :: CountedQueue bq => PageStore bq -> IO ()
storePages (PageStore crawlerStatus outputType storeQueue) = do

    mOutput <- readTVarIO outputType

    runResourceT $ fetchUpstream
                =$ L.map (\x -> fromCrawledDocument x :: CrawledWarcEntry)
                =$ L.map toStorableDocument
                $$ getSink mOutput

    atomically $ writeTVar crawlerStatus Halted

    where
    fetchUpstream :: MonadResource r => Source r CrawledDocument
    fetchUpstream = do

        ma <- liftIO . atomically $ do
            status <- readTVar crawlerStatus
            if status /= Halted
                then Just <$> readQueue storeQueue
                else return Nothing

        case ma of
            Nothing -> return ()
            Just x -> do
                yield x
                fetchUpstream

    getSink :: MonadResource r => Maybe Output -> Sink ByteString r ()
    getSink              Nothing = sinkHandle stdout
    getSink (Just (WarcFile wf)) = sinkFile wf
