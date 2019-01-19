module Workers (initialiseWorkers) where

import Crawl
import Errors
import Output as O
import Settings
import Types as T

import CountedQueue (CountedQueue)

import Control.Concurrent                   (ThreadId, forkIO)
import Control.Concurrent.STM               (STM, atomically)
import qualified Service
import qualified StmContainers.Set  as S
import qualified StmContainers.Map  as M

initialiseWorkers :: CountedQueue bq => Crawler bq -> LogFunction -> IO ()
initialiseWorkers crawler logFunc = do

    crawlerThreads       <- S.newIO
    crawlerThreadsToStop <- S.newIO 
    activeThreads        <- M.newIO
    threadClocks         <- M.newIO

    let workers = Workers { getCrawlerThreads       = crawlerThreads,
                            getCrawlerThreadsToStop = crawlerThreadsToStop,
                            getActiveThreads        = activeThreads,
                            getThreadClocks         = threadClocks}

    setNumCrawlers crawler workers logFunc numStartCrawlers

    forkWorker workers "Storage" $ storePages (makePageStore crawler)

    forkWorker workers "Logging" $ logErrors crawler logFunc

    forkWorker workers "Service" $ Service.start crawler workers

forkWorker :: Workers -> String -> IO () -> IO ()
forkWorker workers threadName a = do
    threadId <- forkIO a
    atomically $ subscribe threadId

    where
    subscribe :: ThreadId -> STM ()
    subscribe threadId = M.insert threadName threadId (getActiveThreads workers)

makePageStore :: Crawler bq -> PageStore bq
makePageStore crawler =
    PageStore { O.getCrawlerStatus = T.getCrawlerStatus crawler 
              , O.getOutputType    = T.getCrawlOutputType . getCrawlerSettings $ crawler
              , O.getStoreQueue    = T.getStoreQueue crawler
              }
