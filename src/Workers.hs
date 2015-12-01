module Workers where

import Control.Concurrent                   (ThreadId, forkIO)
import Control.Concurrent.STM               (STM, atomically)
import Data.Time                            (UTCTime)
import qualified STMContainers.Set  as S
import qualified STMContainers.Map  as M
import Types

data Workers = Workers {
    getCrawlerThreads :: S.Set ThreadId,
    getCrawlerThreadsToStop :: S.Set ThreadId,

    getActiveThreads :: M.Map ThreadId String,
    getThreadClocks :: M.Map ThreadId (UTCTime, CanonicalUrl)
}

initialiseWorkers :: IO Workers
initialiseWorkers = do

    crawlerThreads <- S.newIO
    crawlerThreadsToStop <- S.newIO 

    activeThreads <- M.newIO
    
    threadClocks <- M.newIO

    let workers = Workers { getCrawlerThreads = crawlerThreads,
                            getCrawlerThreadsToStop = crawlerThreadsToStop,

                            getActiveThreads = activeThreads,
                            getThreadClocks = threadClocks}
    return workers

subscribe :: Workers -> String -> ThreadId -> STM ()
subscribe workers threadName threadId = M.insert threadName threadId (getActiveThreads workers)

forkWorker :: Workers -> String -> IO () -> IO ()
forkWorker workers threadName a = do
    threadId <- forkIO a
    atomically $ subscribe workers threadName threadId