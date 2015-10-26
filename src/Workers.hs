module Workers where

import Control.Concurrent                   (ThreadId, forkIO)
import Control.Concurrent.STM               (STM, atomically)
import qualified STMContainers.Set  as S
import qualified STMContainers.Map  as M

data Workers = Workers {
    getCrawlerThreads :: S.Set ThreadId,
    getCrawlerThreadsToStop :: S.Set ThreadId,

    getParserThreads :: S.Set ThreadId,
    getParserThreadsToStop :: S.Set ThreadId,

    getActiveThreads :: M.Map ThreadId String
}

initialiseWorkers :: IO Workers
initialiseWorkers = do

    crawlerThreads <- S.newIO
    crawlerThreadsToStop <- S.newIO 

    parserThreads <- S.newIO
    parserThreadsToStop <- S.newIO 

    activeThreads <- M.newIO

    let workers = Workers { getCrawlerThreads = crawlerThreads,
                            getCrawlerThreadsToStop = crawlerThreadsToStop,

                            getParserThreads = parserThreads,
                            getParserThreadsToStop = parserThreadsToStop,

                            getActiveThreads = activeThreads }
    return workers

subscribe :: Workers -> String -> ThreadId -> STM ()
subscribe workers threadName threadId = M.insert threadName threadId (getActiveThreads workers)

forkWorker :: Workers -> String -> IO () -> IO ()
forkWorker workers threadName a = do
    threadId <- forkIO a
    atomically $ subscribe workers threadName threadId    