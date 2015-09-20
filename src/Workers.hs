module Workers where

import Control.Concurrent                   (forkIO, threadDelay)
import Control.Concurrent.STM               (atomically)
import Control.Monad                        (forever)
import GHC.Conc                             (STM, ThreadId, threadStatus)
import qualified ListT              as L
import qualified STMContainers.Set  as S
import qualified STMContainers.Map  as M

data Workers = Workers {
    getCrawlerThreads :: S.Set ThreadId,
    getCrawlerThreadsToStop :: S.Set ThreadId,
    getActiveThreads :: M.Map ThreadId String
}

initialiseWorkers :: IO Workers
initialiseWorkers = do

    crawlerThreads <- S.newIO
    crawlerThreadsToStop <- S.newIO 
    activeThreads <- M.newIO

    let workers = Workers { getCrawlerThreads = crawlerThreads,
                            getCrawlerThreadsToStop = crawlerThreadsToStop,
                            getActiveThreads = activeThreads }
    forkWorker workers "monitor" $ monitor workers
    return workers

subscribe :: Workers -> String -> ThreadId -> STM ()
subscribe workers threadName threadId = M.insert threadName threadId (getActiveThreads workers)

monitor :: Workers -> IO ()
monitor workers = forever $ do
    
    ls <- atomically $ mapAsList (getActiveThreads workers)
    
    stati <- mapM threadStatus (map fst ls)
    
    let ns = zip (map snd ls) stati
    
    mapM_ (\(n,s) -> putStrLn $ n ++ ": " ++ show s) ns
    
    threadDelay 1000000

    where
    mapAsList :: M.Map a b -> STM [(a,b)]
    mapAsList = L.toList . M.stream

forkWorker :: Workers -> String -> IO () -> IO ()
forkWorker workers threadName a = do
    threadId <- forkIO a
    atomically $ subscribe workers threadName threadId    