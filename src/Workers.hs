module Workers where

import Control.Concurrent                   (forkIO, threadDelay)
import Control.Concurrent.STM               (atomically)
import Control.Monad                        (forever)
import GHC.Conc                             (STM, ThreadId, threadStatus)
import qualified ListT              as L
import qualified STMContainers.Map  as M

data Workers = Workers {
    getActiveThreads :: M.Map ThreadId String
}

initialiseHealth :: IO Workers
initialiseHealth = do
    m <- M.newIO
    let workers = Workers {getActiveThreads = m}
    forkHealth workers "monitor" $ monitor workers
    return workers

subscribe :: Workers -> String -> ThreadId -> STM ()
subscribe workers threadName threadId = M.insert threadName threadId (getActiveThreads workers)

monitor :: Workers -> IO ()
monitor workers = forever $ do
    
    ls <- atomically . L.toList . M.stream . getActiveThreads $ workers
    
    stati <- mapM threadStatus (map fst ls)
    
    let ns = zip (map snd ls) stati
    
    mapM_ (\(n,s) -> putStrLn $ n ++ ": " ++ show s) ns
    
    threadDelay 1000000

forkHealth :: Workers -> String -> IO () -> IO ()
forkHealth workers threadName a = do
    threadId <- forkIO a
    atomically $ subscribe workers threadName threadId    