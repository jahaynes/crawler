module Health where

--import Types

import GHC.Conc                             (STM, ThreadId, threadStatus)
import Control.Concurrent.STM               (atomically)
import qualified STMContainers.Map as M
import Control.Concurrent                   (forkIO, threadDelay)
import Control.Monad                        (forever, when, unless)
import qualified ListT as L

data Health = Health {
    getActiveThreads :: M.Map ThreadId String
}

initialiseHealth :: IO Health
initialiseHealth = do
    m <- M.newIO
    let health = Health {getActiveThreads = m}
    forkHealth health "monitor" $ monitor health
    return health

subscribe :: Health -> String -> ThreadId -> STM ()
subscribe health threadName threadId = M.insert threadName threadId (getActiveThreads health)

monitor :: Health -> IO ()
monitor health = forever $ do
    
    ls <- atomically . L.toList . M.stream . getActiveThreads $ health
    
    stati <- mapM threadStatus (map fst ls)
    
    let ns = zip (map snd ls) stati
    
    mapM_ (\(n,s) -> putStrLn $ n ++ ": " ++ show s) ns
    
    print "Bleh"
    threadDelay 1000000

forkHealth :: Health -> String -> IO () -> IO ()
forkHealth health threadName a = do
    threadId <- forkIO a
    atomically $ subscribe health threadName threadId    