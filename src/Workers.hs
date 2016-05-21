module Workers where

import Communication
import Crawl
import Errors
import MessageHandler
import Output
import Settings
import Types

import Control.Concurrent                   (ThreadId, forkIO)
import Control.Concurrent.STM               (STM, atomically)
import qualified STMContainers.Set  as S
import qualified STMContainers.Map  as M

initialiseWorkers :: CrawlerState -> StoreFunction -> IO ()
initialiseWorkers crawlerState storeFunc = do

    crawlerThreads <- S.newIO
    crawlerThreadsToStop <- S.newIO 

    activeThreads <- M.newIO

    threadClocks <- M.newIO

    let workers = Workers { getCrawlerThreads = crawlerThreads,
                            getCrawlerThreadsToStop = crawlerThreadsToStop,

                            getActiveThreads = activeThreads,
                            getThreadClocks = threadClocks}

    setNumCrawlers crawlerState workers numStartCrawlers

    forkWorker workers "Storage" $ storePages crawlerState storeFunc

    forkWorker workers "Logging" $ logErrors crawlerState

    forkWorker workers "Message Handler" $ receiveMessagesWith (handleMessages crawlerState workers)                            

forkWorker :: Workers -> String -> IO () -> IO ()
forkWorker workers threadName a = do
    threadId <- forkIO a
    atomically $ subscribe threadId

    where
    subscribe :: ThreadId -> STM ()
    subscribe threadId = M.insert threadName threadId (getActiveThreads workers)
