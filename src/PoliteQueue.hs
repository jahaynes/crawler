module PoliteQueue where

import Urls (getDomain)
import Types
import Data.Maybe                   (fromJust)
import Control.Applicative          ((<$>))
import Control.Concurrent           (ThreadId)
import qualified Control.Concurrent.STM.TQueue as Q
import Control.Monad.STM    (STM)
import qualified STMContainers.Map as M
import qualified Control.Concurrent.STM.TVar as TV

newIO :: IO PoliteQueue
newIO = do
    sz <- TV.newTVarIO 0
    tm <- M.newIO
    dm <- M.newIO
    sl <- Q.newTQueueIO
    return $ PoliteQueue sz tm dm sl

size :: PoliteQueue -> STM Int
size = TV.readTVar . getSize
    
writeQueue :: CanonicalUrl -> PoliteQueue -> STM Success
writeQueue url politeQueue = do
    TV.modifyTVar' (getSize politeQueue) (+1)
    case getDomain url of
        Nothing -> return . Failure $ "Could not get domain from url: " ++ show url
        Just domain -> do
            mQueue <- M.lookup domain (getDomainMapping politeQueue)
            case mQueue of
                Just queue -> Q.writeTQueue queue url
                Nothing -> Q.writeTQueue (getSlowLane politeQueue) url
            return Success
                   
readQueue :: ThreadId -> PoliteQueue -> STM CanonicalUrl
readQueue threadId politeQueue = do
    mMyDomain <- M.lookup threadId (getThreadMapping politeQueue)
    case mMyDomain of
        Just myDomain -> do
            myQueue <- fromJust <$> M.lookup myDomain (getDomainMapping politeQueue)
            mUrl <- Q.tryReadTQueue myQueue
            case mUrl of
                Just url -> do
                    TV.modifyTVar' (getSize politeQueue) (\x -> x - 1)
                    return url
                Nothing -> do
                    M.delete threadId (getThreadMapping politeQueue)
                    M.delete myDomain (getDomainMapping politeQueue)
                    readQueue threadId politeQueue
        Nothing ->
            let loop = do
                    nextUrl <- Q.readTQueue (getSlowLane politeQueue)
                    let domain = fromJust . getDomain $ nextUrl
                    mQueueForDomain <- M.lookup domain (getDomainMapping politeQueue)
                    case mQueueForDomain of
                        Just queueForDomain -> do
                            Q.writeTQueue queueForDomain nextUrl
                            loop
                        Nothing -> do
                            M.insert domain threadId (getThreadMapping politeQueue)
                            newQueue <- Q.newTQueue
                            M.insert newQueue domain (getDomainMapping politeQueue)
                            TV.modifyTVar' (getSize politeQueue) (\x -> x - 1)
                            return nextUrl
            in loop
               

                   