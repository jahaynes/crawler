{-# LANGUAGE OverloadedStrings #-}

module MessageHandler where

import Communication
import CountedQueue
import Crawl
import Shared
import Types
import Urls
import Workers

import Control.Concurrent.STM           (atomically, readTVar, modifyTVar')
import Control.Monad                    (liftM)
import Data.Function                    (on)
import Data.List                        (groupBy)
import GHC.Conc                         (threadStatus)
import Network.HTTP.Conduit

import qualified Data.ByteString.Char8      as C8
import qualified STMContainers.Set          as S

handleMessages :: CrawlerState -> Workers -> Message -> IO Message
handleMessages crawlerState workers (CommandMessage c) = liftM AnswerMessage . handleCommand $ c
    where

    {- Add a URL -}
    handleCommand (AddUrl url) =
        case canonicaliseByteString url of
            Nothing -> return . Failure $ "Couldn't canonicalise url: " ++ show url
            Just x -> do
                accepted <- processNextUrl crawlerState x
                print accepted
                return Confirmation

    {- Remove a URL -}
    handleCommand (RemoveUrl _) = undefined

    {- Set the URL Patterns -}
    handleCommand (SetUrlPatterns ps) = do
        atomically $ do
            let patterns = getUrlPatterns crawlerState
            existing <- setAsList patterns
            mapM_ (`S.delete` patterns) existing
            mapM_ (`S.insert` patterns) ps
        return Confirmation

    handleCommand (SetNumCrawlers _) = undefined

    {- Tell the crawler to idle -}
    handleCommand Idle = do

        (willIdle, oldStatus) <- atomically $ do
            status <- readTVar (getCrawlerStatus crawlerState)
            case status of
                 HaltingStatus -> return (False, status)
                 _ -> do
                     modifyTVar' (getCrawlerStatus crawlerState) (const IdleStatus) 
                     return (True, status)
        if willIdle
            then do
                putStrLn $ "Switching from " ++ show oldStatus ++ " to idle..."
                setNumCrawlers crawlerState workers 0
                return Confirmation
            else do
                let msg = "Can't start idling from state " ++ show oldStatus
                putStrLn msg
                return $ Failure msg

    {- Tell the crawler to halt -}
    handleCommand Halt = do
        
        willHalt <- atomically $ do
            status <- readTVar (getCrawlerStatus crawlerState)
            case status of
                HaltingStatus -> return False
                _ -> do
                    modifyTVar' (getCrawlerStatus crawlerState) (const HaltingStatus)
                    return True
        if willHalt
            then do
                putStrLn "Halting..."
                setNumCrawlers crawlerState workers 0
                return Confirmation
            else do
                let msg = "Can't halt (was already halting)"
                putStrLn msg
                return $ Failure msg

handleMessages crawlerState workers (QuestionMessage q) = liftM AnswerMessage . handleQuestion $ q
    where

    {- Ask the size of the URL Queue -}
    handleQuestion (GetQueueSize queue) =
        case queue of
            UrlQueue -> returnQueueSize . getUrlQueue $ crawlerState
            StoreQueue -> returnQueueSize . getStoreQueue $ crawlerState
            ErrorQueue -> returnQueueSize . getLogQueue $ crawlerState

        where
        returnQueueSize = liftM QueueSize . atomically . size

    {- Ask for the Crawler's current Status -}
    handleQuestion GetCrawlerStatus = liftM CrawlerStatus . atomically . readTVar . getCrawlerStatus $ crawlerState

    {- Get the workers' statuses -}
    handleQuestion GetWorkerStatuses = do
        ls <- atomically $ mapAsList (getActiveThreads workers)
        statuses <- mapM (threadStatus . fst) ls
        let ns = zip (map snd ls) statuses
            ss = map (\(n,s) -> n ++ ": " ++ show s) ns
        return $ WorkerStatus ss

    {- Get a report of the shared cookies -}
    handleQuestion GetCookieReport = do
        cookies <- atomically . readTVar . getCookieList $ crawlerState

        --let byDomain = groupBy ((==) `on` cookie_domain) cookies

        let rendered = map (\c -> C8.concat [cookie_name c, ": ", cookie_value c, "<br/>"]) cookies

        return $ CookieReport (map C8.unpack rendered)

handleMessages _ _ (AnswerMessage _) = error "Shouldn't have received AnswerMessage"
