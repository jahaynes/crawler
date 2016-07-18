{-# LANGUAGE OverloadedStrings #-}

module MessageHandler where

import Communication
import CountedQueue
import Crawl
import qualified PoliteQueue    as PQ
import Shared
import Types
import Urls

import Control.Concurrent.STM           (atomically, readTVar, modifyTVar')
import GHC.Conc                         (threadStatus)
import Network.HTTP.Conduit

import qualified Data.ByteString.Char8      as C8
import qualified STMContainers.Set          as S

handleMessages :: Crawler -> Workers -> Message -> IO Message
handleMessages crawler workers (CommandMessage c) = fmap AnswerMessage . handleCommand $ c
    where

    {- Add a URL -}
    handleCommand (AddUrl url) =
        case canonicaliseByteString url of
            Nothing -> return . CouldntAnswer $ "Couldn't canonicalise url: " ++ show url
            Just x -> do
                accepted <- processNextUrl crawler x
                print accepted
                return Confirmation

    {- Remove a URL -}
    handleCommand (RemoveUrl _) = undefined

    {- Set the URL Patterns -}
    handleCommand (SetUrlPatterns ps) = do
        atomically $ do
            let patterns = getUrlPatterns crawler
            existing <- setAsList patterns
            mapM_ (`S.delete` patterns) existing
            mapM_ (`S.insert` patterns) ps
        return Confirmation

    handleCommand (SetNumCrawlers _) = undefined

    {- Tell the crawler to idle -}
    handleCommand Idle = do

        (willIdle, oldStatus) <- atomically $ do
            status <- readTVar (getCrawlerStatus crawler)
            case status of
                 HaltingStatus -> return (False, status)
                 _ -> do
                     modifyTVar' (getCrawlerStatus crawler) (const IdleStatus) 
                     return (True, status)
        if willIdle
            then do
                putStrLn $ "Switching from " ++ show oldStatus ++ " to idle..."
                setNumCrawlers crawler workers 0
                return Confirmation
            else do
                let msg = "Can't start idling from state " ++ show oldStatus
                putStrLn msg
                return $ CouldntAnswer msg

    {- Tell the crawler to halt -}
    handleCommand Halt = do
        
        willHalt <- atomically $ do
            status <- readTVar (getCrawlerStatus crawler)
            case status of
                HaltingStatus -> return False
                _ -> do
                    modifyTVar' (getCrawlerStatus crawler) (const HaltingStatus)
                    return True
        if willHalt
            then do
                putStrLn "Halting..."
                setNumCrawlers crawler workers 0
                return Confirmation
            else do
                let msg = "Can't halt (was already halting)"
                putStrLn msg
                return $ CouldntAnswer msg

handleMessages crawler workers (QuestionMessage q) = AnswerMessage <$> handleQuestion q
    where

    {- Ask the size of the URL Queue -}
    handleQuestion (GetQueueSize queue) =
        case queue of
            UrlQueue -> QueueSize <$> (atomically . PQ.size . getUrlQueue $ crawler)
            StoreQueue -> returnQueueSize . getStoreQueue $ crawler
            ErrorQueue -> returnQueueSize . getLogQueue $ crawler

        where
        returnQueueSize = fmap QueueSize . atomically . size

    {- Ask for the Crawler's current Status -}
    handleQuestion GetCrawlerStatus = CrawlerStatus <$> (atomically . readTVar . getCrawlerStatus $ crawler)

    {- Get the workers' statuses -}
    handleQuestion GetWorkerStatuses = do
        ls <- atomically $ mapAsList (getActiveThreads workers)
        statuses <- mapM (threadStatus . fst) ls
        let ns = zip (map snd ls) statuses
            ss = map (\(n,s) -> n ++ ": " ++ show s) ns
        return $ WorkerStatus ss

    {- Get a report of the shared cookies 
    handleQuestion GetCookieReport = do
        cookies <- atomically . readTVar . getCookieList $ crawler

        let rendered = map (\c -> C8.concat [cookie_name c, ": ", cookie_value c, "<br/>"]) cookies

        return $ CookieReport (map C8.unpack rendered) -}

handleMessages _ _ (AnswerMessage _) = error "Shouldn't have received AnswerMessage"
