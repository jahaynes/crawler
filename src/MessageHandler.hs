module MessageHandler where

import Communication
import CountedQueue
import Crawl
import Parse
import Types
import Urls
import Workers

import Control.Concurrent.STM           (atomically, readTVar, modifyTVar')

import qualified STMContainers.Set as S

handleMessages :: CrawlerState -> Workers -> Message -> IO Message
handleMessages crawlerState workers (CommandMessage c) = handleCommand c >>= return . AnswerMessage
    where

    {- Add a URL -}
    handleCommand (AddUrl url) =
        case canonicaliseByteString url of
            Nothing -> return . Failure $ "Couldn't canonicalise url: " ++ show url
            Just x -> do
                processNextUrl crawlerState x
                putStrLn "Added url to frontier"
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

    handleCommand (SetNumParsers _) = undefined

    {- Tell the crawler to idle -}
    handleCommand (Idle) = do

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
    handleCommand (Halt) = do
        
        willHalt <- atomically $ do
            status <- readTVar (getCrawlerStatus crawlerState)
            case status of
                HaltingStatus -> return False
                _ -> do
                    modifyTVar' (getCrawlerStatus crawlerState) (const HaltingStatus)
                    return True
        if willHalt
            then do
                putStrLn $ "Halting..."
                setNumCrawlers crawlerState workers 0
                setNumParsers crawlerState workers 0
                return Confirmation
            else do
                let msg = "Can't halt (was already halting)"
                putStrLn msg
                return $ Failure msg

handleMessages crawlerState _ (QuestionMessage q) = handleQuestion q >>= return . AnswerMessage
    where

    {- Ask the size of the URL Queue -}
    handleQuestion (GetQueueSize queue) =
        case queue of
            UrlQueue -> return . QueueSize =<< (atomically . size . getUrlQueue $ crawlerState)
            ParseQueue -> return . QueueSize =<< (atomically . size . getParseQueue $ crawlerState)
            StoreQueue -> return . QueueSize =<< (atomically . size . getStoreQueue $ crawlerState)
            ErrorQueue -> return . QueueSize =<< (atomically . size . getLogQueue $ crawlerState)
        
    {- Ask for the Crawler's current Status -}
    handleQuestion (GetCrawlerStatus) = do
        status <- atomically . readTVar $ getCrawlerStatus crawlerState
        return $ Status status
        
handleMessages _ _ (AnswerMessage _) = error "Shouldn't have received AnswerMessage"