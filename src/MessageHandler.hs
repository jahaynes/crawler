module MessageHandler where

import CountedQueue
import Types

import Communication
import Crawl
import Urls

import Control.Concurrent.STM           (atomically, readTVar, modifyTVar')

import qualified STMContainers.Set as S

handleMessages :: CrawlerState -> Message -> IO Message
handleMessages crawlerState (CommandMessage c) = handleCommand c >>= return . AnswerMessage
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
                return Confirmation
            else do
                let msg = "Can't start idling from state " ++ show oldStatus
                putStrLn msg
                return $ Failure msg

        --RunningStatus | IdleStatus | HaltingStatus

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
                return Confirmation
            else do
                let msg = "Can't halt (was already halting)"
                putStrLn msg
                return $ Failure msg

handleMessages crawlerState (QuestionMessage q) = handleQuestion q >>= return . AnswerMessage
    where

    {- Ask the size of the URL Queue -}
    handleQuestion (GetQueueSize queue) =
        case queue of
            UrlQueue -> do
                i <- atomically . size . getUrlQueue $ crawlerState
                return . QueueSize $ i
            ParseQueue -> undefined
            StoreQueue -> undefined
            ErrorQueue -> undefined
        
handleMessages _ (AnswerMessage _) = error "Shouldn't have received AnswerMessage"