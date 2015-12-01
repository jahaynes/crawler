{-# LANGUAGE OverloadedStrings #-}

module Parse where

import CountedQueue
import Crawl                    (processNextUrl)
import Shared
import Types
import Urls
import Workers

import Data.Char                (isSpace)
import Data.ByteString.Char8    (ByteString, unpack)
import qualified Data.ByteString.Char8  as C8
import Network.URI              (isURI, parseAbsoluteURI, parseRelativeReference)
import Text.HTML.TagSoup        (Tag (TagOpen), isTagOpenName, canonicalizeTags)
import Text.HTML.TagSoup.Fast   (parseTags)

import Control.Applicative              ((<$>), (<*>))
import Control.Concurrent               (ThreadId, myThreadId)
import Control.Concurrent.STM           (STM, atomically)
import Control.Monad                    (replicateM_)
import Data.Either                      (partitionEithers)
import qualified STMContainers.Set as S

setNumParsers :: CrawlerState -> Workers -> Int -> IO ()
setNumParsers crawlerState workers desiredNum = do

    threadsToAdd <- atomically $ do
        currentNumParsers <- getActiveParserCount
        case desiredNum - currentNumParsers of
            0 -> return 0
            threadDelta ->
                if threadDelta > 0
                    then return threadDelta
                    else do
                        threadsToStop <- takeSet (-threadDelta) (getParserThreads workers)
                        mapM_ (\t -> S.insert t (getParserThreadsToStop workers)) threadsToStop
                        return 0
    replicateM_ threadsToAdd addParser

    where
    addParser :: IO ()
    addParser = forkIO_ $ do
        threadId <- myThreadId
        atomically $ S.insert threadId (getParserThreads workers)
        parsePages workers crawlerState threadId

    getActiveParserCount :: STM Int
    getActiveParserCount = (-) <$> sizeOfSet (getParserThreads workers) <*> sizeOfSet (getParserThreadsToStop workers)

parsePages :: Workers -> CrawlerState -> ThreadId -> IO ()
parsePages workers crawlerState threadId =

    whileActive threadId (getParserThreads workers) (getParserThreadsToStop workers) $ do

        (redirects, dat) <- atomically $ readQueue (getParseQueue crawlerState)
        let referenceUrl = head redirects 
            (hrefErrors, nextHrefs) = partitionEithers . getRawHrefs referenceUrl $ dat
        mapM_ (atomically . writeQueue (getLogQueue crawlerState)) hrefErrors
        mapM_ (processNextUrl crawlerState) nextHrefs

getRawHrefs :: CanonicalUrl -> ByteString -> [Either Loggable CanonicalUrl]
getRawHrefs onUrl bs =
    let tags = filter (\(k,_) -> k == "href")
             . concatMap (\(TagOpen _ attribs) -> attribs)
             . filter (isTagOpenName "a") 
             . parseTags $ bs
    in map (derelativise . snd) tags

    where
    derelativise :: ByteString -> Either Loggable CanonicalUrl
    derelativise bsUrl = do
        let url = unpack . trim $ bsUrl
        
        if "mailto:" `C8.isPrefixOf` bsUrl
            then Left $ LoggableWarning onUrl $ C8.append "Found an email " bsUrl
            else
                if isURI url
                    then
                        case canonicaliseByteString bsUrl of
                            Nothing -> Left $ LoggableError onUrl $ C8.append "Could not parse URL: " bsUrl
                            Just canonicalised -> Right canonicalised
                    else do
                        let mOnUrl = parseAbsoluteURI (show onUrl)
                            mUrl = parseRelativeReference url
                        case (mOnUrl, mUrl) of
                            (Just ou, Just u) -> Right $ ou `urlPlus` u                      
                            x -> Left . LoggableError onUrl . C8.append "Couldn't derelativise " . C8.pack . show $ x

        where
        trim = C8.reverse . C8.dropWhile isSpace . C8.reverse . C8.dropWhile isSpace
