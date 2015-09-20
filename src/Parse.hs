{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Crawl
import Types
import Urls

import Data.Char                (isSpace)
import Data.ByteString.Char8    (ByteString, unpack)
import qualified Data.ByteString.Char8  as C8
import Network.URI              (isURI, parseAbsoluteURI, parseRelativeReference)
import Text.HTML.TagSoup        (Tag (TagOpen), parseTags, isTagOpenName, canonicalizeTags)

import Control.Concurrent.STM.TBQueue
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM           (atomically)
import Control.Monad                    (forever)
import Data.Either                      (partitionEithers)

parsePages :: CrawlerState -> IO ()
parsePages crawlerState = forever $ do

    (redirects, dat) <- atomically $ readTQueue (getParseQueue crawlerState)
    let referenceUrl = head redirects 
        (hrefErrors, nextHrefs) = partitionEithers . getRawHrefs referenceUrl $ dat
    mapM_ (atomically . writeTBQueue (getLogQueue crawlerState)) hrefErrors
    mapM_ (processNextUrl crawlerState) nextHrefs

getRawHrefs :: CanonicalUrl -> ByteString -> [Either Loggable CanonicalUrl]
getRawHrefs onUrl bs =
    let tags = filter (\(k,_) -> k == "href")
             . concatMap (\(TagOpen _ attribs) -> attribs)
             . filter (isTagOpenName "a") 
             . canonicalizeTags
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