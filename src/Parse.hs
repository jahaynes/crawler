{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Forms
import Types
import Urls

import Data.Char                (isSpace)
import Data.CaseInsensitive     (mk)
import Data.ByteString.Char8    (ByteString, unpack)
import qualified Data.ByteString.Char8  as C8
import Network.URI              (isURI, parseAbsoluteURI)
import Text.HTML.TagSoup        (Tag (TagOpen), isTagOpenName)
import Text.HTML.TagSoup.Fast   (parseTags)

import Data.Either              (partitionEithers)

parsePage :: [CanonicalUrl] -> ByteString -> ([Loggable], [CanonicalUrl], [Form])
parsePage redirects contents = do

    {- TODO - strictly evalute tags up here
    then pass those tags to getRawHrefs and getForms (simultaneously?) -}

    let onUrl = head redirects

        (loggables, urls) = partitionEithers . getRawHrefs onUrl $ contents

        forms = getForms onUrl contents

    (loggables, urls, forms)

getRawHrefs :: CanonicalUrl -> ByteString -> [Either Loggable CanonicalUrl]
getRawHrefs onUrl bs =
    let tags = filter (\(k,_) -> mk k == mk "href")
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
                            Nothing -> 
                                let errMessage = C8.append "Could not parse URL: " bsUrl
                                in Left (LoggableError onUrl errMessage)
                            Just canonicalised -> Right canonicalised
                    else do
                        let mOnUrl = parseAbsoluteURI (show onUrl)
                            mUrl = parseRelative url
                        case (mOnUrl, mUrl) of
                            (Just ou, Just u) -> Right $ ou `urlPlus` u                      
                            x ->
                                let errMessage = C8.concat ["Couldn't derelativise ", C8.pack . show $ x, "right side was: ", bsUrl]
                                in Left (LoggableError onUrl errMessage)

        where
        trim = C8.reverse . C8.dropWhile isSpace . C8.reverse . C8.dropWhile isSpace
