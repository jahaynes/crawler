{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Forms
import Types
import Urls

import Data.Char                (isSpace)
import Data.CaseInsensitive     (mk)
import Data.List                (find)
import Data.ByteString.Char8    (ByteString, unpack)
import qualified Data.ByteString.Char8  as C8
import Data.ByteString.Search   (breakAfter)
import Network.URI              (isURI, parseAbsoluteURI)
import Safe                     (headMay)
import Text.HTML.TagSoup        (Tag (TagOpen), canonicalizeTags, isTagOpenName, isTagCloseName, fromAttrib)
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

findPageRedirect :: ByteString -> Maybe CanonicalUrl
findPageRedirect rawPage = do
    meta <- getMeta rawPage
    let content = fromAttrib "content" meta
    urlSection <- find (C8.isPrefixOf "url") . C8.splitWith (`elem` " ;") $ content
    let url = C8.takeWhile (not . isSpace) . snd . breakAfter "=" $ urlSection
    canonicaliseByteString url

    where
    getMeta = headMay
            . filter (\(TagOpen _ attrs) -> elem ("http-equiv","refresh") attrs)
            . filter (isTagOpenName "meta")
            . takeWhile (not . isTagCloseName "head")
            . dropWhile (not . isTagOpenName "head")
            . canonicalizeTags
            . parseTags

getRawHrefs :: CanonicalUrl -> ByteString -> [Either Loggable CanonicalUrl]
getRawHrefs onUrl bs =
    let tags = filter (\(k,_) -> mk k == mk "href")
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
