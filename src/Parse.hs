{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Forms
import Shared (both)
import Types
import Urls

import Data.Char                (isSpace, toLower)
import Data.CaseInsensitive     (mk)
import Data.ByteString.Char8    (ByteString, unpack)
import Data.List                (find)
import Data.Maybe               (fromJust)
import Data.ByteString.Search   (breakAfter)
import qualified Data.ByteString.Char8  as C8
import Network.URI              (isURI, parseAbsoluteURI)
import Safe
import Text.HTML.TagSoup        (Tag (TagOpen), isTagOpenName, isTagCloseName, fromAttrib, canonicalizeTags)
import Text.HTML.TagSoup.Fast   (parseTags)

import Data.Either              (partitionEithers)

findPageRedirect :: ByteString -> Maybe CanonicalUrl
findPageRedirect rawPage = do
    meta <- getMeta rawPage
    let content = fromAttrib "content" meta
    urlSection <- find (C8.isPrefixOf "url") . C8.splitWith (`elem` [' ',';']) $ content
    let url = C8.takeWhile (not . isSpace) . snd . breakAfter "=" $ urlSection
    canonicaliseByteString url

    where
    getMeta = headMay
            . filter (\(TagOpen _ attrs) -> elem ("http-equiv","refresh") (map (both (C8.map toLower)) attrs))
            . filter (isTagOpenName "meta")
            . takeWhile (not . isTagCloseName "head")
            . dropWhile (not . isTagOpenName "head")
            . canonicalizeTags
            . parseTags

parsePage :: [CanonicalUrl] -> ByteString -> ([Loggable], [CanonicalUrl], [Form])
parsePage redirects contents = do

    {- TODO - strictly evalute tags up here
    then pass those tags to getRawHrefs and getForms (simultaneously?) -}

    let onUrl = head redirects

        (loggables, urls) = partitionEithers . getRawHrefs onUrl $ contents

        forms = getForms onUrl contents

    (loggables, urls, forms)

getRawHrefs :: CanonicalUrl -> ByteString -> [Either Loggable CanonicalUrl]
getRawHrefs onUrl = map (derelativise onUrl . trim . snd)
                  . filter (\(k,_) -> mk k == mk "href")
                  . concatMap (\(TagOpen _ attribs) -> attribs)
                  . filter (isTagOpenName "a") 
                  . parseTags

    where
    trim :: ByteString -> ByteString
    trim = C8.reverse . C8.dropWhile isSpace . C8.reverse . C8.dropWhile isSpace
