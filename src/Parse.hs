{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Forms
import Shared (both)
import Types
import Urls

import Data.Char                (isSpace, toLower)
import Data.CaseInsensitive     (mk)
import Data.ByteString.Char8    (ByteString)
import Data.List                (find)
import Data.ByteString.Search   (breakAfter)
import qualified Data.ByteString.Char8  as C8
import Safe
import Text.HTML.TagSoup        (Tag (TagOpen), isTagOpenName, isTagCloseName, fromAttrib, canonicalizeTags)

import Data.Either              (partitionEithers)

findPageRedirect :: [Tag ByteString] -> Maybe CanonicalUrl
findPageRedirect tags = do
    meta <- getMeta tags
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

parsePage :: [CanonicalUrl] -> [Tag ByteString] -> ([Loggable], [CanonicalUrl], [Form])
parsePage redirects tags = do
    let onUrl = head redirects
        (loggables, urls) = partitionEithers . getRawHrefs onUrl $ tags
        forms = getForms onUrl tags
    (loggables, urls, forms)

getRawHrefs :: CanonicalUrl -> [Tag ByteString] -> [Either Loggable CanonicalUrl]
getRawHrefs onUrl = map (derelativise onUrl . trim . snd)
                  . filter (\(k,_) -> mk k == mk "href")
                  . concatMap (\(TagOpen _ attribs) -> attribs)
                  . filter (isTagOpenName "a") 

    where
    trim :: ByteString -> ByteString
    trim = C8.reverse . C8.dropWhile isSpace . C8.reverse . C8.dropWhile isSpace
