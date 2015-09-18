{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Urls

import Data.Char                (isSpace)
import Data.ByteString.Char8    (ByteString, unpack)
import qualified Data.ByteString.Char8  as C8
import Network.URI              (isURI, parseAbsoluteURI, parseRelativeReference)
import Text.HTML.TagSoup        (Tag (TagOpen), parseTags, isTagOpenName, canonicalizeTags)

getRawHrefs :: CanonicalUrl -> ByteString -> [Maybe CanonicalUrl]
getRawHrefs onUrl bs =
    let tags = filter (\(k,_) -> k == "href")
             . concatMap (\(TagOpen _ attribs) -> attribs)
             . filter (isTagOpenName "a") 
             . canonicalizeTags
             . parseTags $ bs
    in map (derelativise . snd) tags

    where
    derelativise :: ByteString -> Maybe CanonicalUrl
    derelativise bsUrl =
        let url = unpack . trim $ bsUrl
        in    
        if isURI url
            then canonicaliseByteString bsUrl
            else do
                let mOnUrl = parseAbsoluteURI (show onUrl)
                    mUrl = parseRelativeReference url
                case (mOnUrl, mUrl) of
                    (Just ou, Just u) -> Just $ ou `urlPlus` u                      
                    x -> error $ "Derelativise ERROR " ++ show (onUrl, url) ++ " " ++ show x

        where
        trim = C8.reverse . C8.dropWhile isSpace . C8.reverse . C8.dropWhile isSpace