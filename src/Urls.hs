{-# LANGUAGE OverloadedStrings #-}

module Urls where

import Types

import Data.ByteString.Char8  as C8 (ByteString, pack, unpack, isInfixOf, isPrefixOf)
import Data.ByteString.Search       (breakOn, breakAfter)
import Data.List.Split              (splitWhen)
import Network.URI
import Network.HTTP.Client          (Request, getUri)

canonicaliseRequest :: Request -> Maybe CanonicalUrl
canonicaliseRequest = canonicaliseNetworkUri . getUri

canonicaliseNetworkUri :: URI -> Maybe CanonicalUrl
canonicaliseNetworkUri = canonicaliseString . show

canonicaliseByteString :: ByteString -> Maybe CanonicalUrl
canonicaliseByteString = canonicaliseString . unpack

canonicaliseString :: String -> Maybe CanonicalUrl
canonicaliseString str =
    case parseAbsoluteURI str of
        Just x -> Just (CanonicalUrl (pack . normalize . show $ (x :: URI)))
        Nothing -> Nothing

normalize :: String -> String
normalize = normalizeCase . normalizeEscape . normalizePathSegments

urlPlus :: URI -> URI -> CanonicalUrl
urlPlus a b = CanonicalUrl (pack . normalize . show . relativeTo b $ a)

contains :: CanonicalUrl -> ByteString -> Bool
contains (CanonicalUrl u) bs = bs `isInfixOf` u

startsWith :: CanonicalUrl -> ByteString -> Bool
startsWith (CanonicalUrl u) bs = bs `isPrefixOf` u

parseRelative :: String -> Maybe URI
parseRelative relative =
    case stripQueryParams relative of
         (rel, Nothing) -> parseRelativeReference rel
         (rel, Just query) -> case parseRelativeReference rel of
                                  Just x -> Just (x {uriQuery = query})
                                  Nothing -> Nothing
    where
    stripQueryParams :: String -> (String, Maybe String)
    stripQueryParams url
        | '?' `elem` url = do
            case splitWhen (\a -> a == '?' || a == '#') url of
                [r,q,f] -> (concat [r,"#",f], Just ('?':q))
                [r,q] -> (r, Nothing)
                _ -> (url, Nothing)
        | otherwise = (url, Nothing)

getDomain :: CanonicalUrl -> Domain
getDomain (CanonicalUrl u) = Domain . fst . breakOn "/" . snd . breakAfter "//" $ u