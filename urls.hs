{-# LANGUAGE OverloadedStrings #-}

module Urls (CanonicalUrl,
             canonicaliseRequest,
             canonicaliseNetworkUri,
             canonicaliseByteString,
             canonicaliseString,
             isAcceptable,
             contains,
             urlPlus) where

import Types

import Data.ByteString.Char8    (ByteString, pack, unpack, isInfixOf)
import Network.URI              (URI, parseAbsoluteURI, normalizeCase, relativeTo, normalizeEscape, normalizePathSegments)
import Network.HTTP.Client      (Request, getUri)

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

isAcceptable :: CanonicalUrl -> Bool
isAcceptable (CanonicalUrl bs) = True

urlPlus :: URI -> URI -> CanonicalUrl
urlPlus a b = CanonicalUrl (pack . normalize . show . relativeTo b $ a)

contains :: CanonicalUrl -> ByteString -> Bool
contains (CanonicalUrl u) bs = bs `isInfixOf` u 