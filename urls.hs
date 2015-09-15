{-# LANGUAGE OverloadedStrings #-}

module Urls (CanonicalUrl,
             canonicaliseRequest,
             canonicaliseNetworkUri,
             canonicaliseByteString,
             canonicaliseString,
             isAcceptable,
             urlPlus) where

import Data.ByteString.Char8    (ByteString, pack, unpack, isInfixOf)
import Data.Char                (toLower)
import Network.URI              (URI, parseAbsoluteURI, normalizeCase, relativeTo, normalizeEscape, normalizePathSegments)
import Network.HTTP.Client      (Request, getUri)
import qualified Data.ByteString.Char8  as C8

import Data.Hashable

newtype CanonicalUrl = CanonicalUrl ByteString

instance Eq CanonicalUrl where
    (CanonicalUrl a) == (CanonicalUrl b) = a == b

instance Hashable CanonicalUrl where
    hashWithSalt salt (CanonicalUrl bs) = hashWithSalt salt bs

instance Show CanonicalUrl where
    show (CanonicalUrl bs) = unpack bs

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
