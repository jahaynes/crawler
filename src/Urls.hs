{-# LANGUAGE OverloadedStrings #-}

module Urls where

import Settings
import Types

import Control.Applicative          ((<$>))
import Data.ByteString.Char8  as C8 (ByteString, append, concat, pack, unpack, isInfixOf, isPrefixOf, null)
import Data.ByteString.Search       (breakOn, breakAfter)
import Data.List.Split              (splitWhen)
import Network.URI
import Network.HTTP.Client          (Request, getUri)

canonicaliseRequest :: Request -> Maybe CanonicalUrl
canonicaliseRequest = canonicaliseNetworkUri . getUri

canonicaliseNetworkUri :: URI -> Maybe CanonicalUrl
canonicaliseNetworkUri = canonicaliseString . show . stripPort

    where
    stripPort :: URI -> URI
    stripPort uri = uri { uriAuthority = stripPort' (uriScheme uri) <$> uriAuthority uri }
        where
        stripPort' :: String -> URIAuth -> URIAuth
        stripPort' "http:" auth = auth {uriPort = if uriPort auth == ":80" then "" else uriPort auth}
        stripPort' "https:" auth = auth {uriPort = if uriPort auth == ":443" then "" else uriPort auth}
        stripPort' _ auth = auth

canonicaliseByteString :: ByteString -> Maybe CanonicalUrl
canonicaliseByteString = canonicaliseString . unpack

canonicaliseString :: String -> Maybe CanonicalUrl
canonicaliseString str =
    case parseAbsoluteURI (discard str) of
        Just x -> Just (CanonicalUrl (pack . normalize . show $ (x :: URI)))
        Nothing -> Nothing

normalize :: String -> String
normalize = normalizeCase . normalizeEscape . normalizePathSegments

urlPlus :: URI -> URI -> CanonicalUrl
urlPlus a b = CanonicalUrl (pack . discard . normalize . show . relativeTo b $ a)

discard :: String -> String
discard str | discardFragments = takeWhile (/= '#') str
            | otherwise = str

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
                [r,q,f] -> (Prelude.concat [r,"#",f], Just ('?':q))
                [r,q] -> (r, Nothing)
                _ -> (url, Nothing)
        | otherwise = (url, Nothing)

derelativise :: CanonicalUrl -> ByteString -> Either Loggable CanonicalUrl
derelativise onUrl bsUrl = do
    let url = unpack bsUrl

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

getDomain :: CanonicalUrl -> Maybe Domain
getDomain (CanonicalUrl u) =
    let d = fst . breakOn "/" . snd . breakAfter "//" $ u
    in if C8.null d then Nothing else Just (Domain d)