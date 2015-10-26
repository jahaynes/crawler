module Fetch where

import Urls
import Settings

import Control.Applicative          ((<$>))
import Control.Monad                (when)
import Control.Exception.Lifted     (try)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.ByteString.Char8        (ByteString)
import Data.ByteString.Lazy.Char8   (toStrict)
import Data.Conduit                 (ResumableSource, ($$+-))
import Data.Conduit.Binary          (sinkLbs)
import Data.List                    (group)
import Data.Maybe                   (catMaybes)
import Network.HTTP.Conduit
import Network.HTTP.Types.Status    (statusCode)

requestWithoutRedirects :: CanonicalUrl -> IO Request 
requestWithoutRedirects url = do
    req <- proxySettings <$> parseUrl (show url)
    return $ req {redirectCount=0}

getWithRedirects :: Manager -> CanonicalUrl -> IO (Either String ByteString, [CanonicalUrl])
getWithRedirects man url = do

    req <- requestWithoutRedirects url

    (mLbs, mRedirects) <-
        runResourceT $ do
            (mResponse, mRedirects) <- followRedirects maxRedirects req [canonicaliseRequest req]
            case mResponse of
                Left l -> return (Left l, mRedirects)
                Right response -> do
                    lbs <- responseBody response $$+- sinkLbs
                    return (Right (toStrict lbs), mRedirects)

    -- Include the starting URL as a "redirect", in case a "/" was put on the end
    let redirects = catMaybes mRedirects ++ [url]

    when (length redirects < length mRedirects + 1) (putStrLn "Warning, not all redirects were parsed!")

    return (mLbs, dedupe redirects)

    where
    dedupe :: [CanonicalUrl] -> [CanonicalUrl]
    dedupe = map head . group

    followRedirects :: Int -> 
                       Request ->
                       [Maybe CanonicalUrl] ->
                       (ResourceT IO) (Either String (Response (ResumableSource (ResourceT IO) ByteString)), [Maybe CanonicalUrl])
    followRedirects 0     _ redirs = return (Left "Too many redirects", redirs)
    followRedirects n req redirs = do
        eitherResponse <- try (http req man)
        case eitherResponse of
            Right r -> return (Right r, redirs)
            Left (StatusCodeException status rheaders cj) -> do
                let code = statusCode status
                    mRedirReq = getRedirectedRequest req rheaders cj code
                case mRedirReq of
                    Just redirReq -> followRedirects (n-1) redirReq (canonicaliseRequest redirReq:redirs)
                    Nothing -> return (Left ("Code: " ++ show code), redirs)
            Left e -> return (Left (show e), redirs)
