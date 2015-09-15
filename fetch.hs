module Fetch where

import Urls
import Settings

import Control.Applicative          ((<$>))
import Control.Monad                (when)
import Control.Exception.Lifted     (try)
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString.Char8        (ByteString)
import Data.ByteString.Lazy.Char8   (toStrict)
import Data.Conduit                 (($$+-))
import Data.Conduit.Binary          (sinkLbs)
import Data.Maybe                   (catMaybes)
import Network.HTTP.Conduit
import Network.HTTP.Types.Status    (statusCode)

getWithRedirects :: Manager -> CanonicalUrl -> IO (Maybe ByteString, [CanonicalUrl])
getWithRedirects man url = do

    req_ <- proxySettings <$> parseUrl (show url)
    
    let req = req_ {redirectCount=0}
    
    (mLbs, mRedirects) <-
        runResourceT $ do
            (mResponse, mRedirects) <- followRedirects maxRedirects req [canonicaliseRequest req]
            case mResponse of
                Nothing -> return (Nothing, mRedirects)
                Just response -> do
                    lbs <- responseBody response $$+- sinkLbs
                    return (Just $ toStrict lbs, mRedirects)
                
    -- Include the starting URL as a "redirect", in case a "/" was put on the end
    let redirects = catMaybes mRedirects ++ [url]

    when (length redirects < length mRedirects + 1) (putStrLn "Warning, not all redirects were parsed!")

    return (mLbs, redirects)

    where
    followRedirects 0     _ redirs = return (Nothing, redirs)
    followRedirects n req redirs = do
        eitherResponse <- try (http req man)
        case eitherResponse of
            Right r -> return (Just r, redirs)
            Left (StatusCodeException status rheaders cj) -> do
                let code = statusCode status
                    mRedirReq = getRedirectedRequest req rheaders cj code
                case mRedirReq of
                    Just redirReq ->
                        followRedirects (n-1) redirReq (canonicaliseRequest redirReq:redirs)
                    Nothing -> return (Nothing, redirs)
            _ -> return (Nothing, redirs)
