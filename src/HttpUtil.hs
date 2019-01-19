module HttpUtil where

import Prelude hiding (fail)

import Control.Monad.Fail    (MonadFail, fail)
import Data.ByteString.Char8 (unpack)
import Data.CaseInsensitive  (mk)
import Safe                  (headMay, readMay)
import Network.HTTP.Conduit  (Response, responseHeaders)
import Network.HTTP.Types    (hContentLength)

getContentLength :: Response a -> Maybe Int
getContentLength resp = do
    strContentLength <- headMay
                      . map    (\(_,v) -> v)
                      . filter (\(k,_) -> mk k == mk hContentLength)
                      $ responseHeaders resp
    readMay . unpack $ strContentLength

checkSize :: MonadFail m => Int -> Response a -> m ()
checkSize maxContentLength res =
    case getContentLength res of
        Just x | x <= maxContentLength -> return ()
               | otherwise             -> fail "TODO - Too big"
        Nothing -> return ()
