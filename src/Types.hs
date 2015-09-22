module Types where

import CountedQueue

import Data.ByteString.Char8 
import Data.Hashable
import qualified STMContainers.Set as S
import qualified STMContainers.Map as M

type Crawled = ([CanonicalUrl], ByteString)

data CrawlerState = CrawlerState {
    getUrlQueue :: CountedQueue CanonicalUrl,  
    getParseQueue :: CountedQueue Crawled,
    getStoreQueue :: CountedQueue Crawled,
    getLogQueue :: CountedQueue Loggable,
    getUrlsInProgress :: S.Set CanonicalUrl,
    getUrlsCompleted :: S.Set CanonicalUrl,
    getUrlsFailed :: M.Map CanonicalUrl String
}

newtype CanonicalUrl = CanonicalUrl ByteString

instance Eq CanonicalUrl where
    (CanonicalUrl a) == (CanonicalUrl b) = a == b

instance Hashable CanonicalUrl where
    hashWithSalt salt (CanonicalUrl bs) = hashWithSalt salt bs

instance Show CanonicalUrl where
    show (CanonicalUrl bs) = unpack bs

data Loggable = LoggableWarning CanonicalUrl ByteString
              | LoggableError CanonicalUrl ByteString