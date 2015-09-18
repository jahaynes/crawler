module Types where

import Control.Concurrent.STM.TBQueue   (TBQueue)
import Control.Concurrent.STM.TQueue    (TQueue)

import Data.ByteString.Char8 
import Data.Hashable
import qualified STMContainers.Set as S
import qualified STMContainers.Map as M

type Crawled = ([CanonicalUrl], ByteString)

data CrawlerState = CrawlerState {
    getUrlQueue :: TQueue CanonicalUrl,  
    getParseQueue :: TQueue Crawled,
    getStoreQueue :: TBQueue Crawled,
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