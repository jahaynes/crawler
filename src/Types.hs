module Types where

import Communication

import Data.ByteString.Char8            (ByteString, unpack)
import Data.Hashable                    (Hashable, hashWithSalt)

import qualified Data.Map       as M

import Control.Concurrent               (ThreadId)
import Control.Concurrent.STM           (TVar)
import Control.Concurrent.STM.TQueue    (TQueue)
import Data.Time                        (UTCTime)
import Network.HTTP.Conduit             (Manager, Cookie)
import StmContainers.Map                (Map)
import StmContainers.Set                (Set)

data CrawledDocument = CrawledDocument
                     { getRedirectChain :: [CanonicalUrl]
                     , getContent       :: ByteString
                     , getThreadId      :: ThreadId
                     }

data Crawler bq = Crawler {
    getCrawlerStatus         :: TVar CrawlerStatus,
    getUrlQueue              :: PoliteQueue,
    getStoreQueue            :: bq CrawledDocument,
    getNumStored             :: TVar Int,
    getLogQueue              :: bq Loggable,
    getManager               :: Manager,
    getCookieList            :: TVar [Cookie],
    getUrlIncludePatterns    :: Set ByteString,            -- TODO functionalise (ByteString -> Bool)
    getDomainIncludePatterns :: Set ByteString,
    getUrlsInProgress        :: Set CanonicalUrl,
    getUrlsCompleted         :: Set CanonicalUrl,
    getUrlsFailed            :: Map CanonicalUrl String,
    getCrawlerSettings       :: CrawlerSettings
}

data CrawlerSettings = CrawlerSettings {
    getCrawlOutputType  :: TVar (Maybe Output),
    getFormInstructions :: TVar SuppliedFormActions,
    getHrefDirections   :: TVar [HrefDirection],
    getProxySettings    :: TVar (Maybe ProxySettings),
    getCrawlLimit       :: TVar (Maybe Int)
}

data PoliteQueue = PoliteQueue {
                        getSize :: TVar Int,
                        getThreadMapping :: Map ThreadId Domain,
                        getDomainMapping :: Map Domain (TQueue CanonicalUrl),
                        getSlowLane :: TQueue CanonicalUrl
                   }

data Workers = Workers {
    getCrawlerThreads       :: Set ThreadId,
    getCrawlerThreadsToStop :: Set ThreadId,
    getActiveThreads        :: Map ThreadId String,
    getThreadClocks         :: Map ThreadId (UTCTime, CanonicalUrl)
}

newtype Output = WarcFile FilePath

newtype CanonicalUrl = CanonicalUrl ByteString deriving Ord

newtype Domain = Domain ByteString deriving Eq

instance Hashable Domain where
    hashWithSalt salt (Domain d) = hashWithSalt salt d

instance Eq CanonicalUrl where
    (CanonicalUrl a) == (CanonicalUrl b) = a == b

instance Hashable CanonicalUrl where
    hashWithSalt salt (CanonicalUrl bs) = hashWithSalt salt bs

instance Show CanonicalUrl where
    show (CanonicalUrl bs) = unpack bs

data Loggable = CrawlWarning CanonicalUrl ByteString
              | CrawlError CanonicalUrl ByteString
              | GeneralMessage ByteString
              | GeneralError ByteString
                  deriving Show

newtype Label = Label ByteString deriving (Show, Eq, Ord)
type FormKey = ByteString
type FormValue = ByteString

newtype UrlRegex = UrlRegex ByteString deriving Show
newtype FormActionRegex = FormActionRegex ByteString deriving Show
newtype FormParameters = FormParameters [(FormKey, FormValue)] deriving Show

newtype SuppliedFormActions =
    SuppliedFormActions (M.Map Label (UrlRegex, FormActionRegex, FormParameters)) deriving Show

newtype RelativeUrl = RelativeUrl ByteString deriving Show

data HrefDirection = HrefDirection Label UrlRegex HrefRegex deriving Show
newtype HrefRegex = HrefRegex ByteString deriving Show



data ProxySettings = ProxySettings ByteString Int

class FromCrawledDocument a where
    fromCrawledDocument :: CrawledDocument -> a
    toStorableDocument :: a -> ByteString
