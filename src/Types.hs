module Types where

import CountedQueue
import Communication

import Control.Monad.Trans.Resource     (ResourceT)
import Data.ByteString.Char8            (ByteString, unpack)
import Data.Conduit                     (ResumableSource)
import Data.Hashable                    (Hashable, hashWithSalt)
import Control.Concurrent               (ThreadId)
import Control.Concurrent.STM           (STM, TVar)
import Control.Concurrent.STM.TQueue    (TQueue)
import ListT                            (toList)
import Network.HTTP.Conduit             (Manager, Cookie, Response)
import Network.HTTP.Types               (Method)
import STMContainers.Map                (Map)
import STMContainers.Set                (Set, stream)

type Crawled = (ThreadId, [CanonicalUrl], ByteString)

data CrawlerState = CrawlerState {
    getCrawlerStatus :: TVar CrawlerStatus,
    getUrlQueue :: PoliteQueue,  
    getStoreQueue :: CountedQueue Crawled,
    getLogQueue :: CountedQueue Loggable,
    getManager :: Manager,
    getCookieList :: TVar [Cookie],
    getUrlPatterns :: Set ByteString,
    getUrlsInProgress :: Set CanonicalUrl,
    getUrlsCompleted :: Set CanonicalUrl,
    getUrlsFailed :: Map CanonicalUrl String
}

data PoliteQueue = PoliteQueue {
                        getSize :: TVar Int,
                        getThreadMapping :: Map ThreadId Domain,
                        getDomainMapping :: Map Domain (TQueue CanonicalUrl),
                        getSlowLane :: TQueue CanonicalUrl
                   }

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

data Loggable = LoggableWarning CanonicalUrl ByteString
              | LoggableError CanonicalUrl ByteString

type Reason = String

data Success = Success | Failure Reason deriving Show

data Form = Form CanonicalUrl Action [Input] deriving Show

data DownloadRequest = GetRequest CanonicalUrl
                     | FormRequest Method CanonicalUrl [(ByteString, ByteString)] deriving Show

type DownloadSource = Response (ResumableSource (ResourceT IO) ByteString)

type DownloadResponse = (ByteString, [Cookie])

data Action = Action Method RelativeUrl deriving Show

newtype Input = Input [(ByteString, ByteString)] deriving Show

newtype RelativeUrl = RelativeUrl ByteString deriving Show

getUrl :: DownloadRequest -> CanonicalUrl
getUrl (GetRequest url) = url
getUrl (FormRequest _ targetUrl _) = targetUrl

setAsList :: Set a -> STM [a]
setAsList = toList . stream
