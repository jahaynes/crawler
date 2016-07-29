module Types where

import CountedQueue
import Communication

import Control.Monad.Trans.Class        (lift)
import Control.Monad.Trans.Either       (EitherT, runEitherT, left)
import Control.Monad.Trans.Resource     (ResourceT, runResourceT)
import Data.ByteString.Char8            (ByteString, unpack)
import Data.Conduit                     (ResumableSource)
import Data.Hashable                    (Hashable, hashWithSalt)

import qualified Data.Map       as M

import Control.Concurrent               (ThreadId)
import Control.Concurrent.STM           (STM, TVar)
import Control.Concurrent.STM.TQueue    (TQueue)
import Data.Time                        (UTCTime)
import ListT                            (toList)
import Network.HTTP.Conduit             (Manager, Cookie, Response)
import Network.HTTP.Types               (Method)
import STMContainers.Map                (Map)
import STMContainers.Set                (Set, stream)

data CrawledDocument = CrawledDocument
                     { getRedirectChain :: [CanonicalUrl]
                     , getContent :: ByteString
                     , getThreadId :: ThreadId
                     } deriving Show

data Crawler = Crawler {
    getCrawlerStatus :: TVar CrawlerStatus,
    getUrlQueue :: PoliteQueue,  
    getStoreQueue :: CountedQueue CrawledDocument,
    getLogQueue :: CountedQueue Loggable,
    getManager :: Manager,
    getCookieList :: TVar [Cookie],
    getUrlPatterns :: Set ByteString,
    getUrlsInProgress :: Set CanonicalUrl,
    getUrlsCompleted :: Set CanonicalUrl,
    getUrlsFailed :: Map CanonicalUrl String,
    getCrawlerSettings :: CrawlerSettings
}

data CrawlerSettings = CrawlerSettings {
    getFormInstructions :: TVar SuppliedFormActions,
    getHrefDirections :: TVar [HrefDirection],
    getProxySettings :: TVar (Maybe ProxySettings)
}

data PoliteQueue = PoliteQueue {
                        getSize :: TVar Int,
                        getThreadMapping :: Map ThreadId Domain,
                        getDomainMapping :: Map Domain (TQueue CanonicalUrl),
                        getSlowLane :: TQueue CanonicalUrl
                   }

data Workers = Workers {
    getCrawlerThreads :: Set ThreadId,
    getCrawlerThreadsToStop :: Set ThreadId,

    getActiveThreads :: Map ThreadId String,
    getThreadClocks :: Map ThreadId (UTCTime, CanonicalUrl)
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
              | LoggableError CanonicalUrl ByteString deriving Show

type Reason = ByteString

data Success = Success | Failure Reason deriving Show

data Form = Form CanonicalUrl Action [FormParameters] deriving Show
newtype Label = Label ByteString deriving (Show, Eq, Ord)
type FormKey = ByteString
type FormValue = ByteString

data DownloadRequest = GetRequest CanonicalUrl
                     | FormRequest Label Method CanonicalUrl FormParameters deriving Show

type DownloadSource = Response (ResumableSource WebIO ByteString)

data DownloadResult = DownloadResult !ByteString ![Cookie] ![CanonicalUrl]

newtype UrlRegex = UrlRegex ByteString deriving Show
newtype FormActionRegex = FormActionRegex ByteString deriving Show
newtype FormParameters = FormParameters [(FormKey, FormValue)] deriving Show

newtype SuppliedFormActions = SuppliedFormActions (M.Map Label (UrlRegex, FormActionRegex, FormParameters)) deriving Show

data ApplicableSuppliedFormActions = ApplicableSuppliedFormActions Label FormParameters deriving Show

data CombinedFormActions = CombinedFormActions Label FormParameters

data Action = Action Method RelativeUrl deriving Show

newtype RelativeUrl = RelativeUrl ByteString deriving Show

data HrefDirection = HrefDirection Label UrlRegex HrefRegex deriving Show
newtype HrefRegex = HrefRegex ByteString deriving Show

getUrl :: DownloadRequest -> CanonicalUrl
getUrl (GetRequest url) = url
getUrl (FormRequest _ _ targetUrl _) = targetUrl

setAsList :: Set a -> STM [a]
setAsList = toList . stream

{- Custom Monad Transformer Stack
   which implements Resource and Either -}
type WebIO = ResourceT (EitherT String IO)

runWebIO :: ResourceT (EitherT e IO) a -> IO (Either e a)
runWebIO = runEitherT . runResourceT

webErr :: String -> ResourceT (EitherT String IO) a
webErr = lift . left

data ProxySettings = ProxySettings ByteString Int

class FromCrawledDocument a where
    fromCrawledDocument :: CrawledDocument -> a
    toStorableDocument :: a -> ByteString