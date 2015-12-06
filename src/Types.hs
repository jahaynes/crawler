module Types where

import CountedQueue
import Communication

import Data.ByteString.Char8 
import Data.Hashable
import Control.Concurrent.STM           (STM, TVar)
import qualified STMContainers.Set as S
import qualified STMContainers.Map as M
import ListT                            (toList)
import Network.HTTP.Conduit (Cookie)
import Network.HTTP.Types (Method)

type Crawled = ([CanonicalUrl], ByteString)

data CrawlerState = CrawlerState {
    getCrawlerStatus :: TVar CrawlerStatus,
    getUrlQueue :: CountedQueue CanonicalUrl,  
    getStoreQueue :: CountedQueue Crawled,
    getLogQueue :: CountedQueue Loggable,
    getCookieList :: TVar [Cookie],
    getUrlPatterns :: S.Set ByteString,
    getUrlsInProgress :: S.Set CanonicalUrl,
    getUrlsCompleted :: S.Set CanonicalUrl,
    getUrlsFailed :: M.Map CanonicalUrl String
}

newtype CanonicalUrl = CanonicalUrl ByteString deriving Ord

instance Eq CanonicalUrl where
    (CanonicalUrl a) == (CanonicalUrl b) = a == b

instance Hashable CanonicalUrl where
    hashWithSalt salt (CanonicalUrl bs) = hashWithSalt salt bs

instance Show CanonicalUrl where
    show (CanonicalUrl bs) = unpack bs

data Loggable = LoggableWarning CanonicalUrl ByteString
              | LoggableError CanonicalUrl ByteString

type Reason = String

data Accepted = Accepted | NotAccepted Reason deriving Show

data Form = Form Action [Input] deriving Show

data Action = Action Method RelativeUrl deriving Show

data Input = Input [(ByteString, ByteString)] deriving Show

newtype RelativeUrl = RelativeUrl ByteString deriving Show

setAsList :: S.Set a -> STM [a]
setAsList = toList . S.stream
