module DownloadRequest where

import Types

import Data.CaseInsensitive (mk)
import Network.HTTP.Conduit (Request, urlEncodedBody, setQueryString)
import Network.HTTP.Types   (Method, methodPost)

class DownloadRequest r where

    getUrl :: r -> CanonicalUrl

    applyParametersFrom :: r -> (Request -> Request)

newtype GetRequest = GetRequest CanonicalUrl

instance DownloadRequest GetRequest where

    getUrl (GetRequest url) = url

    applyParametersFrom _ = id

data FormRequest = FormRequest Label Method CanonicalUrl FormParameters

instance DownloadRequest FormRequest where

    getUrl (FormRequest _ _ targetUrl _) = targetUrl

    applyParametersFrom (FormRequest _ formMethod _ (FormParameters params))
        | mk formMethod == mk methodPost = urlEncodedBody params
        | otherwise                      = setQueryString (map (\(k,v) -> (k,Just v)) params)
