module DownloadRequest where

import Types

import Network.HTTP.Types (Method)

data DownloadRequest = GetRequest CanonicalUrl
                     | FormRequest Label Method CanonicalUrl FormParameters

getUrl :: DownloadRequest -> CanonicalUrl
getUrl (GetRequest url) = url
getUrl (FormRequest _ _ targetUrl _) = targetUrl
