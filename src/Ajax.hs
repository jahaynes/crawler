module Ajax where

import FrontEndTypes
import Fay.Text (Text)

import FFI

data Method = GET | POST

ajax :: Method -> Url -> Text -> (Text -> Fay ()) -> Fay ()
ajax method url content f = do
    req <- xmlHttpRequest
    case method of
        GET -> openGet req url
        POST -> openPost req url
    setReadyHandler req $ do
        done <- isDone req
        when done $ responseText req >>= f
    send req content

    where
    xmlHttpRequest :: Fay XMLHttpRequest
    xmlHttpRequest = ffi "new XMLHttpRequest()"

    openGet :: XMLHttpRequest -> Text -> Fay ()
    openGet = ffi "%1.open(\"GET\", %2, true)"

    openPost :: XMLHttpRequest -> Text -> Fay ()
    openPost = ffi "%1.open(\"POST\", %2, true)"

    setReadyHandler :: XMLHttpRequest -> Fay () -> Fay ()
    setReadyHandler = ffi "%1['onreadystatechange'] = %2"

    isDone :: XMLHttpRequest -> Fay Bool
    isDone = ffi "4 == %1['readyState']"

    responseText :: XMLHttpRequest -> Fay Text
    responseText = ffi "%1['responseText']"

    send :: XMLHttpRequest -> Text -> Fay ()
    send = ffi "%1.send(%2)"