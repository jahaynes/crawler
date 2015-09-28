module Ajax where

import FrontEndTypes
import Fay.Text

import FFI

ajax :: Url -> (Text -> Fay ()) -> Fay ()
ajax url f = do
    req <- xmlHttpRequest    
    open req url
    setReadyHandler req $ do
        done <- isDone req
        when done $ responseText req >>= f
    send req ""

    where
    xmlHttpRequest :: Fay XMLHttpRequest
    xmlHttpRequest = ffi "new XMLHttpRequest()"

    open :: XMLHttpRequest -> String -> Fay ()
    open = ffi "%1.open(\"GET\", %2, true)"

    setReadyHandler :: XMLHttpRequest -> Fay () -> Fay ()
    setReadyHandler = ffi "%1['onreadystatechange'] = %2"

    isDone :: XMLHttpRequest -> Fay Bool
    isDone = ffi "4 == %1['readyState']"

    responseText :: XMLHttpRequest -> Fay Text
    responseText = ffi "%1['responseText']"

    send :: XMLHttpRequest -> String -> Fay ()
    send = ffi "%1.send(%2)"