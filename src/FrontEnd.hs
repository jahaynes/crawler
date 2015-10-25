{-# LANGUAGE EmptyDataDecls #-}
module Hello where

import FrontEndTypes
import Ajax

import Fay.Text as T
import FFI

main :: Fay ()
main = do

    urlToAddBox <- getElementById "urlToAdd"
    urlPatternBox <- getElementById "urlPattern"

    getElementById "urlsInQueue" >>= \urlsInQueue ->
        repeatTask 300 $ ajax GET (T.pack "/queueSize/UrlQueue") empty $ setInnerHTML urlsInQueue

    getElementById "parseQueue" >>= \parseQueue ->
        repeatTask 300 $ ajax GET (T.pack "/queueSize/ParseQueue") empty $ setInnerHTML parseQueue

    getElementById "storeQueue" >>= \storeQueue ->
        repeatTask 300 $ ajax GET (T.pack "/queueSize/StoreQueue") empty $ setInnerHTML storeQueue

    getElementById "logQueue" >>= \logQueue ->
        repeatTask 300 $ ajax GET (T.pack "/queueSize/ErrorQueue") empty $ setInnerHTML logQueue

    getElementById "addUrl" >>= \btnAdd ->
        addEventListener btnAdd "onclick" $ \e -> do
            preventDefault e
            getValue urlPatternBox >>= setPatterns
            getEscapedValue urlToAddBox >>= addUrl

    getElementById "crawlerStatus" >>= \crawlerStatus ->
        repeatTask 300 $ ajax GET (T.pack "/crawlerStatus") empty $ setInnerHTML crawlerStatus

    getElementById "idle" >>= \btnIdle -> do
        addEventListener btnIdle "onclick" $ \e -> do
            preventDefault e
            idle

    getElementById "halt" >>= \btnHalt -> do
        addEventListener btnHalt "onclick" $ \e -> do
            preventDefault e
            halt

    putStrLn "Loaded"

preventDefault :: Event -> Fay ()
preventDefault = ffi "%1.preventDefault()"

getEscapedValue :: Element -> Fay Escaped
getEscapedValue el = go' el >>= return . Escaped
    where
    go' :: Element -> Fay Text
    go' = ffi "encodeURIComponent(%1.value.trim())"

getValue :: Element -> Fay Text
getValue = ffi "%1.value.trim()"

setValue :: Element -> Text -> Fay ()
setValue = ffi "%1.value = %2"

addUrl :: Escaped -> Fay ()
addUrl (Escaped url) = ajax POST (T.pack "/addUrl/" `T.append` url) empty $ \_ -> return ()

idle :: Fay ()
idle = ajax POST (T.pack "/idle") empty $ \_ -> return ()

halt :: Fay ()
halt = ajax POST (T.pack "/halt") empty $ \_ -> return ()

setPatterns :: Text -> Fay ()
setPatterns patterns = ajax POST (T.pack "/urlPatterns/") patterns $ \_ -> return ()

getElementById :: String -> Fay Element
getElementById = ffi "document.getElementById(%1)"

addEventListener :: Element -> String -> (Event -> Fay ()) -> Fay ()
addEventListener = ffi "%1[%2] = %3"

remove :: Element -> Element -> Fay ()
remove = ffi "%1.removeChild(%2)"

repeatTask :: Int -> Fay () -> Fay TaskId
repeatTask = ffi "setInterval(%2,%1)"

setInnerHTML :: Element -> Text -> Fay ()
setInnerHTML = ffi "%1['innerHTML'] = %2"
