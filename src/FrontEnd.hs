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

    getElementById "addUrl" >>= \btnAdd ->
        addEventListener btnAdd "onclick" $ \e -> do
            preventDefault e
            getValue urlPatternBox >>= setPatterns
	    getEscapedValue urlToAddBox >>= addUrl

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
