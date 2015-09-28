{-# LANGUAGE EmptyDataDecls, OverloadedStrings #-}
module Hello where

import FrontEndTypes
import Ajax

import Fay.Text
import FFI

main :: Fay ()
main = do

    urlToAddBox <- getElementById "urlToAdd"

    getElementById "urlsInQueue" >>= \urlsInQueue ->
        repeatTask 300 $ ajax "/queueSize/UrlQueue" $ setInnerHTML urlsInQueue

    getElementById "addUrl" >>= \btnAdd ->
        addEventListener btnAdd "onclick" $ \e -> do
            preventDefault e
            getEscapedValue urlToAddBox >>= addUrl

    putStrLn "Loaded"

preventDefault :: Event -> Fay ()
preventDefault = ffi "%1.preventDefault()"

getEscapedValue :: Element -> Fay Escaped
getEscapedValue el = go' el >>= return . Escaped
    where
    go' :: Element -> Fay String
    go' = ffi "encodeURIComponent(%1.value.trim())"

addUrl :: Escaped -> Fay ()
addUrl (Escaped url) = ajax ("/addUrl/" ++ url) $ \_ -> return ()

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
