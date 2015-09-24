{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Main where

import Data.Text.Lazy         as T              (toStrict, unpack)
import Web.Spock.Safe                           
import Network.Wai.Middleware.Static            (staticPolicy, addBase)
import Text.Hamlet                              (Html, shamlet)
import Text.Blaze.Html.Renderer.Text            (renderHtml)
import Text.Blaze.Internal                      (Markup)

import Communication
import Data.ByteString.Char8                as C8
import Control.Monad.IO.Class               (liftIO)

main :: IO ()
main =
    runSpock 8080 $
        spockT id $ do

            get root $ fromTemplate mainPage

            get ("addUrl" <//> var) $ \url -> liftIO . addUrl $ url

    where
    fromTemplate :: Html -> ActionT IO a
    fromTemplate = html . toStrict . renderHtml
        
addUrl url = do
    withClient $ \c -> send (AddUrl (C8.pack url)) c

mainPage :: Markup        
mainPage = [shamlet|
    <!DOCTYPE html>
    <html lang="en-AU">
        <head>
            <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
            <title>Crawler Monitor
        <body>
|]