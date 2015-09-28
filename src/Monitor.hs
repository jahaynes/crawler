{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Main where

import Data.Text.Lazy         as T              (toStrict, pack)
import Web.Spock.Safe                           
import Network.Wai.Middleware.Static            (staticPolicy, addBase)
import Text.Hamlet                              (Html, shamlet)
import Text.Blaze.Html.Renderer.Text            (renderHtml)
import Text.Blaze.Internal                      (Markup)

import Communication
import Data.ByteString.Char8                    as C8
import Control.Monad.IO.Class                   (liftIO)

main :: IO ()
main =
    runSpock 8080 $
        spockT id $ do
            middleware . staticPolicy . addBase $ "static"
            spockApp

spockApp = do

    get root $ fromTemplate mainPage

    get ("addUrl" <//> var) $ \url -> do
        msg <- liftIO . addUrl $ url
        text . toStrict . T.pack . show $ msg

    get ("queueSize" <//> var) $ \qn -> do
        msg <- liftIO . queueSize $ qn
        text . toStrict . T.pack . show $ msg

    where
    fromTemplate :: Html -> ActionT IO a
    fromTemplate = html . toStrict . renderHtml

addUrl :: String -> IO Message
addUrl url = sendAndGetReply $ CommandMessage (AddUrl (C8.pack url))

queueSize :: String -> IO Message
queueSize name = sendAndGetReply . QuestionMessage . GetQueueSize $ (read name :: QueueName)

mainPage :: Markup
mainPage = [shamlet|
    <!DOCTYPE html>
    <html lang="en-AU">
        <head>
            <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
            <title>Crawler Monitor
        <body>
            <div id="parent">URLs in queue: <span id="urlsInQueue"></span>
            <div id="start">start</div>
            <script type="text/javascript" src="fay.js">
|]