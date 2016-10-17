module Routes where

import Data.Char (isDigit)
import Data.Maybe (fromMaybe, fromJust)
import Control.Applicative (optional)

import Control.Monad.IO.Class (liftIO)

import Happstack.Server

import Handler.Request


download :: String -> Int -> Float -> ServerPart Response
download db servicePort laziness = do
    method GET
    rid <- look "id"
    url <- look "url"
    port <- optional $ queryString $ look "port"
    if all isDigit rid
        then let reqId = read rid
            in do
            req <- askRq
            let peer = (fst $ rqPeer req, read $ fromMaybe (show servicePort) port)
            liftIO $ print $ "Download request from " ++ fst peer
            liftIO $ doDownload db peer reqId url laziness
            ok $ toResponse ("OK" :: String)
        else
            badRequest $ toResponse ("Error: Parameter 'id' must be all digits" :: String)

file :: String -> Int -> ServerPart Response
file db servicePort = do
    method POST
    rid <- look "id"
    if all isDigit rid
        then let reqId = read rid
            in do
            port <- optional $ queryString $ look "port"
            req <- askRq
            let peer = (fst $ rqPeer req, read $ fromMaybe (show servicePort) port)
            liftIO $ print $ "File request from " ++ fst peer
            downloader <- liftIO $ getDownloaderIp db peer reqId

            result <- liftIO $ forwardFile downloader db peer reqId =<< takeRequestBody req
            case result of
                Left (Just e) -> badRequest $ toResponse e
                Left Nothing -> ok $ toResponse ("OK" :: String)
                Right b -> do
                    liftIO $ print b
                    ok $ toResponse ("OK" :: String)
        else
            badRequest $ toResponse ("Error: Parameter 'id' must be all digits" :: String)

notFound :: ServerPart Response
notFound = do
    req <- askRq
    Happstack.Server.notFound . toResponse $ unwords ["Error: Cannot", show $ rqMethod req, rqUri req ++ rqQuery req]
