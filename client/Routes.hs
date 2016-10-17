module Routes where

import Data.Maybe (fromMaybe)
import System.Random (randomRIO)
import Control.Monad.IO.Class (liftIO)

import Network.HTTP.Types.Header (hContentType)
import Happstack.Server as HS

import Handler.Client
import Logger

download :: Int -> ServerPart HS.Response
download port = do
    HS.method GET
    url <- look "url"
    rid <- liftIO $ randomRIO (1000, 999999999)

    logToStdout $ unwords ["/download: Getting URL:", url, "with ID:", show rid]
    response <- liftIO $ sendDownloadRequest rid url ("127.0.0.1", port)

    let mimeType = fromMaybe "text/plain" (lookup hContentType $ responseHeaders response)
    ok $ toResponseBS mimeType (responseBody response)

notFound :: ServerPart HS.Response
notFound = do
    req <- askRq
    HS.notFound . toResponse $ unwords ["Error: Cannot", show $ rqMethod req, rqUri req ++ rqQuery req]
