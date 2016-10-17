module Routes where

import Data.Maybe (fromMaybe)
import System.Random (randomRIO)
import Control.Monad.IO.Class (liftIO)

import Network.HTTP.Types.Header (hContentType)
import Happstack.Server as HS

import Handler.Client

download :: Int -> ServerPart HS.Response
download port = do
    HS.method GET
    url <- look "url"
    rid <- liftIO $ randomRIO (1000, 999999999)
    liftIO $ putStrLn url
    response <- liftIO $ sendDownloadRequest rid url ("127.0.0.1", port)
    -- liftIO $ print response
    ok $ toResponseBS (fromMaybe "text/plain" (lookup hContentType $ responseHeaders response)) (responseBody response)

notFound :: ServerPart HS.Response
notFound = do
    req <- askRq
    HS.notFound . toResponse $ unwords ["Error: Cannot", show $ rqMethod req, rqUri req ++ rqQuery req]
