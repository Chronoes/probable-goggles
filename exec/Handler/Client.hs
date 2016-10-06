module Handler.Client (
module Network.HTTP.Client,
StdResponse,
userAgent,
downloadFromURL,
sendDownloadRequest,
sendFileRequest
) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (Header, hUserAgent)
import Happstack.Server (Host)

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S
import qualified Data.Aeson as JSON

import Handler.Types

type StdResponse = Response L.ByteString

userAgent :: Header
userAgent = (hUserAgent, "probable-goggles/0.1.0.0")

sendRequest :: Request -> IO StdResponse
sendRequest r = do
    manager <- newManager tlsManagerSettings
    httpLbs r { requestHeaders = userAgent : requestHeaders r } manager

convertToValue :: (Show a) => a -> Maybe S.ByteString
convertToValue = Just . S.pack . show

setHost :: Host -> Request -> Request
setHost (ip, port) r = r { host = S.pack ip, port = port }

sendDownloadRequest :: Int -> String -> Host -> IO StdResponse
sendDownloadRequest reqId url host =
    sendRequest
    . setHost host
    . setQueryString [("id", convertToValue reqId), ("url", convertToValue url)]
    $ defaultRequest { method = "GET", path = "/download" }

sendFileRequest :: Int -> FileBody -> Host -> IO StdResponse
sendFileRequest reqId b host =
    sendRequest
    . setHost host
    . setQueryString [("id", convertToValue reqId)]
    $ defaultRequest { method = "POST", path = "/file", requestBody = RequestBodyLBS $ JSON.encode b }


downloadFromURL :: String -> IO StdResponse
downloadFromURL url = parseRequest url >>= sendRequest
