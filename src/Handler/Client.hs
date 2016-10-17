module Handler.Client (
module Network.HTTP.Client,
StdResponse,
userAgent,
downloadFromURL,
sendDownloadRequest,
sendRawFileRequest,
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
    putStrLn . S.unpack $ S.unwords [method r, "request sent to", host r]
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

sendRawFileRequest :: Int -> L.ByteString -> Host -> IO StdResponse
sendRawFileRequest reqId b host = sendRequest
    . setHost host
    . setQueryString [("id", convertToValue reqId)]
    $ defaultRequest { method = "POST", path = "/file", requestBody = RequestBodyLBS b }

sendFileRequest :: Int -> FileBody -> Host -> IO StdResponse
sendFileRequest reqId b = sendRawFileRequest reqId (JSON.encode b)

downloadFromURL :: String -> IO StdResponse
downloadFromURL url = parseRequest standardUrl >>= sendRequest
    where standardUrl = if take 4 url /= "http" then "http://" ++ url else url
