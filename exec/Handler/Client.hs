module Handler.Client (
userAgent,
addUserAgent,
downloadFromURL,
sendDownloadRequest,
sendFileRequest
) where

import Data.Maybe (fromJust)
import Network.Simple.TCP
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.URI (SimpleQuery, renderSimpleQuery)
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status (statusCode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

import Network.HTTP.Parser
import Handler.Response

userAgent :: BS.ByteString
userAgent = "probable-goggles/0.1.0.0"

addUserAgent :: HTTPDocument -> HTTPDocument
addUserAgent = addHeader (hUserAgent, userAgent)


-- TODO: Handle faulty connections (host is down or not responding)
-- TODO: recv on socket, consider adjusting size
sendRequest :: HTTPDocument -> (HostName, ServiceName) -> IO HTTPDocument
sendRequest doc (host, port) = connect host port $ \(connectionSocket, remoteAddr) -> do
    putStrLn $ "TCP connection established to " ++ show remoteAddr

    let reqDoc = renderHTTPDocument . addHeader (hHost, BS.pack host) $ addUserAgent doc
    print reqDoc
    send connectionSocket reqDoc

    res <- recv connectionSocket 1024
    let resDoc = splitHeadFromBody parseResponseHead $ fromJust res
    print resDoc
    return resDoc


concatPath :: BS.ByteString -> SimpleQuery -> BS.ByteString
concatPath path = BS.append path . renderSimpleQuery True

downloadFromURL :: String -> IO HTTPDocument
downloadFromURL url = do
    manager <- newManager tlsManagerSettings

    request <- parseRequest url
    response <- httpLbs request manager

    -- putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)

    return ((newResponse $ responseStatus response, responseHeaders response), Just . LBS.toStrict $ responseBody response)

sendDownloadRequest :: SimpleQuery -> (HostName, ServiceName) -> IO HTTPDocument
sendDownloadRequest q = sendRequest ((newRequest GET $ concatPath "/download" q, [(hAccept, "*/*")]), Nothing)

sendFileRequest :: SimpleQuery -> Body -> (HostName, ServiceName) -> IO HTTPDocument
sendFileRequest q b = sendRequest (
    (newRequest POST $ concatPath "/file" q,
    [(hContentType, "application/json"), (hAccept, "*/*")]),
    Just b)
