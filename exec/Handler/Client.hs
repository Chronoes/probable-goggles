module Handler.Client (
userAgent,
addUserAgent,
sendDownloadRequest,
sendFileRequest
) where

import Data.Maybe (fromJust)
import Network.Simple.TCP
import Network.HTTP.Types.URI (SimpleQuery, renderSimpleQuery)
import Network.HTTP.Types.Header
import qualified Data.ByteString.Char8 as BS

import Network.HTTP.Parser
import Handler.Response

userAgent :: BS.ByteString
userAgent = "probable-goggles/0.1.0.0"

addUserAgent :: HTTPDocument -> HTTPDocument
addUserAgent = addHeader (hUserAgent, userAgent)


-- TODO: Handle faulty connections (host is down or not responding)
-- TODO: recv on socket, consider adjusting size
sendRequest :: HTTPDocument -> (HostName, ServiceName) -> IO()
sendRequest doc (host, port) = connect host port $ \(connectionSocket, remoteAddr) -> do
    putStrLn $ "Connection established to " ++ show remoteAddr

    send connectionSocket . renderHTTPDocument $ addUserAgent doc

    res <- recv connectionSocket 1024
    handleResponse . splitHeadFromBody parseResponseHead $ fromJust res


sendDownloadRequest :: SimpleQuery -> (HostName, ServiceName) -> IO()
sendDownloadRequest q = sendRequest ((newRequest GET $ "/download" `BS.append` renderSimpleQuery True q, []), Nothing)

sendFileRequest :: SimpleQuery -> Body -> (HostName, ServiceName) -> IO()
sendFileRequest q b = sendRequest ((newRequest POST $ "/file" `BS.append` renderSimpleQuery True q, []), Just b)
