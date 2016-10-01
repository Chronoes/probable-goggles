module Handler (
handleRequest
) where

import Network.HTTP.Types.URI (SimpleQuery, parseSimpleQuery)
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust, fromMaybe)
import qualified Database.SQLite.Simple as DB

import Network.HTTP.Parser
import ConfigParser (Config)

userAgent :: Header
userAgent = (hUserAgent, "probable-goggles/0.1.0.0")

-- TODO: proper handling of this request
handleDownload :: DB.Connection -> String -> SimpleQuery -> IO HTTPDocument
handleDownload dbc addr q =
    let value a = lookup a q
        requestId = fromMaybe "0" $ value "id" in do
    DB.execute dbc "INSERT INTO requests (request_id, url) VALUES (?, ?)" (BS.unpack requestId, value "url")
    DB.execute dbc "INSERT INTO routing (request_id, download_ip) VALUES (?, ?)" (BS.unpack requestId, addr)
    return ((newResponse ok200, [userAgent, (hContentType, "text/html; charset=utf-8")]), Just $ BS.append "OK\n" requestId)

-- TODO: fix this temp assignment
handleFile = handleDownload

handleNotFound :: BS.ByteString -> StdMethod -> IO HTTPDocument
handleNotFound p m = return ((newResponse notFound404, [userAgent, (hContentType, "text/html; charset=utf-8")]), Just $ BS.unwords ["Error: Cannot", renderStdMethod m, p])


_handleRequest :: String -> HTTPDocument -> DB.Connection -> IO HTTPDocument
_handleRequest addr ((r, h), b) dbc = case (path, method r) of
    ("/download", GET) -> handleDownload dbc addr query
    ("/file", POST) -> handleFile dbc addr query
    _ -> handleNotFound path $ method r
    where (path, queryString) = BS.break (== '?') . uri $ r
          query = parseSimpleQuery . BS.tail $ queryString


handleRequest :: String -> String -> HTTPDocument -> IO HTTPDocument
handleRequest db addr doc = DB.withConnection db $ _handleRequest addr doc
