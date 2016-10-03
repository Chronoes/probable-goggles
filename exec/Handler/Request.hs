module Handler.Request (
userAgent,
handleRequest
) where

import Data.Char (isDigit)
import Data.Foldable (forM_)
import Control.Concurrent (forkIO)
import Network.HTTP.Types.URI (SimpleQuery, parseSimpleQuery)
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Simple.TCP (HostName, ServiceName)
import qualified Data.ByteString.Char8 as BS
import qualified Database.SQLite.Simple as DB

import Network.HTTP.Parser
import Handler.Client

userAgent :: Header
userAgent = (hUserAgent, "probable-goggles/0.1.0.0")

addUserAgent :: HTTPDocument -> HTTPDocument
addUserAgent = addHeader userAgent

_handleDownload :: String -> Int -> String -> String -> IO Bool
_handleDownload db reqId url ip = DB.withConnection db $ \dbc -> do
    res <- DB.query dbc
        "SELECT download_ip, file_ip FROM routing WHERE request_id = ?"
        (DB.Only reqId) :: IO [(Maybe String, Maybe String)]

    case res of
        [] -> do
            DB.execute dbc
                "INSERT INTO requests (request_id, url) VALUES (?, ?)"
                (reqId, url)
            DB.execute dbc
                "INSERT INTO routing (request_id, download_ip) VALUES (?, ?)"
                (reqId, ip)
            return True
        [(Nothing, Just f)] -> do
            DB.execute dbc
                "UPDATE routing SET download_ip = ? WHERE request_id = ?"
                (ip, reqId)
            return False
        [(Nothing, Nothing)] -> error "Both download_ip and file_ip in routing table cannot be NULL"
        _ -> return False


-- TODO: If true, do downloading of requested file and send /file request
downloadOrForward :: Bool -> String -> SimpleQuery -> IO()
downloadOrForward False db q = DB.withConnection db $ \dbc -> do
    res <- DB.query_ dbc
        "SELECT ip, port FROM alive_neighbours" :: IO [(HostName, ServiceName)]
    forM_ res $ sendDownloadRequest q
downloadOrForward True db q = error "TODO downloadOrForward"

handleDownload :: String -> String -> SimpleQuery -> IO HTTPDocument
handleDownload db ip q =
    case (uriValue "id", uriValue "url") of
        (Just rid, Just u)
            | BS.all isDigit rid -> do
                shouldDl <- _handleDownload db (read $ BS.unpack rid :: Int) (BS.unpack u) ip
                forkIO $ downloadOrForward shouldDl db q
                return ((newResponse ok200, headers), Just "OK")
            | otherwise -> badReq "Parameter 'id' must be all digits"
        (Nothing, _) -> badReq "Missing parameter 'id'"
        (_, Nothing) -> badReq "Missing parameter 'url'"
    where uriValue a = lookup a q
          headers = [(hContentType, "text/html; charset=utf-8")]
          badReq err = return ((newResponse badRequest400, headers), Just $ "Error: " `BS.append` err)

-- TODO: fix this temp assignment for handleFile
handleFile = error "TODO handleFile"

handleNotFound :: BS.ByteString -> StdMethod -> IO HTTPDocument
handleNotFound p m = return (
    (newResponse notFound404, [(hContentType, "text/html; charset=utf-8")]),
    Just $ BS.unwords ["Error: Cannot", renderStdMethod m, p])


handleRequest :: String -> String -> HTTPDocument -> IO HTTPDocument
handleRequest db addr ((r, h), b) = do
    doc <- case (method r, path) of
        (GET, "/download") -> handleDownload db addr query
        (POST, "/file") -> handleFile db addr query
        (m, _) -> handleNotFound path m

    return $ addUserAgent doc
    where (path, queryString) = BS.break (== '?') . uri $ r
          query = parseSimpleQuery . BS.tail $ queryString
