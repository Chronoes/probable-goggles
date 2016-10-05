module Handler.Request (
handleRequest
) where

import Data.Char (isDigit)
import Data.Foldable (forM_)
import Data.Traversable (forM)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Control.Concurrent (forkIO)
import Control.Monad ((>=>))

import Network.HTTP.Types.URI (SimpleQuery, parseSimpleQuery)
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Simple.TCP (HostName, ServiceName)
import System.Random (randomIO)
import Data.Aeson ((.=), (.:), (.:?), ToJSON, FromJSON)

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Database.SQLite.Simple as DB

import Network.HTTP.Parser
import Handler.Client
import Handler.Response


badRequest :: BS.ByteString -> HTTPDocument
badRequest err = ((newResponse badRequest400, [(hContentType, "text/html; charset=utf-8")]), Just $ "Error: " `BS.append` err)

okRequest :: HTTPDocument
okRequest = ((newResponse ok200, [(hContentType, "text/html; charset=utf-8")]), Just "OK")

shouldDownload :: String -> Int -> String -> HostName -> IO Bool
shouldDownload db reqId url ip = DB.withConnection db $ \dbc -> do
    res <- DB.query dbc
        "SELECT download_ip, file_ip FROM routing WHERE request_id = ?"
        (DB.Only reqId) :: IO [(Maybe HostName, Maybe HostName)]

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
                "UPDATE requests SET url = ? WHERE request_id = ?"
                (url, reqId)
            DB.execute dbc
                "UPDATE routing SET download_ip = ? WHERE request_id = ?"
                (ip, reqId)
            return False
        [(Nothing, Nothing)] -> error "Both download_ip and file_ip in routing table cannot be NULL"
        _ -> return False


contactNeighbours :: String -> HostName -> ((HostName, ServiceName) -> IO HTTPDocument) -> IO()
contactNeighbours db clientIp action = DB.withConnection db $ \dbc -> do
    res <- DB.query dbc
        "SELECT ip, port FROM alive_neighbours WHERE ip <> ?"
        (DB.Only clientIp) :: IO [(HostName, ServiceName)]
    forM_ res $ action >=> handleResponse

forwardDownload :: String -> HostName -> SimpleQuery -> IO()
forwardDownload db ip = contactNeighbours db ip . sendDownloadRequest

data FileBody = FileBody {
    downloadStatus :: Int,
    mimeType :: Maybe String,
    content :: Maybe String
} deriving (Show)

instance FromJSON FileBody where
    parseJSON (JSON.Object v) =
        FileBody <$>
        v .: "status" <*>
        v .:? "mime-type" <*>
        v .:? "content"

instance ToJSON FileBody where
    toJSON (FileBody s Nothing Nothing) =
        JSON.object ["status" .= s]
    toJSON (FileBody s m c) =
        JSON.object ["status" .= s, "mime-type" .= m, "content" .= c]

    toEncoding (FileBody s Nothing Nothing) =
        JSON.pairs ("status" .= s)
    toEncoding (FileBody s m c) =
        JSON.pairs ("status" .= s <> "mime-type" .= m <> "content" .= c)


initDownload :: (HostName, ServiceName) -> BS.ByteString -> String -> IO()
initDownload client reqId url = do
    ((res, headers), Just body) <- downloadFromURL url
    case statusCode $ status res of
        200 -> sendReq . JSON.encode $
            FileBody 200 (Just . BS.unpack . fromMaybe "text/html" $ lookup hContentType headers) (Just . BS.unpack $ B64.encode body)
        s -> sendReq . JSON.encode $ FileBody s Nothing Nothing
    where sendReq b = sendFileRequest [("id", reqId)] (BSL.toStrict b) client >>= handleResponse

tooLazyToDownload :: Float -> Bool -> IO Bool
tooLazyToDownload _ False = return True
tooLazyToDownload laziness True = do
    rand <- randomIO
    return $ (rand :: Float) >= laziness

handleDownload :: String -> Float -> (HostName, ServiceName) -> SimpleQuery -> IO HTTPDocument
handleDownload db laziness client q =
    case (uriValue "id", uriValue "url") of
        (Just reqId, Just u)
            | BS.all isDigit reqId ->
                let url = BS.unpack u
                in do
                isLazy <- tooLazyToDownload laziness =<< shouldDownload db (read $ BS.unpack reqId :: Int) url (fst client)
                forkIO $ if isLazy then forwardDownload db (fst client) q else initDownload client reqId url
                return okRequest
            | otherwise -> return $ badRequest "Parameter 'id' must be all digits"
        (Nothing, _) -> return $ badRequest "Missing parameter 'id'"
        (_, Nothing) -> return $ badRequest "Missing parameter 'url'"
    where uriValue a = lookup a q


getDownloaderIp :: String -> HostName -> Int -> IO (Maybe HostName)
getDownloaderIp db ip reqId = DB.withConnection db $ \dbc -> do
    res <- DB.query dbc
        "SELECT download_ip, file_ip FROM routing WHERE request_id = ?"
        (DB.Only reqId) :: IO [(Maybe String, Maybe String)]

    case res of
        [] -> do
            DB.execute dbc
                "INSERT INTO requests (request_id) VALUES (?)"
                (DB.Only reqId)
            DB.execute dbc
                "INSERT INTO routing (request_id, file_ip) VALUES (?, ?)"
                (reqId, ip)
            return Nothing
        [(Just d, Nothing)] -> do
            DB.execute dbc
                "UPDATE routing SET file_ip = ? WHERE request_id = ?"
                (ip, reqId)
            return $ Just d
        [(Nothing, Nothing)] -> error "Both download_ip and file_ip in routing table cannot be NULL"
        _ -> return Nothing

-- TODO: Do stuff with the downloader
handleFile :: String -> (HostName, ServiceName) -> SimpleQuery -> Maybe Body -> IO HTTPDocument
handleFile db (ip, port) q b =
    case (lookup "id" q, b) of
        (Just reqId, Just body)
            | BS.all isDigit reqId -> do
                downloader <- getDownloaderIp db ip (read $ BS.unpack reqId :: Int)
                return okRequest
            | otherwise ->
                return $ badRequest "Parameter 'id' must be all digits"
        (Nothing, _) -> return $ badRequest "Missing parameter 'id'"
        (_, Nothing) -> return $ badRequest "Missing body"


handleNotFound :: BS.ByteString -> StdMethod -> IO HTTPDocument
handleNotFound path m = return (
    (newResponse notFound404, [(hContentType, "text/html; charset=utf-8")]),
    Just $ BS.unwords ["Error: Cannot", renderStdMethod m, path])


handleRequest :: (String -> String) -> (HostName, ServiceName) -> HTTPDocument -> IO HTTPDocument
handleRequest conf client ((r, h), b) = case (method r, path) of
    (GET, "/download") -> handleDownload db (read $ conf "laziness" :: Float) client query
    (POST, "/file") -> handleFile db client query b
    (m, _) -> handleNotFound path m

    where (path, queryString) = BS.break (== '?') . uri $ r
          query = parseSimpleQuery . BS.tail $ queryString
          db = conf "db"
