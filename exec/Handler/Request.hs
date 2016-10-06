module Handler.Request (
download,
file,
Handler.Request.notFound
) where

import Data.Char (isDigit)
import Data.Foldable (forM_)
import Data.Traversable (forM)
import Data.Maybe (fromMaybe)
import Control.Monad ((>=>))

import Control.Monad.IO.Class (liftIO)
import System.Random (randomIO)
import Control.Concurrent (forkIO)

import Happstack.Server as HS
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (ok200, statusCode)

import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.ByteString.Base64.Lazy as S64
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Database.SQLite.Simple as DB

import Handler.Client as C
import Handler.Response
import Handler.Types


shouldDownload :: String -> Host -> Int -> String -> IO Bool
shouldDownload db (ip, _) reqId url = DB.withConnection db $ \dbc -> do
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
                "UPDATE requests SET url = ? WHERE request_id = ?"
                (url, reqId)
            DB.execute dbc
                "UPDATE routing SET download_ip = ? WHERE request_id = ?"
                (ip, reqId)
            return False
        [(Nothing, Nothing)] -> error "Both download_ip and file_ip in routing table cannot be NULL"
        _ -> return False


contactNeighbours :: String -> Host -> (Host -> IO C.StdResponse) -> IO()
contactNeighbours db (clientIp, _) action = DB.withConnection db $ \dbc -> do
    res <- DB.query dbc
        "SELECT ip, port FROM alive_neighbours WHERE ip <> ?"
        (DB.Only clientIp) :: IO [Host]
    forM_ res $ action >=> print

forwardDownload :: String -> Host -> Int -> String -> IO()
forwardDownload db peer i = contactNeighbours db peer . sendDownloadRequest i


initDownload :: Host -> Int -> String -> IO()
initDownload peer reqId url = do
    res <- downloadFromURL url
    case statusCode $ responseStatus res of
        200 -> sendReq $
            FileBody 200
                (Just . S.unpack . fromMaybe "text/plain" . lookup hContentType $ responseHeaders res)
                (Just . decodeUtf8 . S64.encode $ responseBody res)
        s -> sendReq $ FileBody s Nothing Nothing
    where sendReq b = sendFileRequest reqId b peer >>= print

tooLazyToDownload :: Float -> Bool -> IO Bool
tooLazyToDownload _ False = return True
tooLazyToDownload laziness True = do
    rand <- randomIO
    return $ (rand :: Float) >= laziness


getDownloaderIp :: String -> Host -> Int -> IO (Maybe String)
getDownloaderIp db (ip, _) reqId = DB.withConnection db $ \dbc -> do
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


download :: String -> Float -> ServerPart HS.Response
download db laziness = do
    HS.method GET
    rid <- look "id"
    url <- look "url"
    if all isDigit rid
        then let reqId = read rid :: Int
            in do
            req <- askRq
            let peer = rqPeer req
            isLazy <- liftIO $ tooLazyToDownload laziness =<< shouldDownload db peer reqId url

            liftIO . forkIO $ if isLazy then forwardDownload db peer reqId url else initDownload peer reqId url
            ok $ toResponse ("OK" :: String)
        else
            badRequest $ toResponse ("Parameter 'id' must be all digits" :: String)

-- TODO: Do stuff with the downloader
file :: String -> ServerPart HS.Response
file db = do
    HS.method POST
    rid <- look "id"
    if all isDigit rid
        then let reqId = read rid :: Int
            in do
            req <- askRq
            let peer = rqPeer req
            downloader <- liftIO $ getDownloaderIp db peer reqId
            ok $ toResponse ("OK" :: String)
        else
            badRequest $ toResponse ("Parameter 'id' must be all digits" :: String)

notFound :: ServerPart HS.Response
notFound = do
    req <- askRq
    HS.notFound . toResponse $ unwords ["Error: Cannot", show $ rqMethod req, rqUri req]
