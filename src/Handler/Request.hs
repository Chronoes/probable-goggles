module Handler.Request (
doDownload,
getDownloaderIp,
forwardFile
) where

import Data.Foldable (forM_)
import Data.Maybe (fromMaybe, fromJust)

import System.Random (randomIO)
import Control.Concurrent (forkIO)

import Happstack.Server (Host, RqBody, unBody)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (statusCode)
import qualified Data.Aeson as JSON

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Database.SQLite.Simple as DB

import Handler.Client
import Handler.Response
import Handler.Types

-- /download related functions
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


contactNeighbours :: String -> Host -> (Host -> IO StdResponse) -> IO()
contactNeighbours db (clientIp, _) action = DB.withConnection db $ \dbc -> do
    res <- DB.query dbc
        "SELECT ip, port FROM alive_neighbours WHERE ip <> ?"
        (DB.Only clientIp) :: IO [(String, String)]
    forM_ res $ \(ip, port) -> action (ip, read port :: Int) >>= handleResponse

forwardDownload :: String -> Host -> Int -> String -> IO()
forwardDownload db peer i = contactNeighbours db peer . sendDownloadRequest i


initDownload :: Host -> Int -> String -> IO()
initDownload peer reqId url = do
    res <- downloadFromURL url
    case statusCode $ responseStatus res of
        200 -> sendReq $
            FileBody 200
                (Just . S.unpack . fromMaybe "text/plain" . lookup hContentType $ responseHeaders res)
                (encodeContent $ responseBody res)
        s -> sendReq $ FileBody s Nothing Nothing
    where sendReq b = sendFileRequest reqId b peer >>= handleResponse

tooLazyToDownload :: Float -> Bool -> IO Bool
tooLazyToDownload _ False = return True
tooLazyToDownload laziness True = do
    rand <- randomIO :: IO Float
    return $ rand < laziness

doDownload :: String -> Host -> Int -> String -> Float -> IO()
doDownload db peer reqId url laziness = do
    isLazy <- tooLazyToDownload laziness =<< shouldDownload db peer reqId url
    forkIO $ handleDl isLazy
    DB.withConnection db $ \dbc ->
        DB.execute dbc
            "UPDATE requests SET action = ? WHERE request_id = ?"
            (dlAction isLazy, reqId)
    where handleDl True = forwardDownload db peer reqId url
          handleDl False = initDownload peer reqId url
          dlAction :: Bool -> String
          dlAction True = "forward"
          dlAction False = "download"


-- /file related functions
getDownloaderIp :: String -> Host -> Int -> IO (Either Bool Host)
getDownloaderIp db (ip, port) reqId = DB.withConnection db $ \dbc -> do
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
            return $ Left True
        [(Just d, Nothing)] -> do
            DB.execute dbc
                "UPDATE routing SET file_ip = ? WHERE request_id = ?"
                (ip, reqId)
            return $ Right (ip, port)
        [(Nothing, Nothing)] -> error "Both download_ip and file_ip in routing table cannot be NULL"
        _ -> return $ Left False


forwardFile :: Either Bool Host -> String -> Host -> Int -> Maybe RqBody -> IO (Either (Maybe String) L.ByteString)
forwardFile (Left False) _ _ _ _ = return $ Left Nothing
forwardFile _ _ _ _ Nothing = return . Left $ Just "Error: Request body empty"
forwardFile (Left True) db peer i (Just b) = do
    forkIO . contactNeighbours db peer . sendRawFileRequest i $ unBody b
    return $ Left Nothing
forwardFile (Right ("127.0.0.1", _)) _ _ _ (Just b) =
    case JSON.decode $ unBody b of
        Nothing -> return . Left $ Just "Error: Invalid body format"
        Just body -> return . handleFaultyBody . decodeContent $ rawContent body
    where handleFaultyBody (Left err) = Left . Just $ "Error: " ++ err
          handleFaultyBody (Right "") = Left $ Just "Error: Empty body"
          handleFaultyBody (Right b) = Right (b :: L.ByteString)
forwardFile (Right downloader) _ _ i (Just b) = do
    forkIO $ sendRawFileRequest i (unBody b) downloader >>= handleResponse
    return $ Left Nothing
