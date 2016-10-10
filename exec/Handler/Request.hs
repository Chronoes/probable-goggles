module Handler.Request (
download,
file,
Handler.Request.notFound
) where

import Data.Char (isDigit)
import Data.Foldable (forM_)
import Data.Traversable (forM)
import Data.Maybe (fromMaybe, fromJust)
import Control.Monad ((>=>))

import Control.Monad.IO.Class (liftIO)
import System.Random (randomIO)
import Control.Concurrent (forkIO)

import Happstack.Server as HS
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (statusCode)
import qualified Data.Aeson as JSON

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Database.SQLite.Simple as DB

import Handler.Client as C
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


contactNeighbours :: String -> Host -> (Host -> IO C.StdResponse) -> IO()
contactNeighbours db (clientIp, _) action = DB.withConnection db $ \dbc -> do
    res <- DB.query dbc
        "SELECT ip, port FROM alive_neighbours WHERE ip <> ?"
        (DB.Only clientIp) :: IO [Host]
    forM_ res $ action >=> handleResponse

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
    rand <- randomIO
    return $ (rand :: Float) >= laziness

doDownload :: String -> Host -> Int -> String -> Float -> IO()
doDownload db peer reqId url laziness = do
    isLazy <- tooLazyToDownload laziness =<< shouldDownload db peer reqId url
    forkIO $ handleDl isLazy
    return ()
    where handleDl True = forwardDownload db peer reqId url
          handleDl False = initDownload peer reqId url


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


forwardFile :: Either Bool Host -> String -> Host -> Int -> Maybe RqBody -> IO (Maybe String)
forwardFile (Left False) _ _ _ _ = return Nothing
forwardFile _ _ _ _ Nothing = return $ Just "Error: Request body empty"
forwardFile (Left True) db peer i (Just b) = do
    forkIO . contactNeighbours db peer . sendRawFileRequest i $ unBody b
    return Nothing
forwardFile (Right ("127.0.0.1", _)) _ _ _ (Just b) = do
    putStrLn "Downloaded body:"
    print $ unBody b
    putStrLn "Decoded body:"
    print . decodeContent $ rawContent $ fromJust (JSON.decode $ unBody b :: Maybe FileBody)
    return Nothing
forwardFile (Right downloader) _ _ i (Just b) = do
    forkIO $ sendRawFileRequest i (unBody b) downloader >>= handleResponse
    return Nothing


-- HTTP Handlers
download :: String -> Int -> Float -> ServerPart HS.Response
download db sp laziness = do
    HS.method GET
    rid <- look "id"
    url <- look "url"
    if all isDigit rid
        then let reqId = read rid :: Int
            in do
            req <- askRq
            let peer = (fst $ rqPeer req, sp)
            liftIO $ doDownload db peer reqId url laziness
            ok $ toResponse ("OK" :: String)
        else
            badRequest $ toResponse ("Error: Parameter 'id' must be all digits" :: String)

file :: String -> Int -> ServerPart HS.Response
file db sp = do
    HS.method POST
    rid <- look "id"
    if all isDigit rid
        then let reqId = read rid :: Int
            in do
            req <- askRq
            let peer = (fst $ rqPeer req, sp)
            downloader <- liftIO $ getDownloaderIp db peer reqId

            err <- liftIO $ forwardFile downloader db peer reqId =<< takeRequestBody req
            case err of
                (Just e) -> badRequest $ toResponse e
                _ -> ok $ toResponse ("OK" :: String)
        else
            badRequest $ toResponse ("Error: Parameter 'id' must be all digits" :: String)

notFound :: ServerPart HS.Response
notFound = do
    req <- askRq
    HS.notFound . toResponse $ unwords ["Error: Cannot", show $ rqMethod req, rqUri req ++ rqQuery req]
