module Routes where

import System.Directory (doesFileExist)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe, fromJust)
import Control.Applicative (optional)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as S
import qualified Database.SQLite.Simple as DB

import qualified Data.Aeson as JSON
import Happstack.Server

import Handler.Request
import Handler.Types


parseHost :: Int -> ServerPart Host
parseHost servicePort = do
    req <- askRq
    port <- optional $ queryString $ look "port"
    return (fst $ rqPeer req, read $ fromMaybe (show servicePort) port)


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

waitForResponse :: Int -> FilePath -> IO (Either String FileBody)
waitForResponse count file
    | count <= 0 = return $ Left "Timeout"
    | otherwise = do
        exists <- doesFileExist file
        if exists
            then Right . fromJust . JSON.decode <$> L.readFile file
            else do
                sleep
                waitForResponse (pred count) file
    where sleep = threadDelay $ 100 * 1000

download :: String -> Int -> FilePath -> Float -> ServerPart Response
download db servicePort cacheDir laziness = do
    method GET
    rid <- look "id"
    url <- look "url"
    if all isDigit rid
        then let reqId = read rid
            in do
            peer <- parseHost servicePort
            liftIO $ doDownload db peer reqId url laziness
            case fst peer of
                "127.0.0.1" -> do
                    result <- liftIO $ waitForResponse 200 $ cacheDir ++ rid
                    case result of
                        Left s -> resp 408 $ toResponse ("Timeout" :: String)
                        Right fb -> ok $ toResponseBS
                            (S.pack . fromMaybe "text/plain" $ mimeType fb)
                            (maybe "Error: no body" (decodeContent' . Just) $ rawContent fb)
                _ -> ok $ toResponse ("OK" :: String)
        else
            badRequest $ toResponse ("Error: Parameter 'id' must be all digits" :: String)

handleFaultyBody :: Either String L.ByteString -> Either String L.ByteString
handleFaultyBody (Left err) = Left $ "Error: " ++ err
handleFaultyBody (Right "") = Left "Error: Empty body"
handleFaultyBody (Right b) = Right b

file :: String -> Int -> FilePath -> ServerPart Response
file db servicePort cacheDir = do
    method POST
    rid <- look "id"
    if all isDigit rid
        then let reqId = read rid
            in do
            peer <- parseHost servicePort
            downloader <- liftIO $ getDownloaderIp db peer reqId

            req <- askRq
            result <- liftIO $ forwardFile downloader db peer reqId =<< takeRequestBody req
            case result of
                Left (Just e) -> badRequest $ toResponse e
                Left Nothing -> ok $ toResponse ("OK" :: String)
                Right b ->
                    case handleFaultyBody . decodeContent $ rawContent b of
                        Left err -> badRequest $ toResponse err
                        Right _ -> do
                            liftIO $ L.writeFile (cacheDir ++ rid) (JSON.encode b)
                            ok $ toResponse ("OK" :: String)
        else
            badRequest $ toResponse ("Error: Parameter 'id' must be all digits" :: String)

notFound :: ServerPart Response
notFound = do
    req <- askRq
    Happstack.Server.notFound . toResponse $ unwords ["Error: Cannot", show $ rqMethod req, rqUri req ++ rqQuery req]
