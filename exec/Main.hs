import Data.Maybe (fromJust)
import System.Environment (getArgs)
import Control.Monad (msum)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (try)
import Data.Foldable (forM_)

import System.Directory (createDirectoryIfMissing)

import Happstack.Server (ServerPart, Response, Conf(..), Host, dir, nullConf, simpleHTTP)
import qualified Data.Aeson as JSON
import qualified Database.SQLite.Simple as DB
import qualified Data.ByteString.Lazy as L

import Routes
import Handler.Client (StdResponse, HttpException, downloadFromURL, responseBody)
import qualified ConfigParser as C

{-
    TODO: Some sort of control mechanism: command line (with sockets) or HTTP server on different port
-}

handleRequest :: (String -> String) -> Int -> ServerPart Response
handleRequest conf port = msum [
        dir "download" $ download db port cacheDir (read $ conf "laziness"),
        dir "file" $ file db port cacheDir,
        notFound
    ]
    where db = conf "db"
          cacheDir = conf "cacheDir"


server :: Conf -> (String -> String) -> IO()
server serverConf conf = simpleHTTP serverConf . handleRequest conf $ port serverConf

splitToHost :: String -> Host
splitToHost s = (ip, read $ tail port)
    where (ip, port) = break (== ':') s

saveNeighbours :: String -> IO() -> Maybe [String] -> IO()
saveNeighbours _ c Nothing = c
saveNeighbours _ c (Just []) = c
saveNeighbours db continue (Just list) = do
    DB.withConnection db $ \dbc -> do
        DB.execute_ dbc
            "DELETE FROM neighbours"
        forM_ neighbours $ \(ip, port) ->
            DB.execute dbc
                "INSERT INTO neighbours (ip, port) VALUES (?, ?)"
                (ip, port)
    continue

    where neighbours = map splitToHost list

neighbourChecker :: String -> String -> IO()
neighbourChecker db dir
    | take 4 dir == "file" = do
        putStrLn "neighbours: Reading from file"
        L.readFile (drop 7 dir) >>= save
        putStrLn "neighbours: neighbours added"
    | otherwise = do
        putStrLn "neighbours: Pinging peer server..."
        res <- try (downloadFromURL dir) :: IO (Either HttpException StdResponse)
        case res of
            Left _ -> do
                putStrLn "neighbours: No response from peer server"
                continue
            Right resp -> do
                print $ responseBody resp
                save $ responseBody resp
                putStrLn "neighbours: neighbours added"

    where sleep m = threadDelay $ m * 60 * 1000 * 1000
          continue = do
              sleep 2
              neighbourChecker db dir
          save = saveNeighbours db continue . JSON.decode


defaultArgs :: [String] -> [String]
defaultArgs [] = ["server.conf"]
defaultArgs args = args


main = do
    _args <- getArgs
    let file:args = defaultArgs _args

    conf <- C.readConfig file

    let confLookup a = fromJust $ lookup a conf
        port = if null args then confLookup "port" else head args
        servConf = nullConf { port = read port :: Int }

    createDirectoryIfMissing True $ confLookup "cacheDir"

    putStrLn $ "Started server on port " ++ port

    forkIO $ neighbourChecker (confLookup "db") (confLookup "directory")
    server servConf confLookup
