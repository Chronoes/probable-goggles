module Main where

import System.Environment (getArgs)
import Data.Maybe (fromJust)

import Control.Monad (msum)
import Happstack.Server (ServerPart, Response, Conf(..), Host, dir, nullConf, simpleHTTP)

import ConfigParser
import Routes

handleRequest :: (String -> String) -> Int -> ServerPart Response
handleRequest conf port = msum [
        dir "download" $ download port,
        notFound
    ]
    where db = conf "db"


server :: Conf -> (String -> String) -> Int -> IO()
server serverConf conf servicePort = simpleHTTP serverConf $ handleRequest conf servicePort


defaultArgs :: [String] -> [String]
defaultArgs [] = ["server.conf"]
defaultArgs args = args


main = do
    _args <- getArgs
    let file:args = defaultArgs _args

    conf <- readConfig file

    let confLookup a = fromJust $ lookup a conf
        servicePort = read $ confLookup "port"
        servConf = nullConf { port = read $ head args }

    putStrLn $ "Started client on port " ++ show (port servConf)

    server servConf confLookup servicePort
