import Data.Maybe (fromJust)
import System.Environment (getArgs)
import Control.Monad (msum)

import Data.Text.Lazy (unpack)
import Happstack.Server (dir, nullConf, simpleHTTP, ServerPart, Response, Conf(..))

import Handler.Request
-- import Handler.Client
import qualified ConfigParser as C

{-
    TODO: Thread to periodically contact directory service (data saved to DB)
-}

handleRequest :: (String -> String) -> ServerPart Response
handleRequest conf = msum [
        dir "download" $ download db (read $ conf "laziness" :: Float),
        dir "file" $ file db,
        notFound
    ]
    where db = conf "db"


server :: Conf -> [C.Config] -> IO()
server serverConf conf = simpleHTTP serverConf $ handleRequest confLookup
    where confLookup a = fromJust $ lookup a conf

readConfig :: String -> IO [C.Config]
readConfig f = do
    conf <- readFile f
    return $ C.parseFile conf


defaultArgs :: [String] -> [String]
defaultArgs [] = ["server.conf"]
defaultArgs args = args


main = do
    _args <- getArgs
    let file:args = defaultArgs _args

    conf <- readConfig file

    let port = if null args then fromJust $ lookup "port" conf else head args

    putStrLn $ "Started server on port " ++ port

    let servConf = nullConf { port = read port :: Int }
    server servConf conf
