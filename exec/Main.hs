import Network.Simple.TCP
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import System.Environment (getArgs)

import Network.HTTP.Parser
import Handler.Request
import Handler.Response
import Handler.Client
import qualified ConfigParser as C

{-
    TODO: Thread to periodically contact directory service (data saved to DB)
-}

server :: ServiceName -> [C.Config] -> IO()
server port conf = serve HostAny port $ \(connectionSocket, remoteAddr) -> do
    putStrLn $ "TCP connection established from " ++ show remoteAddr
    req <- recv connectionSocket 1024

    let doc = splitHeadFromBody parseRequestHead $ fromJust req
    let ((request, headers), body) = doc
    print request
    print headers
    res <- handleRequest (getIp remoteAddr, port) doc

    send connectionSocket . renderHTTPDocument $ addUserAgent res

    where handleRequest = Handler.Request.handleRequest confLookup
          confLookup a = fromJust $ lookup a conf
          getIp = takeWhile (/= ':') . show

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
    server port conf
