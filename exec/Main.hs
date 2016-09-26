import Network.Simple.TCP
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import System.Environment

import Parser
import Handler

server :: String -> IO()
server port = serve HostAny port $ \(connectionSocket, remoteAddr) -> do
    putStrLn $ "TCP connection established from " ++ show remoteAddr
    req <- recv connectionSocket 1024

    let (request, body) = splitHeadFromBody . fromMaybe BS.empty $ req
    let (req, headers) = parseRequestAndHeaders request
    print req
    print headers
    let (res, headers, body) = handleRequest (req, headers, body)

    send connectionSocket $ makeHTTPDocument (res, headers, body)


main = do
    args <- getArgs
    case args of
        [] -> server "1215"
        _ -> server . head $ args
