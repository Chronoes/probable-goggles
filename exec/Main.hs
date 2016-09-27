import Network.Simple.TCP
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

import Parser
import Handler

server :: ServiceName -> IO()
server port = serve HostAny port $ \(connectionSocket, remoteAddr) -> do
    putStrLn $ "TCP connection established from " ++ show remoteAddr
    req <- recv connectionSocket 1024

    let doc = splitHeadFromBody parseRequestHead . fromMaybe BS.empty $ req
    let ((request, headers), body) = doc
    print request
    print headers
    let res = handleRequest doc

    send connectionSocket $ makeHTTPDocument res


main = do
    args <- getArgs
    case args of
        [] -> server "1215"
        _ -> server . head $ args
