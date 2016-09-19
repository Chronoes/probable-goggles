import Network.Simple.TCP
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

import Parser

main = serve (Host "localhost") "1337" $ \(connectionSocket, remoteAddr) -> do
    putStrLn $ "TCP connection established from " ++ show remoteAddr
    req <- recv connectionSocket 1024

    let request = BS.lines . fromMaybe BS.empty $ req
    print . getRequestAndHeaders $ request

    send connectionSocket (BS.pack "HTTP/1.1 200 OK\r\n\r\n")
