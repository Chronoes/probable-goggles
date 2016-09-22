import Network.Simple.TCP
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

import Parser

main = serve HostAny "1337" $ \(connectionSocket, remoteAddr) -> do
    putStrLn $ "TCP connection established from " ++ show remoteAddr
    req <- recv connectionSocket 1024

    let (request, body) = splitHeadersFromBody . fromMaybe BS.empty $ req
    print . parseRequestAndHeaders $ request

    send connectionSocket $ showAllHeaders (Response (BS.pack "HTTP/1.1") 200) []
