module Handler.Response where

import qualified Data.ByteString.Lazy.Char8 as L
import Happstack.Server (Host)

import Handler.Client (StdResponse, responseBody)


handleResponse :: Host -> StdResponse -> IO()
handleResponse (ip, _) r = print $ L.unlines [L.pack $ "Response from " ++ ip, responseBody r]
