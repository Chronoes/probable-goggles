module Handler.Response where

import qualified Data.ByteString.Lazy.Char8 as L
import Happstack.Server (Host)

import Handler.Client (StdResponse, responseBody)
import Logger

handleResponse :: Host -> StdResponse -> IO()
handleResponse (ip, _) r = putStrLn =<< formatString (unlines ["Response from " ++ ip, L.unpack $ responseBody r])
