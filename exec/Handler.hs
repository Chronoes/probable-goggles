module Handler where

import Parser
import Network.HTTP.Types.URI
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Data.Text.Encoding (encodeUtf8)


-- TODO: start implementation: how to store requests for later use?
handleDownload :: QueryText -> HTTPDocument
handleDownload q = (newResponse 200, [Header "Content-Type" "text/html; charset=utf-8"], Just . BS.append "OK\n" . encodeUtf8 . fromJust . snd . head $ q)

-- TODO: fix this temp assignment
handleFile = handleDownload

handleNotFound :: HTTPDocument
handleNotFound = (newResponse 404, [Header "Content-Type" "text/html; charset=utf-8"], Just "Error: No such path")

handleRequest :: HTTPDocument -> HTTPDocument
handleRequest (r, h, b) = case (path, method r) of
    ("/download", GET) -> handleDownload query
    ("/file", POST) -> handleFile query
    _ -> handleNotFound
    where (path, queryString) = BS.break (== '?') . uri $ r
          query = parseQueryText . BS.tail $ queryString
