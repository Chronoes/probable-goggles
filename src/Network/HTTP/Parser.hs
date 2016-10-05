module Network.HTTP.Parser (
StdMethod(..),
RequestResponse(..),
Header,
Body,
HTTPDocument,
httpProtocol,
newRequest,
newResponse,
splitHeadFromBody,
parseRequestHead,
parseResponseHead,
renderStdMethod,
renderHTTPDocument,
addHeader
)
where

import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Data.Maybe (fromMaybe)
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Char8 as BS

import Data.ByteString.Char8.Ops (trim)



type Body = BS.ByteString
type Head = (RequestResponse, [Header])
type HTTPDocument = (Head, Maybe Body)

data RequestResponse = Request {
    reqProtocol :: BS.ByteString,
    method :: StdMethod,
    uri :: BS.ByteString
} | Response {
    resProtocol :: BS.ByteString,
    status :: Status
} deriving (Eq, Show)

httpProtocol :: BS.ByteString
httpProtocol = "HTTP/1.1"

eol :: BS.ByteString -> BS.ByteString
eol = (`BS.append` "\r\n")

endOfHeader :: BS.ByteString -> Bool
endOfHeader "" = True
endOfHeader t = t == "\r"

splitHeadFromBody :: ([BS.ByteString] -> Head) -> BS.ByteString -> (Head, Maybe Body)
splitHeadFromBody f b = case body of
        [] -> (f h, Nothing)
        [""] -> (f h, Nothing)
        _ -> (f h, Just . BS.unlines $ body)
    where (h, b') = break endOfHeader . BS.lines $ b
          body = tail b'


parseHeader :: BS.ByteString -> Header
parseHeader s = (CI.mk name, trim $ BS.tail value)
    where (name, value) = BS.break (== ':') s

renderHeader :: Header -> BS.ByteString
renderHeader (n, v) = CI.original n `BS.append` ": " `BS.append` v

renderHeaders :: [Header] -> BS.ByteString
renderHeaders = BS.concat . map (eol . renderHeader)

hasHeader :: [Header] -> HeaderName -> Bool
infix 4 `hasHeader`
hasHeader [] _ = False
hasHeader _ "" = False
hasHeader xs s = elem s $ map fst xs


renderReqRes :: RequestResponse -> BS.ByteString
renderReqRes (Request p m u) = BS.unwords [renderStdMethod m, u, p]
renderReqRes (Response p s) = BS.unwords [p, BS.pack . show $ statusCode s, statusMessage s]

newRequest :: StdMethod -> BS.ByteString -> RequestResponse
newRequest = Request httpProtocol

newResponse :: Status -> RequestResponse
newResponse = Response httpProtocol

parseRequest, parseResponse :: BS.ByteString -> RequestResponse
parseRequest b = Request p (method $ parseMethod m) uri
    where [m, uri, p] = BS.words b
          method (Right m) = m
          method _ = GET


_parseStatus :: Maybe (Int, BS.ByteString) -> Status
_parseStatus (Just (c, m)) = mkStatus c m
_parseStatus Nothing = mkStatus 599 "Broken response"

parseStatus :: BS.ByteString -> Status
parseStatus = _parseStatus . BS.readInt

parseResponse b = Response protocol (parseStatus $ BS.unwords s)
    where protocol:s = BS.words b

parseHead :: (BS.ByteString -> RequestResponse) -> [BS.ByteString] -> Head
parseHead f req = (f $ head req, map parseHeader $ tail req)

parseRequestHead, parseResponseHead :: [BS.ByteString] -> Head
parseRequestHead = parseHead parseRequest

parseResponseHead = parseHead parseResponse

renderHead :: RequestResponse -> [Header] -> BS.ByteString
renderHead r = eol
    . BS.append (eol $ renderReqRes r)
    . renderHeaders

renderHTTPDocument :: HTTPDocument -> BS.ByteString
renderHTTPDocument ((r, h), Nothing) = renderHead r h
renderHTTPDocument ((r, h), Just b) = renderHead r ((hContentLength, BS.pack . show $ BS.length b) : h) `BS.append` b

addHeader :: Header -> HTTPDocument -> HTTPDocument
addHeader h ((r, hs), b) = ((r, h:hs), b)
