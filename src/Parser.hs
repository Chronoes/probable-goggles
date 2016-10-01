module Parser (
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
renderHTTPDocument
)
where

import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import qualified Data.CaseInsensitive as CI

import ByteStringOps (trim)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as BS



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
splitHeadFromBody f b = case components of
        (h, [""]) -> (f h, Nothing)
        (h, body) -> (f h, Just . BS.unlines $ body)
    where components = break endOfHeader . BS.lines $ b


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
parseRequest b = Request (list !! 2) (method . parseMethod $ head list) (list !! 1)
    where list = BS.words b
          method (Left m) = GET
          method (Right m) = m

parseResponse b = Response protocol (status $ BS.readInt s)
    where (protocol, s) = BS.break (== ' ') b
          status (Just (c, m)) = mkStatus c m
          status Nothing = badRequest400

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
renderHTTPDocument ((r, h), Nothing) = renderHead r ((hContentLength, "0") : h)
renderHTTPDocument ((r, h), Just b) = renderHead r ((hContentLength, BS.pack . show $ BS.length b) : h) `BS.append` b
