module Parser (
Method(..),
RequestResponse(..),
Header(..),
Body,
HTTPDocument,
httpProtocol,
newRequest,
newResponse,
splitHeadFromBody,
parseRequestAndHeaders,
parseResponseAndHeaders,
makeHTTPDocument
)
where

import ByteStringOps
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as BS


type Body = BS.ByteString
type HTTPDocument = (RequestResponse, [Header], Maybe Body)

eol :: BS.ByteString -> BS.ByteString
eol = (`BS.append` "\r\n")

endOfHeader :: BS.ByteString -> Bool
endOfHeader t = BS.null t || t == "\r"

splitHeadFromBody :: BS.ByteString -> ([BS.ByteString], Maybe Body)
splitHeadFromBody b = case components of
        (h, [""]) -> (h, Nothing)
        (h, body) -> (h, Just . BS.unlines $ body)
    where components = break endOfHeader . BS.lines $ b


data Header = Header {
    headerName :: BS.ByteString,
    headerValue :: BS.ByteString
} deriving (Eq, Ord, Show)

instance ShowBS Header where
    showBS (Header n v) = n `BS.append` ": " `BS.append` v

instance ReadBS Header where
    readBS a = Header name (trim . BS.tail $ value)
        where (name, value) = BS.break (== ':') a


headersToString :: [Header] -> BS.ByteString
headersToString = BS.concat . map (eol . showBS)

hasHeader :: [Header] -> BS.ByteString -> Bool
infix 4 `hasHeader`
hasHeader [] _ = False
hasHeader _ "" = False
hasHeader xs s = elem s . map headerName $ xs

httpProtocol :: BS.ByteString
httpProtocol = "HTTP/1.1"

data Method = GET | POST deriving (Eq, Show, Read)

instance ShowBS Method where
    showBS GET = "GET"
    showBS POST = "POST"

instance ReadBS Method where
    readBS "GET" = GET
    readBS "POST" = POST

data RequestResponse = Request {
    reqProtocol :: BS.ByteString,
    method :: Method,
    uri :: BS.ByteString
} | Response {
    resProtocol :: BS.ByteString,
    statusCode :: Int
} deriving (Eq, Show)

instance ShowBS RequestResponse where
    showBS (Request m u p) = BS.unwords [m, showBS u, p]
    showBS (Response p s) = BS.unwords [p, getStatusText s]


newRequest = Request httpProtocol
newResponse = Response httpProtocol

parseRequest :: BS.ByteString -> RequestResponse
parseRequest b = Request (list !! 2) (readBS . head $ list) (list !! 1)
    where list = BS.words b

parseResponse :: BS.ByteString -> RequestResponse
parseResponse b = Response protocol (fst . fromMaybe (0, "") . BS.readInt $ status)
    where (protocol, status) = BS.break (== ' ') b

parseReqResAndHeaders :: (BS.ByteString -> RequestResponse) -> [BS.ByteString] -> (RequestResponse, [Header])
parseReqResAndHeaders f req = (f . head $ req, map readBS .  tail $ req)

parseRequestAndHeaders = parseReqResAndHeaders parseRequest

parseResponseAndHeaders = parseReqResAndHeaders parseResponse

getStatusText :: Int -> BS.ByteString
getStatusText 200 = "200 OK"
getStatusText 400 = "400 Bad Request"
getStatusText 404 = "404 Not Found"
getStatusText _ = error "Status code does not exist"

showAllHeaders :: RequestResponse -> [Header] -> BS.ByteString
showAllHeaders r = eol
    . BS.append (eol . showBS $ r)
    . headersToString

makeHTTPDocument :: HTTPDocument -> BS.ByteString
makeHTTPDocument (r, h, Nothing) = showAllHeaders r (Header "Content-Length" "0" : h)
makeHTTPDocument (r, h, Just b) = showAllHeaders r (Header "Content-Length" (BS.pack . show . BS.length $ b) : h) `BS.append` b
