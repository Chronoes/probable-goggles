{-# LANGUAGE OverloadedStrings #-}

module Parser (
MainHeader(..),
Header,
getRequestAndHeaders,
getResponseAndHeaders,
makeRequest,
makeResponse
)
where


import Data.Char
import Data.Maybe
import qualified Data.ByteString.Char8 as BS

trim :: BS.ByteString -> BS.ByteString
trim "" = ""
trim b = (fst . BS.spanEnd isSpace) . BS.dropWhile isSpace $ b

eol :: BS.ByteString -> BS.ByteString
eol = (`BS.append` "\r\n")

endOfHeader :: BS.ByteString -> Bool
endOfHeader t = BS.null t || t == "\r"


data Header = Header {
    name :: BS.ByteString,
    value :: BS.ByteString
} deriving (Eq, Ord, Show)

parseToHeader :: BS.ByteString -> Header
parseToHeader b = Header name (trim . BS.tail $ value)
    where (name, value) = BS.break (== ':') b

headerToString :: Header -> BS.ByteString
headerToString h = name h `BS.append` ": " `BS.append` value h

headersToString :: [Header] -> BS.ByteString
headersToString = BS.concat . map (eol . headerToString)


data MainHeader = Request {
    method :: BS.ByteString,
    uri :: BS.ByteString,
    reqProtocol :: BS.ByteString
} | Response {
    resProtocol :: BS.ByteString,
    statusCode :: Int
} deriving (Eq, Show)



parseRequest :: BS.ByteString -> MainHeader
parseRequest b = Request (head list) (list !! 1) (list !! 2)
    where list = BS.words b

parseResponse :: BS.ByteString -> MainHeader
parseResponse b = Response protocol (fst . fromMaybe (0, "") . BS.readInt $ status)
    where (protocol, status) = BS.break (== ' ') b


getMainHeaderAndHeaders :: (BS.ByteString -> MainHeader) -> [BS.ByteString] -> (MainHeader, [Header])
getMainHeaderAndHeaders f req = (f . head $ req, map parseToHeader . takeWhile (not . endOfHeader) . tail $ req)

getRequestAndHeaders = getMainHeaderAndHeaders parseRequest

getResponseAndHeaders = getMainHeaderAndHeaders parseResponse


makeRequest :: MainHeader -> [Header] -> BS.ByteString
makeRequest req = eol
    . BS.append (eol . BS.unwords $ [method req, uri req, reqProtocol req])
    . headersToString

getStatusText :: Int -> BS.ByteString
getStatusText 200 = "200 OK"
getStatusText _ = error "Status code does not exist"

makeResponse :: MainHeader -> [Header] -> BS.ByteString
makeResponse r = eol
    . BS.append (eol . BS.unwords $ [resProtocol r, getStatusText . statusCode $ r])
    . headersToString
