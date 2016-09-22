{-# LANGUAGE OverloadedStrings #-}

module Parser (
RequestResponse(..),
Header,
splitHeadersFromBody,
parseRequestAndHeaders,
parseResponseAndHeaders,
showAllHeaders
)
where

import ByteStringOps
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as BS


eol :: BS.ByteString -> BS.ByteString
eol = (`BS.append` "\r\n")

endOfHeader :: BS.ByteString -> Bool
endOfHeader t = BS.null t || t == "\r"

splitHeadersFromBody :: BS.ByteString -> ([BS.ByteString], [BS.ByteString])
splitHeadersFromBody = break endOfHeader . BS.lines


data Header = Header {
    name :: BS.ByteString,
    value :: BS.ByteString
} deriving (Eq, Ord, Show)

instance ShowBS Header where
    showBS (Header n v) = n `BS.append` ": " `BS.append` v

instance ReadBS Header where
    readBS a = Header name (trim . BS.tail $ value)
        where (name, value) = BS.break (== ':') a


headersToString :: [Header] -> BS.ByteString
headersToString = BS.concat . map (eol . showBS)


data RequestResponse = Request {
    method :: BS.ByteString,
    uri :: BS.ByteString,
    reqProtocol :: BS.ByteString
} | Response {
    resProtocol :: BS.ByteString,
    statusCode :: Int
} deriving (Eq, Show)


instance ShowBS RequestResponse where
    showBS (Request m u p) = BS.unwords [m, u, p]
    showBS (Response p s) = BS.unwords [p, getStatusText s]


parseRequest :: BS.ByteString -> RequestResponse
parseRequest b = Request (head list) (list !! 1) (list !! 2)
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
