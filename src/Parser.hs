module Parser (
getRequestAndHeaders
)
where

import Data.Char
import qualified Data.ByteString.Char8 as BS


data Header = Header {
    name :: BS.ByteString,
    value :: BS.ByteString
} deriving (Eq, Ord, Show)

parseToHeader :: BS.ByteString -> Header
parseToHeader bytes = Header name (BS.init . BS.tail $ value)
    where (name, value) = BS.break (== ':') bytes


data Request = Request {
    method :: BS.ByteString,
    uri :: BS.ByteString,
    protocol :: BS.ByteString
} deriving (Eq, Show)

parseRequest :: BS.ByteString -> Request
parseRequest bytes = Request (head list) (list !! 1) (list !! 2)
    where list = BS.words bytes


notEndOfHeader :: BS.ByteString -> Bool
notEndOfHeader = (`notElem` [BS.singleton '\r', BS.empty])


getRequestAndHeaders :: [BS.ByteString] -> (Request, [Header])
getRequestAndHeaders req = (parseRequest . head $ req, map parseToHeader . takeWhile notEndOfHeader . tail $ req)
