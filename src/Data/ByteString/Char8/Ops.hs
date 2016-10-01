module Data.ByteString.Char8.Ops where

import Data.Char (isSpace)
import qualified Data.ByteString.Char8 as BS

trim :: BS.ByteString -> BS.ByteString
trim "" = ""
trim b = (fst . BS.spanEnd isSpace) . BS.dropWhile isSpace $ b
