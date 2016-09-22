{-# LANGUAGE OverloadedStrings #-}

module ByteStringOps where

import Data.Char (isSpace)
import qualified Data.ByteString.Char8 as BS

class ShowBS a where
    showBS :: a -> BS.ByteString

class ReadBS a where
    readBS :: BS.ByteString -> a


trim :: BS.ByteString -> BS.ByteString
trim "" = ""
trim b = (fst . BS.spanEnd isSpace) . BS.dropWhile isSpace $ b
