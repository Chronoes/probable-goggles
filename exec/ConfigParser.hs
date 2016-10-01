module ConfigParser where

import Data.Char (isSpace)
import Data.String (IsString)

type Config = (String, String)

trim :: String -> String
trim "" = ""
trim b = reverse . dropSpace . reverse $ dropSpace b
    where dropSpace = dropWhile isSpace

splitKeyValue :: String -> Config
splitKeyValue s = (trim key, trim $ tail val)
    where (key, val) = break (== '=') s

isCommentOrEmpty :: String -> Bool
isCommentOrEmpty "" = True
isCommentOrEmpty (x:xs) = x /= ';'

parseFile :: String -> [Config]
parseFile = map splitKeyValue . filter isCommentOrEmpty . lines
