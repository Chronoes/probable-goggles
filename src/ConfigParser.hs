module ConfigParser where

import Data.Char (isSpace)

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
isCommentOrEmpty (x:xs) = x == ';'

parseFile :: String -> [Config]
parseFile = map splitKeyValue . filter (not . isCommentOrEmpty) . lines

readConfig :: FilePath -> IO [Config]
readConfig f = do
    conf <- readFile f
    return $ parseFile conf
