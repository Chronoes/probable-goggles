module Logger (
logToStdout,
formatString
) where

import Data.Time

import Control.Monad.IO.Class (liftIO)
import Happstack.Server (ServerPart)

formatString :: String -> IO String
formatString s = do
    time <- getCurrentTime
    let formattedTime = formatTime defaultTimeLocale "%F %T" time
    return $ unwords ['[':formattedTime ++ "]", s]

logToStdout :: String -> ServerPart()
logToStdout a = liftIO $ putStrLn =<< formatString a
