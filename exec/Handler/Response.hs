module Handler.Response where

import Happstack.Server (Response)

-- TODO: Do I need to handle this? marked just in case
handleResponse :: Response -> IO()
handleResponse = print
-- handleResponse ((r, _), Nothing) = print r
-- handleResponse ((r, _), Just b) = putStrLn $ show r ++ "\n" ++ show b
