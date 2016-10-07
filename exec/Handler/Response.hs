module Handler.Response where

import Handler.Client (StdResponse, responseBody)

-- TODO: Do I need to handle this? marked just in case
handleResponse :: StdResponse -> IO()
handleResponse = print . responseBody
