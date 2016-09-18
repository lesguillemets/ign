{-# LANGUAGE OverloadedStrings #-}
module Download
    (
        ignDownload
    )
    where


import Network.HTTP.Simple (httpLBS)
import Network.HTTP.Conduit
import Network.HTTP.Types (statusCode)

import Data.ByteString.Lazy.Char8 (pack, ByteString)
import Data.Monoid


downloadUrl :: String -> String
downloadUrl fileType =
    "https://raw.githubusercontent.com/github/gitignore/master/"
    <> fileType <> ".gitignore"

ignDownload :: String -> IO (Maybe ByteString)
ignDownload f = do
    url <- parseRequest (downloadUrl f)
    resp <- httpLBS url
    case statusCode . responseStatus $ resp of
         200 -> return (Just . responseBody $ resp)
         _ -> return Nothing
