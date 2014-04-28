{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import Pipes.HTTP (parseUrl, withManager, tlsManagerSettings, withHTTP)
import Network.URI (escapeURIString, isAllowedInURI, isUnreserved)
import Data.Text.Format (Format, format)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as TIO


latestUrl :: Format
latestUrl = "https://registry.npmjs.org/{}/latest"


main :: IO ()
main = do
    [name] <- getArgs
    putStrLn $ "Fetching latest version of " ++ name
    let url = format latestUrl [escapeURIString isUnreserved name]
    TIO.putStrLn $ TL.toStrict url
    req <- parseUrl $ TL.unpack url
    withManager tlsManagerSettings $ \mngr ->
        withHTTP req mngr $ \resp -> do
            putStrLn $ "resp: " ++ "yo"

    putStrLn "yo"
