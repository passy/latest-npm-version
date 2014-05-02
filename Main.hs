{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import Control.Lens (_Right, (^.), (^?))
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Pipes (runEffect, (>->))
import Pipes.HTTP (parseUrl, withManager, tlsManagerSettings, withHTTP, responseBody)
import Network.URI (escapeURIString, isAllowedInURI, isUnreserved)
import Data.Text.Format (Format, format)
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.Aeson (json')
import Data.Aeson.Lens (key, _String)
import Pipes.Attoparsec (parse)

import qualified Pipes.ByteString as PB
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
            json <- evalStateT (parse json') (responseBody resp)
            case json of
                Nothing -> putStrLn "Nope"
                Just v -> case v of
                    Left x -> putStrLn "Left"
                    Right y -> do
                        let version = y ^? key (T.pack "version") . _String
                        putStrLn $ show version
    putStrLn "yo"
