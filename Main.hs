{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import Control.Lens ((^?))
import Pipes.HTTP (parseUrl, withManager, tlsManagerSettings, withHTTP, responseBody, Request)
import Network.URI (escapeURIString, isUnreserved)
import Data.Text.Format (Format, format)
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.Aeson (json')
import Data.Aeson.Lens (key, _String, AsValue)
import Pipes.Attoparsec (parse)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL


latestUrl :: Format
latestUrl = "https://registry.npmjs.org/{}/latest"


main :: IO ()
main = do
    [name] <- getArgs
    req <- buildRequest name latestUrl
    makeVersionRequest req

extractVersion :: AsValue s => Maybe (Either t s) -> Maybe T.Text
extractVersion json =
    json >>= either (const Nothing) (^? key "version" . _String)

buildRequest :: String -> Format -> IO Request
buildRequest name urlFormat =
    parseUrl $ TL.unpack $ format urlFormat [escapeURIString isUnreserved name]

-- How do I lift something out of the inner monad so I can use it in main (or an
-- exposed module function for that matter.)
makeVersionRequest :: Request -> IO ()
makeVersionRequest req =
    withManager tlsManagerSettings $ \mngr ->
        withHTTP req mngr $ \resp -> do
            json <- evalStateT (parse json') (responseBody resp)
            print $ extractVersion json
