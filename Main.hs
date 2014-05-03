{-# LANGUAGE OverloadedStrings #-}

import Control.Lens ((^?))
import Control.Monad (liftM)
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.Aeson (json', Value)
import Data.Aeson.Lens (key, _String, AsValue)
import Data.Text.Format (Format, format)
import Network.URI (escapeURIString, isUnreserved)
import Pipes.Attoparsec (parse, ParsingError)
import Pipes.HTTP (parseUrl, withManager, tlsManagerSettings, withHTTP, responseBody, Request)
import System.Environment (getArgs)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL


latestUrl :: Format
latestUrl = "https://registry.npmjs.org/{}/latest"


main :: IO ()
main = do
    [name] <- getArgs
    req <- buildRequest name latestUrl
    resp <- makeVersionRequest req
    let version = extractVersion resp
    print version

extractVersion :: AsValue s => Maybe (Either t s) -> Maybe T.Text
extractVersion json =
    json >>= either (const Nothing) (^? key "version" . _String)

buildRequest :: String -> Format -> IO Request
buildRequest name urlFormat =
    parseUrl $ TL.unpack $ format urlFormat [escapeURIString isUnreserved name]

makeVersionRequest :: Request -> IO (Maybe (Either ParsingError Value))
makeVersionRequest req =
    withManager tlsManagerSettings $ \mngr ->
        withHTTP req mngr $ \resp -> do
            evalStateT (parse json') (responseBody resp)
