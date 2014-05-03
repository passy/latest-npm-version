{-# LANGUAGE OverloadedStrings #-}

module Npm.Latest (
    fetchLatestVersion
) where

import Control.Lens ((^?))
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.Aeson (json', Value)
import Data.Aeson.Lens (key, _String, AsValue)
import Data.Text.Format (Format, format)
import Network.URI (escapeURIString, isUnreserved)
import Pipes.Attoparsec (parse, ParsingError)
import Pipes.HTTP (parseUrl, withManager, tlsManagerSettings, withHTTP, responseBody, Request)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL


latestUrl :: Format
latestUrl = "https://registry.npmjs.org/{}/latest"

extractVersion :: AsValue s => Maybe (Either t s) -> Maybe T.Text
extractVersion json =
    json >>= either (const Nothing) (^? key "version" . _String)

buildRequest :: String -> Format -> IO Request
buildRequest name urlFormat =
    parseUrl $ TL.unpack $ format urlFormat [escapeURIString isUnreserved name]

makeVersionRequest :: Request -> IO (Maybe (Either ParsingError Value))
makeVersionRequest req =
    withManager tlsManagerSettings $ \mngr ->
        withHTTP req mngr $ \resp ->
            evalStateT (parse json') (responseBody resp)

fetchLatestVersion :: String -> IO (Maybe T.Text)
fetchLatestVersion name = do
    req <- buildRequest name latestUrl
    resp <- makeVersionRequest req
    return $ extractVersion resp
