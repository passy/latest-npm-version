{-# LANGUAGE OverloadedStrings #-}

module Npm.Latest (
    fetchLatestVersion,
    extractVersion
) where

import Npm.Latest.Internal (buildRequest, makeVersionRequest, extractVersion)
import Data.Text.Format (Format)
import qualified Data.Text as T

latestUrl :: Format
latestUrl = "https://registry.npmjs.org/{}/latest"

fetchLatestVersion :: String -> IO (Maybe T.Text)
fetchLatestVersion name = do
    req <- buildRequest name latestUrl
    resp <- makeVersionRequest req
    return $ extractVersion resp
