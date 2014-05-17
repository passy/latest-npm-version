{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Npm.Latest
Description : Fetch the latest version of an npm module.
Copyright   : (c) Pascal Hartig, 2014
License     : MIT
Maintainer  : phartig@rdrei.net
Stability   : experimental
Portability : POSIX

This is just an experiment of me porting a node module to Haskell and learning
about Lenses and Pipes along the way. Use on your own risk.
-}
module Npm.Latest (
    fetchLatestVersion,
    extractVersion,
    GenericNpmException(..)
) where

import Control.Exception (SomeException)
import Data.Text.Format (Format)
import Npm.Latest.Internal (buildRequest, makeVersionRequest, extractVersion, GenericNpmException(..))
import qualified Data.Text as T

latestUrl :: Format
latestUrl = "https://registry.npmjs.org/{}/latest"

-- |Fetch the latest version for the given module name.
fetchLatestVersion :: String -> IO (Either SomeException T.Text)
fetchLatestVersion name = do
    req <- buildRequest name latestUrl
    resp <- makeVersionRequest req
    return $ extractVersion resp
