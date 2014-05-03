{-# LANGUAGE OverloadedStrings #-}

import Npm.Latest (fetchLatestVersion)
import System.Environment (getArgs)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO


main :: IO ()
main = do
    [name] <- getArgs
    version <- fetchLatestVersion name
    case version of
        Nothing -> TIO.putStrLn "Error: fetching/parsing JSON failed"
        Just v -> TIO.putStrLn $ T.unwords [T.pack name, v]
