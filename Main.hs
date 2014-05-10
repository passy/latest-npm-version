{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

import Data.Data (Data)
import Data.Typeable (Typeable)
import Npm.Latest (fetchLatestVersion)
import System.Console.CmdArgs.Implicit (cmdArgsRun, (&=))
import System.Console.CmdArgs.Explicit (HelpFormat(..), helpText)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.Console.CmdArgs.Implicit as CA


data LatestNpmVersion = LatestNpmVersion {name :: String}
    deriving (Show, Data, Typeable)

args :: CA.Mode (CA.CmdArgs LatestNpmVersion)
args = CA.cmdArgsMode $ LatestNpmVersion{name = CA.def &= CA.args}
       &= CA.summary "latest-npm-version v0.2.1"

main :: IO ()
main = do
    mainArgs <- cmdArgsRun args
    let moduleName = (name mainArgs)
    if length moduleName == 0
        then print $ helpText [] HelpFormatDefault args
        else do
            version <- fetchLatestVersion moduleName
            TIO.putStrLn $ case version of
                Nothing -> "Error: fetching/parsing JSON failed"
                Just v -> T.unwords [T.pack moduleName, v]
