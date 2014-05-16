{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

import Paths_latest_npm_version (version)
import Data.Data (Data)
import Data.Version (showVersion)
import Data.Typeable (Typeable)
import Npm.Latest (fetchLatestVersion)
import System.Console.CmdArgs.Implicit (cmdArgsRun, (&=))
import System.Console.CmdArgs.Explicit (HelpFormat(..), helpText)

import qualified Data.Text.IO as TIO
import qualified System.Console.CmdArgs.Implicit as CA


data LatestNpmVersion = LatestNpmVersion {name :: String}
    deriving (Show, Data, Typeable)

programName :: String
programName = "latest-npm-version"

args :: CA.Mode (CA.CmdArgs LatestNpmVersion)
args = CA.cmdArgsMode $ LatestNpmVersion{name = CA.def &= CA.args}
       &= CA.summary (unwords [programName, showVersion version])
       &= CA.program programName

main :: IO ()
main = do
    mainArgs <- cmdArgsRun args
    let moduleName = (name mainArgs)
    if length moduleName == 0
        then print $ helpText [] HelpFormatDefault args
        else
            fetchLatestVersion moduleName >>=
                TIO.putStrLn . either
                    (\_ -> "Error: fetching/parsing JSON failed")
                    id
