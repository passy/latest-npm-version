{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

import Paths_latest_npm_version (version)
import Control.Exception (fromException)
import Network.HTTP.Client (HttpException(StatusCodeException))
import Data.Data (Data)
import Data.Version (showVersion)
import Data.Typeable (Typeable)
import Npm.Latest (fetchLatestVersion, GenericNpmException(..))
import System.Console.CmdArgs.Implicit (cmdArgsRun, (&=))
import System.Console.CmdArgs.Explicit (HelpFormat(..), helpText)

import qualified Data.Text as T
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
                    formatError
                    id

    where
        formatError e =
            T.pack $ "FATAL: " ++ case fromException e of
                Just GenericNpmException -> "Unknown error"
                _ -> case fromException e of
                    Just (s@StatusCodeException{}) -> "HTTP error " ++ show s
                    _ -> "Unknown error"
