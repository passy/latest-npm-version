{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

import Data.Data (Data)
import Data.Typeable (Typeable)
import Npm.Latest (fetchLatestVersion)
import System.Console.CmdArgs.Implicit (cmdArgsRun, (&=))

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.Console.CmdArgs.Implicit as CA


data LatestNpmVersion = LatestNpmVersion {name :: String}
    deriving (Show, Data, Typeable)

args :: CA.Mode (CA.CmdArgs LatestNpmVersion)
args = CA.cmdArgsMode $ LatestNpmVersion{name = CA.def &= CA.args}
       &= CA.summary "latest-npm-version v0.1.0"

main :: IO ()
main = do
    mainArgs <- cmdArgsRun args
    let moduleName = (name mainArgs)
    if length moduleName == 0
        then TIO.putStrLn "Missing module name. Consult --help."
        else do
            version <- fetchLatestVersion moduleName
            case version of
                Nothing -> TIO.putStrLn "Error: fetching/parsing JSON failed"
                Just v -> TIO.putStrLn $ T.unwords [T.pack moduleName, v]
