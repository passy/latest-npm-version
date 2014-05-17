{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, ExistentialQuantification #-}

import Paths_latest_npm_version (version)
import Control.Exception (fromException, Exception, SomeException)
import Network.HTTP.Client (HttpException(StatusCodeException))
import Network.HTTP.Types.Status (statusCode)
import Data.Data (Data)
import Data.Version (showVersion)
import Data.Typeable (Typeable)
import Npm.Latest (fetchLatestVersion, GenericNpmException(..))
import System.Console.CmdArgs.Implicit (cmdArgsRun, (&=))
import System.Console.CmdArgs.Explicit (HelpFormat(..), helpText)
import System.IO (stderr)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.Console.CmdArgs.Implicit as CA


data ErrHandler a = forall e . Exception e => ErrHandler (e -> a)

matchErr :: SomeException -> b -> [ErrHandler b] -> b
matchErr e = foldr (\(ErrHandler f) r -> maybe r f $ fromException e)

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
    let moduleName = name mainArgs
    if null moduleName
        then print $ helpText [] HelpFormatDefault args
        else fetchLatestVersion moduleName >>= either handleErr TIO.putStrLn

handleErr :: SomeException -> IO ()
handleErr err = TIO.hPutStrLn stderr $ matchErr err handleOther
    [ErrHandler handleA, ErrHandler handleB] where
        handleA GenericNpmException = "Invalid JSON data received."
        handleB (StatusCodeException status _ _) =
            if statusCode status == 404
                then "Package not found."
                else T.unwords
                    ["Invalid status code ", T.pack $ show $ statusCode status]
        handleB s@_ = T.unwords ["HTTP error: ", T.pack $ show s]
        handleOther = "Unknown error."
