{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DataKinds, DeriveDataTypeable, ExistentialQuantification #-}
module Npm.Latest.Internal (
    extractVersion,
    buildRequest,
    makeVersionRequest
) where

import Data.Typeable (Typeable)
import Control.Lens ((^?))
import Control.Exception (catchJust, Exception, SomeException, toException)
import Control.Monad (guard)
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.Aeson (json', Value)
import Data.Aeson.Lens (key, _String, AsValue)
import Data.Text.Format (Format, format)
import Network.URI (escapeURIString, isUnreserved)
import Network.HTTP.Client (HttpException(StatusCodeException))
import Pipes.Attoparsec (parse)
import Pipes.HTTP (parseUrl, withManager, tlsManagerSettings, withHTTP, responseBody, Request, Manager)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

data GenericNpmException = GenericNpmException
    deriving (Typeable, Show)
instance Exception GenericNpmException

extractVersion :: AsValue s => Either SomeException s -> Either SomeException T.Text
extractVersion json =
    json >>= \j -> maybe
        (Left $ toException GenericNpmException)
        (Right)
        (j ^? key "version" . _String)

buildRequest :: String -> Format -> IO Request
buildRequest name urlFormat =
    parseUrl $ TL.unpack $ format urlFormat [escapeURIString isUnreserved name]

makeVersionRequest :: Request -> IO (Either SomeException Value)
makeVersionRequest req =
    withManager tlsManagerSettings $ \mngr -> executeHTTPRequest req mngr


executeHTTPRequest :: Request -> Manager -> IO (Either SomeException Value)
executeHTTPRequest req mngr = do
     -- TODO: simplify
     catchJust (isStatusCodeException)
               (withHTTP req mngr $ \resp -> parseResponse resp >>= return . unwrapMaybe)
               (\e -> return $ Left $ toException e)
     where
        -- unwrapMaybe :: Maybe (Either a Value) -> Either SomeException Value
        unwrapMaybe = maybe
            (Left $ toException $ GenericNpmException)
            (\e -> case e of
                Left ex -> Left $ toException $ ex
                Right v -> Right v)

        parseResponse resp = evalStateT (parse json') (responseBody resp)

isStatusCodeException :: HttpException -> Maybe HttpException
isStatusCodeException e@(StatusCodeException _ _ _) = return e
isStatusCodeException _ = Nothing
