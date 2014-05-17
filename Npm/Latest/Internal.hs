{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DataKinds, DeriveDataTypeable, ExistentialQuantification #-}
module Npm.Latest.Internal (
    extractVersion,
    buildRequest,
    makeVersionRequest,
    GenericNpmException(..)
) where

import Data.Typeable (Typeable)
import Control.Lens ((^?))
import Control.Exception (catchJust, Exception, SomeException, toException)
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.Aeson (json', Value)
import Data.Aeson.Lens (key, _String, AsValue)
import Data.Text.Format (Format, format)
import Network.URI (escapeURIString, isUnreserved)
import Network.HTTP.Client (HttpException(StatusCodeException))
import Pipes.Attoparsec (parse, ParsingError)
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
        Right
        (j ^? key "version" . _String)

buildRequest :: String -> Format -> IO Request
buildRequest name urlFormat =
    parseUrl $ TL.unpack $ format urlFormat [escapeURIString isUnreserved name]

makeVersionRequest :: Request -> IO (Either SomeException Value)
makeVersionRequest req =
    withManager tlsManagerSettings $ \mngr -> executeHTTPRequest req mngr


executeHTTPRequest :: Request -> Manager -> IO (Either SomeException Value)
executeHTTPRequest req mngr =
     catchJust filterStatusCodeException
               (withHTTP req mngr ((unwrapMaybe `fmap`) . parseResponse))
               (return . Left . toException)
     where
        unwrapMaybe :: Maybe (Either ParsingError b) -> Either SomeException b
        unwrapMaybe = maybe
            (Left $ toException GenericNpmException)
            (either (Left . toException) Right)

        parseResponse resp = evalStateT (parse json') (responseBody resp)

filterStatusCodeException :: HttpException -> Maybe HttpException
filterStatusCodeException e@StatusCodeException{} = return e
filterStatusCodeException _ = Nothing
