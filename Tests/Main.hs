{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Control.Exception (SomeException(..), toException)
import Npm.Latest (extractVersion, GenericNpmException(..))

import qualified Data.Text as T

instance Eq SomeException where
    -- Terrible hack, but this is for tests, so it's fine, right?
    a == b = show a == show b

main :: IO ()
main = hspec $ do
    describe "extractVersion" $ do
        it "is Right for a valid JSON object" $ do
            let json = T.unpack "{\"version\": \"1.0\"}"
            extractVersion (Right json) `shouldBe` (Right "1.0")

        it "is Left for a malformed JSON object" $ do
            let json = T.unpack "{\"description\": \"not what we expect\"}"
            extractVersion (Right $ json) `shouldBe`
                (Left $ toException GenericNpmException)

        it "is Left for a malformed JSON object" $ do
            let json = T.unpack "not even $ JSON"
            extractVersion (Right $ json) `shouldBe`
                (Left $ toException GenericNpmException)
