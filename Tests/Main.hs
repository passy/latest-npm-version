{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Npm.Latest (extractVersion)

import qualified Data.Text as T

main :: IO ()
main = hspec $ do
    describe "extractVersion" $ do
        it "returns Just for a valid JSON object" $ do
            let json = T.unpack "{\"version\": \"1.0\"}"
            extractVersion (Just $ Right $ json) `shouldBe` Just "1.0"

        it "returns Nothing for a malformed JSON object" $ do
            let json = T.unpack "{\"description\": \"not what we expect\"}"
            extractVersion (Just $ Right $ json) `shouldBe` Nothing

        it "returns Nothing for a malformed JSON object" $ do
            let json = T.unpack "not even $ JSON"
            extractVersion (Just $ Right $ json) `shouldBe` Nothing
