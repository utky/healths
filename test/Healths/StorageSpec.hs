{-# LANGUAGE OverloadedStrings #-}
module Healths.StorageSpec where

import Healths.Core
import Healths.Storage
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Storage" $
    describe "AppState" $
      it "should be idempotent between save and load" $ do
        let filepath = "test.json"
        save emptyState filepath
        result <- load filepath
        result `shouldBe` Just emptyState
