{-# LANGUAGE OverloadedStrings #-}
module Healths.CoreSpec where

import Healths.Core
import Test.Hspec

main :: IO ()
main = hspec spec

 
spec :: Spec
spec =
  describe "Core" $ do
    describe "Profile" $
      it "should be idempotent in default value" $
        defaultProfile `shouldBe` defaultProfile
    describe "AppState" $
      it "should be idempotent in empty value" $
        emptyState `shouldBe` emptyState
