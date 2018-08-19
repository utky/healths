{-# LANGUAGE OverloadedStrings #-}
module Healths.ServerSpec where

import Healths.Core
import Healths.Server
import Test.Hspec
import Test.Hspec.Wai
import Web.Spock (spockAsApp)
import Data.Aeson (encode)

main :: IO ()
main = hspec spec

matchWithEq :: (Eq a, Show a) => a -> a -> Maybe String
matchWithEq expected actual
  | expected == actual = Nothing
  | otherwise = Just $ "Response body mismatch: " ++ (show actual)

spec :: Spec
spec =
  with (spockAsApp (app "test.json")) $
    describe "Server" $
      describe "profile" $
        it "should be idempotent between post and then get" $ do
          let body = encode $ Profile
                              { profileWeight           = 90.0
                              , profileBMI              = 11.75
                              , profileBodyFat          = 27.0
                              , profileMuscle           = 30.0
                              , profileVisceralFatLevel = 7.0
                              }
          post "/profiles/test" body `shouldRespondWith` 200
          get "profiles/test" `shouldRespondWith` 200 { matchBody = MatchBody (\_ b -> matchWithEq body b)}

