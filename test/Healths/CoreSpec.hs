{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Healths.CoreSpec where

import Control.Monad.State
import qualified Data.Map as Map
import Healths.Types
import Healths.Store
import Healths.Core
import qualified Data.Text as T
import Data.Time
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

type TestStore = Map.Map UserId Profile

newtype CoreSpecM a = CoreSpecM
  { runSpecM :: State TestStore a}
  deriving (Functor, Applicative, Monad, MonadState TestStore)

instance GetProfile CoreSpecM where
  getProfile u = do
    m <- get
    return $ Map.lookup u m

instance PutProfile CoreSpecM where
  putProfile u p = do
    modify (Map.insert u p)

instance AppendHistory CoreSpecM where
  appendHistory _ _ = return ()


instance Arbitrary Day where
    arbitrary = ModifiedJulianDay <$> arbitrary

instance Arbitrary DiffTime where
    arbitrary = secondsToDiffTime <$> arbitrary

instance Arbitrary UTCTime where
    arbitrary = UTCTime <$> arbitrary <*> arbitrary

instance Arbitrary Profile where
    arbitrary
      = Profile
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

spec :: Spec
spec =
  describe "Core" $ do
    describe "Profile" $ do
      it "should be default value with no saved state" $ property $
        \k -> let f = fetchProfile (T.pack k) :: CoreSpecM Profile
                  actual = evalState (runSpecM f) Map.empty
              in  actual == defaultProfile
      it "can be fetched as saved" $ property $
        \now k p ->
          let k' = T.pack k
              f = saveProfile now k' p >> fetchProfile k'
              actual = evalState (runSpecM f) Map.empty
          in  actual == p
