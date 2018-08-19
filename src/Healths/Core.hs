{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Healths.Core where

import qualified Data.Map.Strict as Map
import Data.Text
import Data.Aeson

{-
State

users:
  ilyaletre:
    weight: xxx
-}

type UserId = Text

data User = User
  { userId      :: !UserId
  , userProfile :: !Profile
  }
  deriving (Eq, Show)

-- | Kilogram
type Weight           = Float
-- |
type BMI              = Float
-- | Percentage
type BodyFat          = Float
-- | Kilogram
type Muscle           = Float
-- |
type VisceralFatLevel = Float

-- | Health tracker profile
data Profile = Profile
  { profileWeight           :: !Weight
  , profileBMI              :: !BMI
  , profileBodyFat          :: !BodyFat
  , profileMuscle           :: !Muscle
  , profileVisceralFatLevel :: !VisceralFatLevel
  }
  deriving (Eq, Show)

instance FromJSON Profile where
  parseJSON = withObject "Profile" $ \v -> Profile
    <$> v .: "weight"
    <*> v .: "bmi"
    <*> v .: "body_fat"
    <*> v .: "muscle"
    <*> v .: "visceral_fat_level"

instance ToJSON Profile where
  toJSON (Profile weight bmi bodyFat muscle visceralFatLevel) =
    object
      [ "weight"             .= weight
      , "bmi"                .= bmi
      , "body_fat"           .= bodyFat
      , "muscle"             .= muscle
      , "visceral_fat_level" .= visceralFatLevel
      ]

defaultProfile :: Profile
defaultProfile = Profile
  { profileWeight           = 60.0
  , profileBMI              = 21.75
  , profileBodyFat          = 17.0
  , profileMuscle           = 40.0
  , profileVisceralFatLevel = 8.0
  }


newtype AppState = AppState
  { appStateUsers :: Map.Map UserId Profile }
  deriving (Eq, Show)


instance FromJSON AppState where
  parseJSON = withObject "AppState" $ \v -> AppState
    <$> v .: "users"

instance ToJSON AppState where
  toJSON (AppState users) =
    object
      [ "users" .= users
      ]

instance Monoid AppState where
  mempty = AppState mempty
  mappend (AppState x) (AppState y) = AppState $ mappend x y

emptyState :: AppState
emptyState = mempty

putProfile :: UserId -> Profile -> AppState -> AppState
putProfile u p (AppState m) = AppState $ Map.insert u p m

getProfile :: UserId -> AppState -> Maybe Profile
getProfile u (AppState m) = Map.lookup u m
