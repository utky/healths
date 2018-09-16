{-# LANGUAGE OverloadedStrings #-}
-- | Interfaces to Spock Connection Pool with Raw file handler
module Healths.Store where

import Control.Monad.Free (Free, liftF)
import Healths.Types     (Profile, UserId)
import Data.Time (UTCTime)
import Data.Aeson

-- | Alias of UTCTIme
type Time = UTCTime

-- | Time tagged data
data History a = History !Time a

instance Eq a => Eq (History a) where
  (History t1 x1) == (History t2 x2) = t1 == t2 && x1 == x2

instance Show a => Show (History a) where
  show (History t x) = show t ++ ": " ++ show x

instance Eq a => Ord (History a) where
  compare (History t1 _) (History t2 _) = compare t1 t2


data Filter
  = Filter
  { filterUserId :: !UserId
  , filterCount  :: !Integer
  , filterStart  :: !(Maybe Time)
  , filterEnd    :: !(Maybe Time)
  }
  deriving (Eq, Show)


instance FromJSON Filter where
   parseJSON = withObject "Filter" $ \v -> Filter
    <$> v .: "user_id"
    <*> v .: "count"
    <*> v .: "start"
    <*> v .: "end"


instance ToJSON Filter where
  toJSON (Filter userId count start end) =
    object
      [ "user_id" .= userId
      , "count"   .= count
      , "start"   .= start
      , "end"     .= end
      ]


-- | Syntax for appending profile data into history
class AppendHistory m where
  appendHistory :: UserId -> History Profile -> m ()


instance (AppendHistory f, Functor f) => AppendHistory (Free f) where
  appendHistory u h = liftF $ appendHistory u h


-- | Syntax for fetching profile data from somewhere
class GetProfile m where
  getProfile :: UserId -> m (Maybe Profile)


instance (GetProfile f, Functor f) => GetProfile (Free f) where
  getProfile u = liftF $ getProfile u


-- | Syntax for adding profile data into somewhere
class PutProfile m where
  putProfile :: UserId -> Profile -> m ()


instance (PutProfile f, Functor f) => PutProfile (Free f) where
  putProfile u p = liftF $ putProfile u p
