-- | Exposes core of use cases in business domain
module Healths.Core where

import Healths.Types
import Healths.Store


-- | Return saved profile or default profile
fetchProfile
  :: ( Monad m
     , GetProfile m
     )
  => UserId
  -> m Profile
fetchProfile uid = do
  profile <- getProfile uid
  case profile of
    Nothing  -> return defaultProfile
    (Just p) -> return p

-- | Save profile to specified context and save its history
saveProfile
  :: ( Monad m
     , PutProfile m
     , AppendHistory m
     )
  => Time -> UserId -> Profile -> m ()
saveProfile time userId profile = do
  -- Save current data into storage
  putProfile userId profile
  -- And also save as history
  appendHistory userId $ History time profile
