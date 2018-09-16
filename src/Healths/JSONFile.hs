{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Store of profile based on Spock data structure
module Healths.JSONFile where

import Data.Aeson

import Healths.Types
import Healths.Store
import Control.Monad ((<=<))
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Reader
import Control.Concurrent.STM ( TVar
                              , atomically , modifyTVar
                              , readTVar
                              , readTVarIO
                              , newTVarIO
                              )
import System.Directory (doesFileExist)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as B


-- |
newtype FileStoreConfig
  = FileStoreConfig
  { location :: FilePath
  }
  deriving (Eq, Show)


-- | Encapsulation layer for actual TVar state
newtype HealthState
  = HealthState
  { appState  :: TVar AppState
  }
  deriving (Eq)



-- |
---0type InnerM conn = SpockAction conn () HealthState
--newtype HealthM conn a
--  = HealthM
--  { runHealth :: InnerM conn a  }
--  deriving ( Functor
--           , Applicative
--           , Monad
--           , MonadIO
--           )
--
--liftSpock :: InnerM conn a -> HealthM conn a
--liftSpock = HealthM

class HasFileStore a where
  getFileStoreConfig :: a -> FileStoreConfig


-- |
newtype FileStore a
  = FileStore
  { unStore :: ReaderT FileStoreConfig IO a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader FileStoreConfig
           , MonadIO
           )


initAppState :: IO (TVar AppState)
initAppState = newTVarIO emptyState

-- | Load application state saved in file system
-- This could be used for building initial state of application
loadState :: (MonadIO m) => FilePath -> m (Maybe AppState)
loadState = liftIO . fmap decode . B.readFile


-- | Read profile in application state from memory
-- This could be used for building initial state of application
readProfile :: (MonadIO m) => FilePath -> UserId -> m (Maybe Profile)
readProfile loc uid =
  liftIO $ parseProfile <$> B.readFile loc
  where
    parseProfile :: B.ByteString -> Maybe Profile
    parseProfile = (Map.lookup uid . appStateUsers) <=< decode

data CacheCtx
  = CacheCtx
  { cacheConfig :: !FileStoreConfig
  , cacheState :: !(TVar AppState)
  }

newtype JSONCache a
  = JSONCache
  { runCache :: ReaderT CacheCtx IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader CacheCtx)

instance GetProfile JSONCache where
  getProfile uid = do -- JSONCache
    tstate <- asks cacheState
    liftIO $ atomically $ do -- IO
      s <- readTVar tstate
      return $ Map.lookup uid $ appStateUsers s

instance PutProfile JSONCache where
  putProfile uid profile = do -- JSONCache
    CacheCtx { cacheConfig = c, cacheState = ts } <- ask
    liftIO $ do -- IO
      atomically $ modifyTVar ts swapProfile
      modifiedState <- readTVarIO ts
      B.writeFile (location c) $ encode modifiedState
    where
      swapProfile = AppState . Map.insert uid  profile . appStateUsers
