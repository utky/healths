{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-} {-# LANGUAGE FlexibleInstances #-} -- | Store of profile based on Spock data structure
module Healths.Spock where

import Data.Aeson
import Web.Spock

import Healths.Types
import Healths.Store
import Control.Monad ((<=<))
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, MonadReader, asks)
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
type InnerM conn = SpockAction conn () HealthState
newtype HealthM conn a
  = HealthM
  { runHealth :: InnerM conn a  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           )

liftSpock :: InnerM conn a -> HealthM conn a
liftSpock = HealthM

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


initHealthState :: IO HealthState
initHealthState = HealthState <$> newTVarIO emptyState

-- | Load application state saved in file system
loadState :: (MonadIO m) => FilePath -> m (Maybe AppState)
loadState = liftIO . fmap decode . B.readFile


-- | Read profile in application state from memory
readProfile :: (MonadIO m) => FilePath -> UserId -> m (Maybe Profile)
readProfile loc uid =
  liftIO $ parseProfile <$> B.readFile loc
  where
    parseProfile :: B.ByteString -> Maybe Profile
    parseProfile = (Map.lookup uid . appStateUsers) <=< decode


instance (HasFileStore conn) => GetProfile (HealthM conn) where
  getProfile uid = do -- HealthM
    tstate <- liftSpock $ appState  <$> getState
    liftIO $ atomically $ do -- IO
      state <- readTVar tstate
      return $ Map.lookup uid $ appStateUsers state

instance (HasFileStore conn) => PutProfile (HealthM conn) where
  putProfile uid profile = do -- HealthM
    (HealthState state) <- liftSpock getState
    newState <- liftIO $ do -- IO
      atomically $ modifyTVar state swapProfile
      readTVarIO state
    liftSpock $ runQuery $ \c -> do
      let loc = location $ getFileStoreConfig c
      B.writeFile loc $ encode newState
    where
      swapProfile = AppState . Map.insert uid  profile . appStateUsers

instance GetProfile FileStore where
  getProfile userId = do -- FileStore
    loc <- asks location
    liftIO $ do -- IO
      exists <- doesFileExist loc
      if exists
        then readProfile loc userId
        else return Nothing
