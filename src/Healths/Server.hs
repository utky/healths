{-# LANGUAGE OverloadedStrings #-}
module Healths.Server where

import Web.Spock
import Web.Spock.Config
import Network.Wai (Middleware)

import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Free (Free, foldFree)
import Control.Concurrent.STM ( TVar )
import Data.Time.Clock  (getCurrentTime, NominalDiffTime)
import Data.Text.Lazy (toStrict)
import Healths.Types
import Healths.Store
import Healths.JSONFile
import Healths.Core
import Healths.Config
import Healths.InfluxDB
import qualified Healths.View as V
import qualified Text.Blaze.Html.Renderer.Text as Renderer

data AppF a
  = Influx (InfluxM a)
  | Json (JSONCache a)

-- Delegating instanc implementation to underlying Monad

instance GetProfile AppF where
  getProfile = Json . getProfile

instance PutProfile AppF where
  putProfile uid = Json . putProfile uid

instance AppendHistory AppF where
  appendHistory uid = Influx . appendHistory uid

instance Functor AppF where
  fmap f (Influx m) = Influx $ fmap f m
  fmap f (Json m) = Json $ fmap f m

-- | Combined type with Free and Application
type AppM a = Free AppF a

-- | Run effect of application with given initial state and connection
runAppM :: TVar AppState -> AppConn -> AppF a -> IO a
runAppM _ (AppConn (infconf, _))  (Influx influx) =
  flip runReaderT infconf $ runInflux influx
runAppM tstate (AppConn (_, fileconf)) (Json js) =
  let cfg = CacheCtx { cacheConfig = fileconf, cacheState = tstate }
  in  flip runReaderT cfg $ runCache js

-- Wrapper of configuration for InfluxDB and JSON cache
newtype AppConn = AppConn (InfluxHandle, FileStoreConfig)

instance HasFileStore AppConn where
  getFileStoreConfig (AppConn (_, c)) = c


-- | Build spock connection pool with given configuration
makeConnection :: AppConn -> PoolOrConn AppConn
makeConnection = PCConn . connectionBuilder
  where
    connectionBuilder :: AppConn -> ConnBuilder AppConn
    connectionBuilder c = ConnBuilder
      { cb_createConn = return c
      , cb_destroyConn = const $ return ()
      , cb_poolConfiguration = defaultPoolConfiguration
      }
    defaultPoolConfiguration :: PoolCfg
    defaultPoolConfiguration = PoolCfg
      { pc_stripes = 1
      , pc_resPerStripe = 1
      , pc_keepOpenTime = 60000 :: NominalDiffTime
      }


-- | Alias of application specific monad
type ServerM = SpockM AppConn () (TVar AppState)

-- | Bind application with specified port
start :: Int -> HealthConfig -> IO ()
start port config = runSpock port (app config)

-- | Entrypoint of application
app :: HealthConfig -> IO Middleware
app config = do
  let dbHandle  = newInfluxHandle $ historyDatabaseHost config
      localDBConfig = FileStoreConfig $ localDatabasePath config
      conn = makeConnection $ AppConn (dbHandle, localDBConfig)
  state <- initAppState
  spockCfg <- defaultSpockCfg () conn state
  spock spockCfg routes


routes :: ServerM ()
routes = do

  get ("profiles" <//> var) $ \user -> do
    ts <- getState
    profile <- runQuery $ \conn ->
      foldFree (runAppM ts conn) $ fetchProfile user
    json profile

  post ("profiles" <//> var) $ \user -> do
    ts <- getState
    now <- liftIO getCurrentTime
    profile <- jsonBody'
    runQuery $ \conn ->
      foldFree (runAppM ts conn) $ saveProfile now user profile

  get ("edit" <//> var) $ \user -> do
    ts <- getState
    profile <- runQuery $ \conn ->
      foldFree (runAppM ts conn) $ fetchProfile user
    html $ toStrict $ Renderer.renderHtml $ V.profile user profile
