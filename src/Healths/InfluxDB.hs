{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Implementation of store using InfluxDB
module Healths.InfluxDB where

import qualified Database.InfluxDB as Influx
import Healths.Types
import Healths.Store
import Data.String (fromString)
import Control.Lens
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, MonadReader, asks)
import qualified Data.Text as T
import qualified Data.Map as Map

type DatabaseName = T.Text

defaultDatabase :: Influx.Database
defaultDatabase = "healths"

defaultMeasurement :: Influx.Measurement
defaultMeasurement = "profiles"

data InfluxHandle
  = InfluxHandle
  { server :: Influx.Server
  , database :: Influx.Database
  , queryParam :: Influx.QueryParams
  , writeParam :: Influx.WriteParams
  }

newInfluxHandle
  :: T.Text -- ^ Server hostname
  -> InfluxHandle
newInfluxHandle hostname
  = InfluxHandle
  { server = newServer
  , database = newDatabase
  , queryParam = newQueryParams
  , writeParam = newWriteParams
  }
  where
    newServer = Influx.defaultServer & Influx.host .~ hostname
    newDatabase = defaultDatabase
    newQueryParams =
      Influx.queryParams newDatabase
        & Influx.server .~ newServer
    newWriteParams =
      Influx.writeParams newDatabase
        & Influx.server .~ newServer
        & Influx.precision .~ Influx.Second


newtype InfluxM a
  = InfluxM { runInflux :: ReaderT InfluxHandle IO a }
  deriving (Functor, Applicative, Monad, MonadReader InfluxHandle, MonadIO)

influxWrite ::  UserId -> History Profile -> InfluxM ()
influxWrite u (History t p) = do
  wp <- asks writeParam
  liftIO $ Influx.write wp $
    Influx.Line
      defaultMeasurement
      (Map.fromList
        [ ("user_id", fromString $ T.unpack u)
        ])
      (Map.fromList
        [ ("weight",             Influx.FieldFloat (profileWeight p))
        , ("bmi",                Influx.FieldFloat (profileBMI p))
        , ("body_fat",           Influx.FieldFloat (profileBodyFat p))
        , ("muscle",             Influx.FieldFloat (profileMuscle p))
        , ("visceral_fat_level", Influx.FieldFloat (profileVisceralFatLevel p))
        ])
      (Just t)

instance AppendHistory InfluxM where
  appendHistory = influxWrite

