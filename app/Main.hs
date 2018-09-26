{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Healths.Config (HealthConfig(..))
import Healths.Server (start)
import Data.Monoid ((<>))
import qualified Options.Applicative as Opt
import qualified System.Environment as Env
import qualified Data.Map as Map
import qualified Data.Text as T

type Env = Map.Map String String
env :: IO Env
env = Map.fromList <$> Env.getEnvironment

getPort :: Env -> Int
getPort = read . Map.findWithDefault "8080" "HEALTHS_PORT"

getInflux :: Env -> T.Text
getInflux = read . Map.findWithDefault "localhost" "HEALTHS_INFLUXHOST"

getDatabase :: Env -> FilePath
getDatabase = read . Map.findWithDefault "data.json" "HEALTHS_DATABASE"

options :: Int -> T.Text -> FilePath -> Opt.Parser HealthConfig
options p i d
  = HealthConfig
  <$> Opt.option Opt.auto
        ( Opt.value p
        <> Opt.long "port"
        <> Opt.short 'p'
        <> Opt.help "TCP port number for web server"
        <> Opt.metavar "HEALTHS_PORT"
        )
  <*> Opt.strOption
        ( Opt.value i
        <> Opt.long "influxhost"
        <> Opt.short 'i'
        <> Opt.help "Hostname of InfluxDB"
        <> Opt.metavar "HEALTHS_INFLUXHOST"
        )
  <*> Opt.strOption
        ( Opt.value d
        <> Opt.long "database"
        <> Opt.short 'd'
        <> Opt.help "File path for local state database"
        <> Opt.metavar "HEALTHS_DATABASE"
        )

main :: IO ()
main = do
  e <- env
  let p = getPort e
      i = getInflux e
      d = getDatabase e
  cfg <- Opt.execParser (Opt.info (options p i d) Opt.idm)
  start cfg
