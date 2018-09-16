{-# LANGUAGE OverloadedStrings #-}
module Main where

import Healths.Config (HealthConfig(..))
import Healths.Server (start)

main :: IO ()
main =
  let cfg = HealthConfig { historyDatabaseHost = "localhost"
                         , localDatabasePath = "data.json"}
  in  start 8080 cfg
