module Healths.Config where

import Data.Aeson
import qualified Data.Text as T

data HealthConfig
  = HealthConfig
  { historyDatabaseHost :: T.Text
  , localDatabasePath :: FilePath
  }
