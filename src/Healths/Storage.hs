-- | Interfaces to Spock Connection Pool with Raw file handler
module Healths.Storage where

import Healths.Core     (AppState(..))
import qualified Data.ByteString.Lazy as B
import Data.Aeson (encode, decode', decode)
import Data.Time.Clock  (NominalDiffTime)
import System.IO        ( FilePath
                        , Handle
                        , IOMode(ReadWriteMode)
                        , openFile
                        , hClose
                        , hFlush)
import Web.Spock.Config ( ConnBuilder(..)
                        , PoolCfg(..)
                        , PoolOrConn(PCConn))

defaultPoolConfiguration :: PoolCfg
defaultPoolConfiguration = PoolCfg
  { pc_stripes = 10
  , pc_resPerStripe = 10
  , pc_keepOpenTime = 60000 :: NominalDiffTime
  }


fileConnectionBuilder :: FilePath -> ConnBuilder FilePath
fileConnectionBuilder p = ConnBuilder
  { cb_createConn = return p
  , cb_destroyConn = const $ return ()
  , cb_poolConfiguration = defaultPoolConfiguration
  }


fileConnection :: FilePath -> PoolOrConn FilePath
fileConnection = PCConn . fileConnectionBuilder

save :: AppState -> FilePath -> IO ()
save state p = B.writeFile p $ encode state

load :: FilePath -> IO (Maybe AppState)
load = fmap decode . B.readFile
