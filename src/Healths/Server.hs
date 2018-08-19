{-# LANGUAGE OverloadedStrings #-}
module Healths.Server where

import Web.Spock
import Web.Spock.Config
import Network.Wai (Middleware)

import Control.Monad.Trans
import Data.IORef
import Data.Monoid
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.IO (Handle, FilePath)
import Healths.Core
import Healths.Storage

data MySession = EmptySession
newtype MyAppState = DummyAppState (IORef Int)

start :: Int -> FilePath -> IO ()
start port dataFile = runSpock port (app dataFile)

app :: FilePath -> IO Middleware
app dataFile =
    do ref <- newIORef 0
       spockCfg <- defaultSpockCfg EmptySession (fileConnection dataFile) (DummyAppState ref)
       spock spockCfg routes

index :: T.Text
index = T.pack $ concat 
  [ "<!DOCTYPE html>"
  , "<html>"
  , "<head>"
  , "<meta charset=\"UTF-8\">"
  , "<title>Health tracker</title>"
  , "</head>"
  , "<body>"
  , "<form>"
  , "</form>"
  , "</body>"
  , "</html>"
  ]

routes :: SpockM FilePath MySession MyAppState ()
routes = do
  get root $
      text "Hello World!"
  get ("profiles" <//> var) $ \user -> do
    profile <- runQuery $ \conn -> do
      state <- load conn
      return $ fromMaybe defaultProfile (state >>= getProfile user)
    json profile
  post ("profiles" <//> var) $ \user -> do
    profile <- jsonBody'
    runQuery $ \conn -> do
      state <- load conn
      maybe (return ()) (flip save conn . putProfile user profile) state
