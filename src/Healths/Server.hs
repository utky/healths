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
import Data.Text.Lazy (toStrict)
import System.IO (Handle, FilePath)
import Healths.Core
import Healths.Storage
import qualified Healths.View as V
import qualified Text.Blaze.Html.Renderer.Text as Renderer

data MySession = EmptySession
newtype MyAppState = DummyAppState (IORef Int)

start :: Int -> FilePath -> IO ()
start port dataFile = runSpock port (app dataFile)

app :: FilePath -> IO Middleware
app dataFile =
    do ref <- newIORef 0
       spockCfg <- defaultSpockCfg EmptySession (fileConnection dataFile) (DummyAppState ref)
       spock spockCfg routes


routes :: SpockM FilePath MySession MyAppState ()
routes = do

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

  get ("edit" <//> var) $ \user -> do
    profile <- runQuery $ \conn -> do
      state <- load conn
      return $ fromMaybe defaultProfile (state >>= getProfile user)
    html $ toStrict $ Renderer.renderHtml $ V.profile user profile
