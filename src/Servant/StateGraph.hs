{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
module Servant.StateGraph
  ( module Servant.StateGraph
  , module Servant.StateGraph.Graph
  , module Servant.StateGraph.Graph.JSON
  ) where

import qualified Data.ByteString.Lazy as BL
import Data.Default
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Typeable
import Servant.StateGraph.Graph
import Servant.StateGraph.Graph.JSON
import Servant.StateGraph.Graph.Server

data StateGraphCommand = WriteJSON FilePath
                  | WriteJS FilePath
                  | PrintJSON
                  | Serve Int

data StateGraphConfig = StateGraphConfig
  { _command :: StateGraphCommand
  }

instance Default StateGraphConfig where
  def = StateGraphConfig (Serve 8090)

stateGraph :: (HasGraph api api, LinksFor api) => StateGraphConfig -> Proxy api -> IO ()
stateGraph (StateGraphConfig {_command}) api =
  case _command of
    WriteJSON path -> BL.writeFile path json
    WriteJS path -> BL.writeFile path js
    PrintJSON -> BL.putStr json
    Serve port -> serveGraph (graph api) port
  where
    json = graphToJSON (graph api)
    js = "var ApiOutput = " <> json <> ";"

stateGraph' :: (HasGraph api api, LinksFor api) => Proxy api -> IO ()
stateGraph' = stateGraph def
