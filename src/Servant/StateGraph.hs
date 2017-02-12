{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
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
import Servant.API.TypeLevel
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

stateGraph :: (Endpoints api ~ endpoints, EndpointsHaveGraph endpoints, LinksFor api) => StateGraphConfig -> Proxy api -> IO ()
stateGraph (StateGraphConfig {_command}) api =
  case _command of
    WriteJSON path -> BL.writeFile path json
    WriteJS path -> BL.writeFile path js
    PrintJSON -> BL.putStr json
    Serve port -> serveGraph gr port
  where
    gr = graph api
    json = graphToJSON gr
    js = "var ApiOutput = " <> json <> ";"

stateGraph' :: (Endpoints api ~ endpoints, EndpointsHaveGraph endpoints, LinksFor api) => Proxy api -> IO ()
stateGraph' = stateGraph def
