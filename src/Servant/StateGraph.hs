{-| Provides tools to generate a graph from a 'servant' API.
-}

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

-- | What action 'stateGraph' should execute.
data StateGraphCommand = WriteJSON FilePath -- ^ Write a JSON representation to file.
                       | WriteJS FilePath -- ^ Write a JS representation ("var ApiOutput =" ++ JSON) to file.
                       | PrintJSON -- ^ Print JSON to stdout.
                       | Serve Int -- ^ Serve the graph viewer on the specified port.

-- | Configuration for 'stateGraph'.
data StateGraphConfig = StateGraphConfig
  { _command :: StateGraphCommand
  }

instance Default StateGraphConfig where
  def = StateGraphConfig (Serve 8090)

-- | Graph an API using the specified 'StateGraphConfig'.
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

-- | Graph an API using the default 'StateGraphConfig' (serve a viewer on port
-- 8090).
stateGraph' :: (Endpoints api ~ endpoints, EndpointsHaveGraph endpoints, LinksFor api) => Proxy api -> IO ()
stateGraph' = stateGraph def
