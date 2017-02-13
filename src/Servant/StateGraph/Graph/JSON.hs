{-| Export functionality for Cytoscape.js.
-}
{-# LANGUAGE TemplateHaskell #-}
module Servant.StateGraph.Graph.JSON where

import qualified Data.ByteString.Lazy as BL
import Data.Graph.Inductive.Graph
import Servant.StateGraph.Graph.Types
import Data.Aeson
import Data.Aeson.TH

-- | A record storing data for Cytoscape.js.
newtype CytoData a = CytoData
  { _data :: a } deriving (Eq, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1, unwrapUnaryRecords = False} ''CytoData)

-- | Node data for Cytoscape.js.
data NodeData = NodeData
  { _id :: String
  , _name :: String
  , _noun :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''NodeData)

-- | Edge data for Cytoscape.js.
data EdgeData = EdgeData
  { _source :: String
  , _target :: String
  , _label :: String
  , _color :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''EdgeData)

-- | A graph in Cytoscape.js format.
data CytoGraph = CytoGraph
  { _nodes :: [CytoData NodeData]
  , _edges :: [CytoData EdgeData]
  } deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''CytoGraph)

-- | Convert an 'ApiGraph' to 'CytoGraph'.
graphToCyto :: ApiGraph -> CytoGraph
graphToCyto gr = CytoGraph cNodes cEdges
  where cNodes = map nodeToCyto (labNodes gr)
        cEdges = map edgeToCyto (labEdges gr)
        nodeColor NormalNode = "lightgrey"
        nodeColor ErrorNode = "red"
        nodeColor TargetNode = "green"
        nodeToCyto (i, ApiNode name nodeType) = CytoData (NodeData ('n' : show i) name (nodeColor nodeType))
        edgeToCyto (i, j, ApiEdge name color) = CytoData (EdgeData ('n' : show i) ('n' : show j) name color)

-- | Output an 'ApiGraph' as Cytoscape.js JSON.
graphToJSON :: ApiGraph -> BL.ByteString
graphToJSON = encode . graphToCyto
