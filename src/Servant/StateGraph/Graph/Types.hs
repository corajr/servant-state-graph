{-| Core types for representing nodes and edges in the state graph.
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module Servant.StateGraph.Graph.Types where

import           Control.Lens.TH
import           Data.Graph.Inductive.Graph        (Node)
import           Data.Graph.Inductive.PatriciaTree
import qualified Data.Map.Strict                   as Map
import           Data.Typeable
import           Network.URI                       (URI)
import           Servant.API                       (StdMethod (..))
import           Servant.API.TypeLevel

-- | A marker for the Root node.
data Root

type instance IsElem' Root api = ()

-- * Graph Types

-- | Types of a state node.
data NodeType = NormalNode -- ^ An ordinary node.
              | TargetNode -- ^ A node representing a target state.
              | ErrorNode -- ^ A node representing an error state.
              deriving (Eq, Show, Ord)

-- | An 'ApiNode' contains the name of the state node (the type of its
-- representation) and its 'NodeType' (whether it is a normal, target, or error
-- state).
data ApiNode = ApiNode
  { apiNodeName :: String
  , apiNodeType :: NodeType
  } deriving (Eq, Show, Ord)


-- | An 'ApiEdge' contains the name of the link (by default, the URI traversed)
-- and its color: see 'verbColors' for list.
data ApiEdge = ApiEdge
  { apiEdgeName  :: String
  , apiEdgeColor :: String
  } deriving (Eq, Show, Ord)

-- | Alias for a PatriciaTrie 'Gr' graph.
type ApiGraph = Gr ApiNode ApiEdge

-- | Like 'Link', but exposing its constructor for manipulation.
data ApiLink = ApiLink
  { _segments    :: [String]
  , _queryParams :: [String]
  } deriving (Eq, Show)
$(makeLenses ''ApiLink)

-- | An empty 'ApiLink'.
emptyLink :: ApiLink
emptyLink = ApiLink mempty mempty

-- | Contains information about an endpoint, including a link to reach it, its
-- HTTP 'StdMethod', and its return type.
data RichEndpoint = RichEndpoint
  { _apiLink        :: ApiLink
  , _endpointMethod :: StdMethod
  , _returnType     :: TypeRep
  } deriving (Eq, Show)
$(makeLenses ''RichEndpoint)

-- | Stores the source and target 'RichEndpoint's, the target 'URI', 'NodeType',
-- and name of the link relation.
data RichLink = RichLink
  { _linkSource         :: RichEndpoint
  , _linkTarget         :: RichEndpoint
  , _linkTargetURI      :: URI
  , _linkTargetNodeType :: NodeType
  , _linkRel            :: String
  }

$(makeLenses ''RichLink)

-- | State data for building up the graph.
data GraphStateData = GraphStateData
  { _typesToNodeIds :: Map.Map TypeRep Node
  , _nodeTypes      :: Map.Map TypeRep NodeType
  , _edgesQueue     :: [RichLink]
  , _currentNodeId  :: Int
  , _currentGraph   :: ApiGraph
  }
$(makeLenses ''GraphStateData)

