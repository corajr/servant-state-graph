{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Servant.StateGraph.Graph.Types where

import Servant.API (StdMethod(..))
import Servant.API.TypeLevel
import Data.Graph.Inductive.PatriciaTree
import Data.Typeable
import Control.Lens.TH

-- | A marker for the Root node.
data Root

type instance IsElem' Root api = ()

data NodeType = NormalNode
              | TargetNode
              | ErrorNode
              deriving (Eq, Show, Ord)

data ApiNode = ApiNode
  { apiNodeName :: String
  , apiNodeType :: NodeType
  } deriving (Eq, Show, Ord)

data ApiEdge = ApiEdge
  { apiEdgeName :: String
  , apiEdgeColor :: String
  } deriving (Eq, Show, Ord)

type ApiGraph = Gr ApiNode ApiEdge

type SourceType = TypeRep
type TargetType = TypeRep

data ApiLink = ApiLink
  { _segments :: [String]
  , _queryParams :: [String]
  }
$(makeLenses ''ApiLink)

emptyLink :: ApiLink
emptyLink = ApiLink mempty mempty

data RichEndpoint = RichEndpoint
  { _apiLink :: ApiLink
  , _endpointMethod :: StdMethod
  , _returnType :: TypeRep
  }
$(makeLenses ''RichEndpoint)


