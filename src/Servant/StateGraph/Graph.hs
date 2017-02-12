{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module Servant.StateGraph.Graph
  ( module Servant.StateGraph.Graph
  , module X
  ) where


import           Control.Monad.State

import           Control.Lens                          (use, uses, view, (%=),
                                                        (%~), (&), (.=), (<<+=),
                                                        (<>=), (<>~), (^.))

import qualified Control.Lens                          as L
import           Control.Lens.TH

import qualified Data.ByteString.Char8                 as B
import           Data.Graph.Inductive.Graph            (LEdge (..), LNode (..),
                                                        Node, empty, insEdge,
                                                        insNode, mkGraph)
import           Data.Graph.Inductive.PatriciaTree
import qualified Data.Map.Strict                       as Map
import           Data.Maybe                            (mapMaybe)
import qualified Data.Text                             as T

import           Data.Proxy                            (Proxy (..))
import           Data.Typeable                         (TypeRep, Typeable,
                                                        typeRep)
import           GHC.Generics
import           GHC.TypeLits


import qualified Network.HTTP.Types                    as HTTP
import           Servant.API

import           Servant.StateGraph.Graph.Links        as X
import           Servant.StateGraph.Graph.RichEndpoint as X
import           Servant.StateGraph.Graph.Types        as X

-- | State data for building up the graph.
data GraphStateData = GraphStateData
  { _typesToNodeIds :: Map.Map TypeRep Node
  , _nodeTypes      :: Map.Map TypeRep NodeType
  , _edgesQueue     :: [RichLink]
  , _currentNodeId  :: Int
  , _currentGraph   :: ApiGraph
  }
$(makeLenses ''GraphStateData)

type GraphState = State GraphStateData

-- | Color for each common type of request: green for GET, purple for POST, blue for PUT, red for DELETE.
verbColors :: StdMethod -> String
verbColors GET    = "green"
verbColors POST   = "purple"
verbColors PUT    = "blue"
verbColors DELETE = "red"
verbColors _      = "black"

-- | Generate a graph from a Servant API type.
graph :: (HasGraph api api, LinksFor api) => Proxy api -> ApiGraph
graph p = view currentGraph $ execState (graphFor p p >> connectLinks) startingState
  where
    startingState = GraphStateData { _typesToNodeIds = withRoot
                                   , _nodeTypes = nodeTypesFromLinks apiLinks
                                   , _edgesQueue = apiLinks
                                   , _currentNodeId = 1
                                   , _currentGraph = rootGraph
                                   }
    withRoot = Map.singleton (typeRep (Proxy :: Proxy Root)) 0
    rootGraph = mkGraph [(0, ApiNode "Root" NormalNode)] []
    apiLinks = linksFor p

mkEdge :: Map.Map TypeRep Node -> RichLink -> Maybe (LEdge ApiEdge)
mkEdge typesToNodeIds' l = do
  startNode <- Map.lookup sourceType typesToNodeIds'
  endNode <- Map.lookup targetType typesToNodeIds'
  return (startNode, endNode, ApiEdge rel (verbColors meth))
    where sourceType = l^.linkSource.returnType
          targetType = l^.linkTarget.returnType
          rel = l^.linkRel
          meth = l^.linkTarget.endpointMethod

connectLinks :: GraphState ()
connectLinks = do
  xs <- use edgesQueue
  typesToNodeIds' <- use typesToNodeIds
  let edges = mapMaybe (mkEdge typesToNodeIds') xs
  mapM_ (\x -> currentGraph %= insEdge x) edges

-- | Generate a graph for an API type.
class HasGraph root api where
  graphFor :: Proxy root -> Proxy api -> GraphState ()

-- Terminal instance for verbs (ending an endpoint path).
instance (Typeable a, ReflectMethod method) => HasGraph root (Verb method status ctypes a) where
  graphFor rootP _ = do
    let retType = typeRep (Proxy :: Proxy a)
    nodeType <- uses nodeTypes (Map.findWithDefault NormalNode retType)
    existingNode <- uses typesToNodeIds (Map.lookup retType)
    case existingNode of
      Just _ -> return ()
      Nothing -> do
        currentNodeId' <- currentNodeId <<+= 1
        let node = (currentNodeId', ApiNode (show retType) nodeType)
        currentGraph %= insNode node
        typesToNodeIds %= Map.insert retType currentNodeId'

 -- Extract one component of the path.
instance (KnownSymbol path, HasGraph root api) => HasGraph root (path :> api) where
  graphFor rootP _ = graphFor rootP subApiP
    where subApiP = Proxy :: Proxy api
          pa = Proxy :: Proxy path

instance (KnownSymbol sym, HasGraph root api)
      => HasGraph root (QueryParam sym a :> api) where
  graphFor rootP p = graphFor rootP subApiP
    where subApiP = Proxy :: Proxy api
          paramP = Proxy :: Proxy (QueryParam sym a)

instance (KnownSymbol sym, HasGraph root api) => HasGraph root (Capture sym a :> api) where
  graphFor rootP _ = graphFor rootP subApiP
    where subApiP = Proxy :: Proxy api
          sym' = Proxy :: Proxy sym

-- Compute subgraphs for each piece of the "alternatives" (:<|>) in an API type, then join them.
instance (HasGraph root a, HasGraph root b) => HasGraph root (a :<|> b) where
  graphFor rootP _ =
    graphFor rootP (Proxy :: Proxy a) >> graphFor rootP (Proxy :: Proxy b)
