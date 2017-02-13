{-| Generate a graph from an API type built up using 'Servant.API' combinators.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Servant.StateGraph.Graph
  ( module Servant.StateGraph.Graph
  , module Servant.StateGraph.Graph.Links
  , module Servant.StateGraph.Graph.RichEndpoint
  , module Servant.StateGraph.Graph.Types
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
import           Servant.API.TypeLevel

import           Servant.StateGraph.Graph.Links
import           Servant.StateGraph.Graph.RichEndpoint
import           Servant.StateGraph.Graph.Types

-- * Graph building

-- | Data for building the graph in the 'State' monad.
type GraphState = State GraphStateData

-- | Color for each common type of request: green for GET, purple for POST, blue for PUT, red for DELETE.
verbColors :: StdMethod -> String
verbColors GET    = "green"
verbColors POST   = "purple"
verbColors PUT    = "blue"
verbColors DELETE = "red"
verbColors _      = "black"

endpoints :: Proxy api -> Proxy (Endpoints api)
endpoints _ = Proxy

-- | Generate a graph from a Servant API type.
graph :: (Endpoints api ~ xs, EndpointsHaveGraph xs,  LinksFor api) => Proxy api -> ApiGraph
graph p = view currentGraph $ execState (graphEndpoints (endpoints p) >> connectLinks) startingState
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

-- | Turns a 'RichLink' into a 'LEdge ApiEdge', if possible.
mkEdge :: Map.Map TypeRep Node -> RichLink -> Maybe (LEdge ApiEdge)
mkEdge typesToNodeIds' l = do
  startNode <- Map.lookup sourceType typesToNodeIds'
  endNode <- Map.lookup targetType typesToNodeIds'
  return (startNode, endNode, ApiEdge rel (verbColors meth))
    where sourceType = l^.linkSource.returnType
          targetType = l^.linkTarget.returnType
          rel = l^.linkRel
          meth = l^.linkTarget.endpointMethod

-- | Connects the edges in the 'GraphStateData' edge queue.
connectLinks :: GraphState ()
connectLinks = do
  xs <- use edgesQueue
  typesToNodeIds' <- use typesToNodeIds
  let edges = mapMaybe (mkEdge typesToNodeIds') xs
  mapM_ (\x -> currentGraph %= insEdge x) edges

-- * Typeclasses

-- | Generate a graph node from an endpoint.
class HasGraph endpoint where
  graphFor :: Proxy endpoint -> GraphState ()

-- Extract one component of the endpoint.
instance (HasGraph subEndpoint) => HasGraph (e :> subEndpoint) where
  graphFor _ = graphFor (Proxy :: Proxy subEndpoint)

-- Terminal instance for verbs (ending an endpoint path).
instance (Typeable a, ReflectMethod method) => HasGraph (Verb method status ctypes a) where
  graphFor _ = do
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

-- | Transform a type-level list of endpoints into a graph.
class EndpointsHaveGraph xs where
  graphEndpoints :: proxy xs -> GraphState ()

instance EndpointsHaveGraph '[] where
  graphEndpoints _ = pure ()

instance (HasGraph x, EndpointsHaveGraph xs) => EndpointsHaveGraph (x ': xs) where
  graphEndpoints _ = graphFor (Proxy :: Proxy x) >> graphEndpoints (Proxy :: Proxy xs)
