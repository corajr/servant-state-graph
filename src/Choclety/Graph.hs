{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Choclety.Graph where


import qualified Data.ByteString.Char8 as B
import Control.Lens.TH

import GHC.Generics
import Data.Typeable
import Data.Maybe
import Data.List
import qualified Data.Text as T
import Data.Proxy (Proxy(..))
import Data.Graph.Inductive.Graph (empty, insNode, insEdge, mkGraph, Node, LEdge(..), LNode(..))
import Data.Graph.Inductive.PatriciaTree
import Control.Lens (view, use, uses, (.=), (<>=), (<<+=), (%=), (&), (<>~), (^.), (%~))
import qualified Control.Lens as L
import qualified Data.Map as Map

import Control.Monad.State

import GHC.TypeLits

import Network.URI (URI(..), nullURI, pathSegments)
import qualified Network.HTTP.Types as HTTP

import Servant.API
import Servant.Utils.Links
import Servant.Docs.Internal
import Servant.API.ContentTypes

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeOperators
-- >>> :set -XFlexibleInstances
-- >>> :set -XMultiParamTypeClasses

-- | A marker for the Root node.
data Root

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

data RichLink = RichLink
  { _linkSource :: RichEndpoint
  , _linkTarget :: RichEndpoint
  , _linkTargetURI :: URI
  , _linkRel :: String
  }

$(makeLenses ''RichLink)

-- | State data for building up the graph.
data GraphStateData = GraphStateData
  { _endpoint :: Endpoint
  , _action :: Action
  , _typesToNodeIds :: Map.Map TypeRep Node
  , _edgesQueue :: [RichLink]
  , _currentNodeId :: Int
  , _currentGraph :: ApiGraph
  }
$(makeLenses ''GraphStateData)

type GraphState = State GraphStateData

apiLinkToURI :: ApiLink -> URI
apiLinkToURI (ApiLink {..}) =
  nullURI { uriPath = intercalate "/" _segments
          , uriQuery = if not (null _queryParams) then '?': intercalate "&" _queryParams else ""
          }

-- | Define the links that lead to this return type.
--
-- >>> data User
-- >>> type UserIndex = "users" :> Get '[JSON] [User]
-- >>> type UserShow = "users" :> Capture "id" Int :> Get '[JSON] User
-- >>> type API = UserIndex :<|> UserShow
-- >>> :{
-- instance LinksTo User API where
--   linksTo t a = [ link t a source target ]
--     where source = Proxy :: Proxy UserIndex
--           target = Proxy :: Proxy UserShow
-- :}

-- desired syntax?
-- instance Links API '[UserIndex] UserShow

class LinksTo target api where
  linksTo :: Proxy target -> Proxy api -> [RichLink]
  linksTo _ _ = []

  link :: (IsElem endpoint api, HasRichEndpoint source, HasRichEndpoint endpoint, Typeable api, Typeable target)
       => Proxy target -> Proxy api -> Proxy source -> Proxy endpoint -> RichLink
  link t a s e = RichLink (getRichEndpoint s) (getRichEndpoint e) uri rel
    where targetEndpoint = getRichEndpoint e
          uri = apiLinkToURI (targetEndpoint ^. apiLink)
          rel = show uri

  nodeType :: Proxy target -> Proxy api -> NodeType
  nodeType _ _ = NormalNode

-- There can be no links to Root, as it represents the state before the first
-- API connection.
instance LinksTo Root api

-- | Color for each common type of request: green for GET, purple for POST, blue for PUT, red for DELETE.
verbColors :: StdMethod -> String
verbColors GET = "green"
verbColors POST = "purple"
verbColors PUT = "blue"
verbColors DELETE = "red"
verbColors _ = "black"

-- | Generate a graph from a Servant API type.
graph :: (HasGraph api api) => Proxy api -> ApiGraph
graph p = view currentGraph $ execState (graphFor p p >> connectLinks) startingState
  where
    startingState = GraphStateData { _endpoint = defEndpoint
                                   , _action = defAction
                                   , _typesToNodeIds = withRoot
                                   , _edgesQueue = []
                                   , _currentNodeId = 1
                                   , _currentGraph = rootGraph
                                   }
    withRoot = Map.singleton (typeRep (Proxy :: Proxy Root)) 0
    rootGraph = mkGraph [(0, ApiNode "Root" NormalNode)] []

mkEdge :: Map.Map TypeRep Node -> RichLink -> Maybe (LEdge ApiEdge)
mkEdge typesToNodeIds' l = do
  startNode <- Map.lookup sourceType typesToNodeIds'
  endNode <- Map.lookup targetType typesToNodeIds'
  return $ (startNode, endNode, ApiEdge rel (verbColors meth))
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

-- https://github.com/haskell-servant/servant/blob/master/servant-docs/src/Servant/Docs/Internal.hs#L813
-- https://hackage.haskell.org/package/servant-docs-0.10/docs/src/Servant-Docs-Internal.html#docs
-- basically a reimplementation of HasDocs type class

-- | Extract the 'ApiLink', return type, and method from an endpoint.
class HasRichEndpoint endpoint where
  getRichEndpoint :: Proxy endpoint -> RichEndpoint

instance HasRichEndpoint Root where
  getRichEndpoint p =
    RichEndpoint { _apiLink = emptyLink
                 , _endpointMethod = error "no method"
                 , _returnType = typeRep p
                 }

instance (HasRichEndpoint sub, KnownSymbol sym) => HasRichEndpoint (sym :> sub) where
  getRichEndpoint _ = apiLink.segments %~ (symbolVal s:) $ getRichEndpoint (Proxy :: Proxy sub)
    where s = Proxy :: Proxy sym

instance (HasRichEndpoint sub, KnownSymbol sym) => HasRichEndpoint (Capture sym a :> sub) where
  getRichEndpoint _ = apiLink.segments %~ ((':':symbolVal s):) $ getRichEndpoint (Proxy :: Proxy sub)
    where s = Proxy :: Proxy sym

instance (HasRichEndpoint sub, KnownSymbol sym) => HasRichEndpoint (QueryParam sym a :> sub) where
  getRichEndpoint _ = apiLink.queryParams %~ (symbolVal s:) $ getRichEndpoint (Proxy :: Proxy sub)
    where s = Proxy :: Proxy sym

instance (ReflectMethod method, Typeable a) => HasRichEndpoint (Verb method status ctypes a) where
  getRichEndpoint _ = RichEndpoint emptyLink method retType
   where method = case (reflectMethod (Proxy :: Proxy method)) of
           "GET" -> GET
           "POST" -> POST
           "PUT" -> PUT
           "DELETE" -> DELETE
         retType = typeRep (Proxy :: Proxy a)

class HasGraph root api where
  graphFor :: Proxy root -> Proxy api -> GraphState ()

-- Terminal instance for verbs (ending an endpoint path).
instance (LinksTo a root, Typeable a, ReflectMethod method) => HasGraph root (Verb method status ctypes a) where
  graphFor rootP _ = do
    pathSegments <- use (endpoint.path)
    let pathName = intercalate "/" pathSegments
    existingNode <- uses typesToNodeIds (Map.lookup stateName)
    case existingNode of
      Just _ -> return () -- no-op
      Nothing -> do
        edgesQueue <>= links
        currentNodeId' <- currentNodeId <<+= 1
        let node = (currentNodeId', ApiNode (show stateName) (nodeType p rootP))
        currentGraph %= insNode node
        typesToNodeIds %= Map.insert stateName currentNodeId'
    endpoint .= defEndpoint
    action .= defAction
    where
      stateName = typeRep p
      links = linksTo p rootP
      p = Proxy :: Proxy a

 -- Extract one component of the path.
instance (KnownSymbol path, HasGraph root api) => HasGraph root (path :> api) where
  graphFor rootP _ = do
    endpoint.path <>= [symbolVal pa]
    graphFor rootP subApiP
    where subApiP = Proxy :: Proxy api
          pa = Proxy :: Proxy path

instance (KnownSymbol sym, HasGraph root api)
      => HasGraph root (QueryParam sym a :> api) where
  graphFor rootP p = do
    graphFor rootP subApiP
    where subApiP = Proxy :: Proxy api
          paramP = Proxy :: Proxy (QueryParam sym a)

instance (KnownSymbol sym, HasGraph root api) => HasGraph root (Capture sym a :> api) where
  graphFor rootP _ = do
    endpoint.path <>= [":" ++ symbolVal sym']
    graphFor rootP subApiP
    where subApiP = Proxy :: Proxy api
          sym' = Proxy :: Proxy sym

-- Compute subgraphs for each piece of the "alternatives" (:<|>) in an API type, then join them.
instance (HasGraph root a, HasGraph root b) => HasGraph root (a :<|> b) where
  graphFor rootP _ = do
    graphFor rootP p1
    graphFor rootP p2
    where p1 :: Proxy a
          p1 = Proxy
          p2 :: Proxy b
          p2 = Proxy
