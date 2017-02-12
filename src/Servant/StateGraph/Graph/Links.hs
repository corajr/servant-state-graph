{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
module Servant.StateGraph.Graph.Links where

import Network.URI (URI(..), nullURI, pathSegments)
import Data.Proxy (Proxy(..))
import Data.Typeable
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Servant.StateGraph.Graph.Types
import Servant.StateGraph.Graph.RichEndpoint
import Servant.API.TypeLevel
import Control.Lens.TH
import Control.Lens.Operators

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeOperators
-- >>> :set -XFlexibleInstances
-- >>> :set -XMultiParamTypeClasses
-- >>> import Servant.API

data RichLink = RichLink
  { _linkSource :: RichEndpoint
  , _linkTarget :: RichEndpoint
  , _linkTargetURI :: URI
  , _linkTargetNodeType :: NodeType
  , _linkRel :: String
  }

$(makeLenses ''RichLink)

apiLinkToURI :: ApiLink -> URI
apiLinkToURI (ApiLink {..}) =
  nullURI { uriPath = '/' : intercalate "/" _segments
          , uriQuery = if not (null _queryParams) then '?': intercalate "&" _queryParams else ""
          }

-- | Infix operator for expressing a link between two endpoints.
type a :=> b = (Proxy a, Proxy b)

-- | A convenient value to be used with @linkFor@, as in:
--
-- >>> type Counter = "counter" :> Get '[JSON] Int
-- >>> type CounterIncrement = "counter" :> "inc" :> Post '[JSON] Int
-- >>> let edge = edgeFrom :: Counter :=> CounterIncrement
-- >>> edge
-- (Proxy,Proxy)
-- >>> :{
-- instance LinksFor (Counter :<|> CounterIncrement) where
--   linksFor api = [ linkFor api edge NormalNode ]
-- :}

edgeFrom :: (Proxy a, Proxy b)
edgeFrom = (Proxy, Proxy)

-- | Define the links that lead to this return type.
--
-- >>> data User
-- >>> type UserIndex = "users" :> Get '[JSON] [User]
-- >>> type UserShow = "users" :> Capture "id" Int :> Get '[JSON] User
-- >>> type API = UserIndex :<|> UserShow
-- >>> :{
-- instance LinksFor API where
--   linksFor api = [ linkFor api (edgeFrom :: UserIndex :=> UserShow) NormalNode ]
-- :}

class LinksFor api where
  linksFor :: Proxy api -> [RichLink]
  linksFor a = []

  linkFor :: ( IsElem source api
             , IsElem target api
             , HasRichEndpoint source
             , HasRichEndpoint target)
          => Proxy api -> (Proxy source, Proxy target) -> NodeType -> RichLink
  linkFor _ (s, t) nodeType = RichLink sourceEndpoint targetEndpoint uri nodeType rel
    where sourceEndpoint = getRichEndpoint s
          targetEndpoint = getRichEndpoint t
          uri = apiLinkToURI (targetEndpoint ^. apiLink)
          meth = targetEndpoint ^. endpointMethod
          rel = if nodeType == ErrorNode then "" else show meth ++ " " ++ show uri

nodeTypesFromLinks :: [RichLink] -> Map.Map TypeRep NodeType
nodeTypesFromLinks = Map.unions . map f
  where f l = Map.singleton (l^.linkTarget.returnType) (l^.linkTargetNodeType)
