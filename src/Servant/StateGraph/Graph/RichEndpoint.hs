{-| A 'RichEndpoint' makes information about an endpoint type accessible at the
term level.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Servant.StateGraph.Graph.RichEndpoint
  ( HasRichEndpoint(..)
  , RichEndpoint(..)
  ) where

import GHC.TypeLits

import Data.Typeable
import Data.Proxy (Proxy(..))
import Control.Lens.Operators
import Servant.API
import Servant.StateGraph.Graph.Types

-- * Typeclass

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

instance (HasRichEndpoint sub) => HasRichEndpoint (ReqBody y x :> sub) where
  getRichEndpoint _ = getRichEndpoint (Proxy :: Proxy sub)

instance (HasRichEndpoint sub) => HasRichEndpoint (Header y x :> sub) where
  getRichEndpoint _ = getRichEndpoint (Proxy :: Proxy sub)

instance (HasRichEndpoint sub, KnownSymbol sym) => HasRichEndpoint (Capture sym a :> sub) where
  getRichEndpoint _ = apiLink.segments %~ ((':':symbolVal s):) $ getRichEndpoint (Proxy :: Proxy sub)
    where s = Proxy :: Proxy sym

instance (HasRichEndpoint sub, KnownSymbol sym) => HasRichEndpoint (QueryParam sym a :> sub) where
  getRichEndpoint _ = apiLink.queryParams %~ (symbolVal s:) $ getRichEndpoint (Proxy :: Proxy sub)
    where s = Proxy :: Proxy sym

instance (HasRichEndpoint sub, KnownSymbol sym) => HasRichEndpoint (QueryParams sym a :> sub) where
  getRichEndpoint _ = apiLink.queryParams %~ (symbolVal s:) $ getRichEndpoint (Proxy :: Proxy sub)
    where s = Proxy :: Proxy sym

instance (HasRichEndpoint sub, KnownSymbol sym) => HasRichEndpoint (QueryFlag sym :> sub) where
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
