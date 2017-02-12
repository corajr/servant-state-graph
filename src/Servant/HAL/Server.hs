{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Servant.HAL.Server where

import Data.Proxy (Proxy(..))
import Servant.API
import Servant.Server.Internal
import Servant.HAL

-- | Transform API type @api@ to a type where every return is @HAL@.
type family Hyper api where
  Hyper (a :<|> b) = Hyper a :<|> Hyper b
  Hyper (e :> x) = e :> Hyper x
  Hyper (Verb m s ct a) = Verb m s ct (HAL a)
