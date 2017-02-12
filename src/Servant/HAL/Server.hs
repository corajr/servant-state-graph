{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances          #-}
{-# LANGUAGE MultiParamTypeClasses          #-}
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

hyper :: Proxy api -> Proxy (Hyper api)
hyper _ = Proxy

halServer :: (HasServer api '[], HasServer (Hyper api) '[]) => Proxy api -> Server api -> Server (Hyper api)
halServer _ s = undefined
