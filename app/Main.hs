{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import qualified Data.ByteString.Lazy as BL
import SampleAPI (Shoppe1)
import Servant.StateGraph
import Servant.API

api :: Proxy Shoppe1
api = Proxy

main :: IO ()
main = stateGraph' api
