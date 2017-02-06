{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Choclety.Graph.JSONSpec (main, spec) where

import Test.Hspec
import Data.Typeable
import qualified Data.ByteString.Lazy as BL
import Choclety.Graph
import Choclety.Graph.JSON
import Servant.API
import Servant.Utils.Links

import Data.Proxy (Proxy(..))

data User = User
  { userName :: String
  }

type API1 = "users" :> Get '[JSON] [User]

instance LinksFor API1 where
  linksFor api = [ linkFor api (edgeFrom :: Root :=> API1) NormalNode]

g1 :: ApiGraph
g1 = graph (Proxy :: Proxy API1)

c1 :: CytoGraph
c1 = CytoGraph [n1, n2] [e1]
  where n1 = CytoData (NodeData "n0" "Root" "lightgrey")
        n2 = CytoData (NodeData "n1" "[User]" "lightgrey")
        e1 = CytoData (EdgeData "n0" "n1" "GET /users" "green")

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "graphToCyto" $ do
    it "converts an ApiGraph to a CytoGraph" $ do
      graphToCyto g1 `shouldBe` c1
  describe "graphToJSON" $ do
    it "converts an ApiGraph to a JSON representation of a CytoGraph" $ do
      graphToJSON g1 `shouldSatisfy` not . BL.null
