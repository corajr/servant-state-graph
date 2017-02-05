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


instance LinksTo [User] API1 where
  linksTo t a = [ link t a source target ]
    where
      source = Proxy :: Proxy Root
      target = Proxy :: Proxy API1

g1 :: ApiGraph
g1 = graph (Proxy :: Proxy API1)

c1 :: CytoGraph
c1 = CytoGraph [n1, n2] [e1]
  where n1 = CytoData (NodeData "n0" "Root" "lightgrey")
        n2 = CytoData (NodeData "n1" "[User]" "lightgrey")
        e1 = CytoData (EdgeData "n0" "n1" "users" "green")


j1 :: BL.ByteString
j1 = "{\"nodes\":[{\"data\":{\"id\":\"n0\",\"name\":\"Root\",\"noun\":\"lightgrey\"}},{\"data\":{\"id\":\"n1\",\"name\":\"[User]\",\"noun\":\"lightgrey\"}}],\"edges\":[{\"data\":{\"source\":\"n0\",\"target\":\"n1\",\"label\":\"users\",\"color\":\"green\"}}]}"

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "graphToCyto" $ do
    it "converts an ApiGraph to a CytoGraph" $ do
      graphToCyto g1 `shouldBe` c1
  describe "graphToJSON" $ do
    it "converts an ApiGraph to a JSON representation of a CytoGraph" $ do
      graphToJSON g1 `shouldBe` j1
