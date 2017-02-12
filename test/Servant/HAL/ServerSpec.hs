{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
module Servant.HAL.ServerSpec where

import Test.Hspec
import Servant
import Servant.HAL
import Servant.HAL.Server
import Data.Proxy (Proxy(..))
import Data.Type.Equality

type API1 = Get '[JSON] String
type HyperAPI1 = Get '[HALJSON, JSON] String

api1 :: Proxy API1
api1 = Proxy

server1 :: Server API1
server1 = return "hello"

type API2 = Get '[JSON] String
       :<|> Capture "id" Int :> Post '[JSON] String
type HyperAPI2 = Get '[HALJSON, JSON] String
             :<|> Capture "id" Int :> Post '[HALJSON, JSON] String

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Hyper" $ do
    it "is a type family that transforms an API type into a hypermedia API type" $
      (Refl :: Hyper API1 :~: HyperAPI1) `shouldBe` Refl
    it "transforms a two-branch API type into a hypermedia API type" $
      (Refl :: Hyper API2 :~: HyperAPI2) `shouldBe` Refl
  describe "hyper" $ do
    it "turns a Proxy api into a Proxy (Hyper api)" $
      hyper api1 `shouldBe` (Proxy :: Proxy (Hyper API1))
