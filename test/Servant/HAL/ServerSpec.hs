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
type HyperAPI1 = Get '[JSON] (HAL String)

api1 :: Proxy API1
api1 = Proxy

server1 :: Server API1
server1 = return "hello"

hyperServer1 :: Server (Hyper API1)
hyperServer1 = return (toHAL "hello")

type API2 = Get '[JSON] String
       :<|> Capture "id" Int :> Post '[JSON] String
type HyperAPI2 = Hyper API2
type HyperAPI2' = Get '[JSON] (HAL String)
             :<|> Capture "id" Int :> Post '[JSON] (HAL String)

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
  describe "halServer" $ do
    -- is it even possible/desirable to write such a function?
    it "takes a Server api and returns a Server (Hyper api)" $ do
      pending
      let hServer = halServer api1 server1
      runHandler server1 `shouldReturn` Right "hello"
      runHandler hServer `shouldReturn` Right (toHAL "hello")
