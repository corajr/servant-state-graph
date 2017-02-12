{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
module Servant.HAL.ServerSpec where

import Test.Hspec
import Servant.API
import Servant.HAL
import Servant.HAL.Server
import Data.Type.Equality

type API1 = Get '[JSON] String
type HyperAPI1 = Hyper API1
type HyperAPI1' = Get '[JSON] (HAL String)

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
      (Refl :: HyperAPI1 :~: HyperAPI1') `shouldBe` Refl
    it "transforms a two-branch API type into a hypermedia API type" $
      (Refl :: HyperAPI2 :~: HyperAPI2') `shouldBe` Refl
