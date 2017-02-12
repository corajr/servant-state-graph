{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Servant.HALSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Aeson
import Data.Aeson.TH
import Data.Proxy (Proxy(..))
import Network.URI (URI)
import qualified Data.HashMap.Strict as HM

import Servant.API
import Servant.HAL

data Order = Order
  { orderID :: Int
  , currency :: String
  , status :: String
  , total :: Float
  } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Order)

order1 :: Order
order1 = Order 523 "USD" "shipped" 10.20

instance ToHAL Order where
  toHAL = halWithSelfID orderAPI (Proxy :: Proxy OrderShow) orderID

type Index = Get '[JSON] String
type OrderIndex = "orders" :> Get '[JSON] [Order]
type OrderShow = "orders" :> Capture "id" Int :> Get '[JSON] Order

type OrderAPI = Index
           :<|> OrderIndex
           :<|> OrderShow

orderAPI :: Proxy OrderAPI
orderAPI = Proxy

link1 :: HALLink
link1 = toHALLink orderAPI (Proxy :: Proxy OrderShow) ($ 523)

order1' :: HAL Order
order1' = toHAL order1

links :: HALLink
links = HALLinks [ toHALLink' orderAPI (Proxy :: Proxy Index)
                 , toHALLink' orderAPI (Proxy :: Proxy OrderIndex)
                 ]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "HALLink" $ do
    it "can represent a single link" $
      toJSON link1 `shouldBe` object ["href" .= uriFromString "/orders/523"]
    it "can represent an array of links" $
      toJSON links `shouldBe` toJSONList [ object ["href" .= uriFromString "/"]
                                         , object ["href" .= uriFromString "/orders"]
                                         ]
  describe "HAL" $ do
    it "wraps a JSON object with optional properties _links and _embedded" $ do
      toHAL order1 `shouldBe` HAL order1 (HM.singleton "self" link1) HM.empty
    it "serializes to have the properties of the object, with _links or _embedded added" $ do
      toJSON order1' `shouldBe` object [ "orderID" .= (523 :: Int)
                                       , "currency" .= ("USD" :: String)
                                       , "status" .= ("shipped" :: String)
                                       , "total" .= (10.20 :: Float)
                                       , "_links" .= object [("self", toJSON link1)]]
    it "can be deserialized into a record with an object and added properties" $
      fromJSON (toJSON order1') `shouldBe` Data.Aeson.Success order1'
