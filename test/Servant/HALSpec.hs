{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
-- module Servant.HALSpec (main, spec) where
module Servant.HALSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Aeson
import Data.Aeson.TH
import qualified Data.HashMap.Strict as HM

import Servant.HAL

data Order = Order
  { currency :: String
  , status :: String
  , total :: Float
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Order)
order1 :: Order
order1 = Order "USD" "shipped" 10.20

link1 :: HALLink
link1 = toLink "/orders/523"

order1' :: HAL
order1' = (toHAL order1) { _links = HM.singleton "self" link1 }

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "HAL" $ do
    it "wraps a JSON object with optional properties _links and _embedded" $ do
      toHAL order1 `shouldBe` HAL (toJSON order1) HM.empty HM.empty
    it "serializes to have the properties of the object, with _links or _embedded added" $ do
      toJSON order1' `shouldBe` object [ "currency" .= ("USD" :: String)
                                       , "status" .= ("shipped" :: String)
                                       , "total" .= (10.20 :: Float)
                                       , "_links" .= object [("self", toJSON link1)]]
    it "can be deserialized into a record with an object and added properties" $
      fromJSON (toJSON order1') `shouldBe` Data.Aeson.Success order1'
