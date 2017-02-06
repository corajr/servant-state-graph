{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Choclety.GraphSpec (main, spec) where

import Test.Hspec
import qualified Data.Text as T
import Data.Typeable
import Data.Proxy (Proxy(..))
import Servant.API
import Servant.Utils.Links
import Choclety.Graph

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

main :: IO ()
main = hspec spec

data SortBy = Age | Name
  deriving (Eq, Show)

instance ToHttpApiData SortBy where
  toQueryParam = T.pack . show

data User = User {
  name :: String,
  age :: Int,
  email :: String
} deriving (Eq, Show)


type UserIndex = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]
type UserIndexPlain = "users" :> Get '[JSON] [User]
type UserShow = "user" :> Capture "id" Int :> Get '[JSON] User
type UserDelete = "user" :> Capture "id" Int :> DeleteNoContent '[JSON] NoContent

type UserAPI = UserIndex
           :<|> UserShow
           :<|> UserDelete

instance LinksFor UserAPI where
  linksFor api =
    [ linkFor api (edgeFrom :: Root :=> UserIndexPlain) NormalNode
    , linkFor api (edgeFrom :: UserIndexPlain :=> UserIndex) NormalNode
    , linkFor api (edgeFrom :: UserIndex :=> UserShow) NormalNode
    , linkFor api (edgeFrom :: UserShow :=> UserDelete) NormalNode
    ]

userAPI :: Proxy UserAPI
userAPI = Proxy

userGraph :: ApiGraph
userGraph = mkGraph [ (0, ApiNode "Root" NormalNode)
                    , (1, ApiNode "[User]" NormalNode)
                    , (2, ApiNode "User" NormalNode)
                    , (3, ApiNode "NoContent" NormalNode)]
                    [ (0, 1, ApiEdge "GET /users" "green")
                    , (1, 1, ApiEdge "GET /users?sortby" "green")
                    , (1, 2, ApiEdge "GET /user/:id" "green")
                    , (2, 3, ApiEdge "DELETE /user/:id" "red")]

type API1 = GetNoContent '[JSON] NoContent

instance LinksFor API1 where
  linksFor api = [ linkFor api (edgeFrom :: Root :=> API1) ErrorNode ]

api1 :: Proxy API1
api1 = Proxy

g1 :: ApiGraph
g1 = mkGraph [(0, ApiNode "Root" NormalNode)
             ,(1, ApiNode "NoContent" ErrorNode)
             ] [(0, 1, ApiEdge "" "green")]

type API2 = Capture "id" Int :> GetNoContent '[JSON] NoContent

instance LinksFor API2 where
  linksFor api = [ linkFor api (edgeFrom :: Root :=> API2) NormalNode ]

api2 :: Proxy API2
api2 = Proxy

g2 :: ApiGraph
g2 = mkGraph [ (0, ApiNode "Root" NormalNode)
             , (1, ApiNode "NoContent" NormalNode)
             ] [(0, 1, ApiEdge "GET /:id" "green")]

spec :: Spec
spec = do
  describe "graph" $ do
    it "translates a single GET endpoint into a graph with one node" $
      graph api1 `shouldBe` g1
    it "translates a single GET endpoint with a Capture into a graph with one node" $
      graph api2 `shouldBe` g2
    it "translates an API type into a graph representation" $
      graph userAPI `shouldBe` userGraph
