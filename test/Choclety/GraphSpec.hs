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
type UserShow = "user" :> Capture "id" Int :> Get '[JSON] User
type UserDelete = "user" :> Capture "id" Int :> DeleteNoContent '[JSON] NoContent

type UserAPI = UserIndex
           :<|> UserShow
           :<|> UserDelete

userApiLink :: (IsElem endpoint UserAPI, HasLink endpoint) => Proxy endpoint -> MkLink endpoint
userApiLink = safeLink (Proxy :: Proxy UserAPI)

instance LinksTo NoContent UserAPI where
  linksTo t a = [ link t a source target ]
    where source = Proxy :: Proxy UserShow
          target = Proxy :: Proxy UserDelete

instance LinksTo User UserAPI where
  linksTo t a = [ link t a source target ]
    where source = Proxy :: Proxy UserIndex
          target = Proxy :: Proxy UserShow

instance LinksTo [User] UserAPI where
  linksTo t a = [ link t a source target
                , link t a target target2
                ]
    where source = Proxy :: Proxy Root
          target = Proxy :: Proxy ("users" :> Get '[JSON] [User])
          target2 = Proxy :: Proxy ("users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User])

userAPI :: Proxy UserAPI
userAPI = Proxy

userGraph :: ApiGraph
userGraph = mkGraph [ (0, ApiNode "Root" NormalNode)
                    , (1, ApiNode "[User]" NormalNode)
                    , (2, ApiNode "User" NormalNode)
                    , (3, ApiNode "NoContent" NormalNode)]
                    [ (0, 1, ApiEdge "users" "green")
                    , (1, 1, ApiEdge "users?sortby" "green")
                    , (1, 2, ApiEdge "user/:id" "green")
                    , (2, 3, ApiEdge "user/:id" "red")]

type API1 = GetNoContent '[JSON] NoContent

instance LinksTo NoContent API1 where
  nodeType _ _ = ErrorNode

api1 :: Proxy API1
api1 = Proxy

g1 :: ApiGraph
g1 = mkGraph [(0, ApiNode "Root" NormalNode)
             ,(1, ApiNode "NoContent" ErrorNode)
             ] []

type API2 = Capture "id" Int :> GetNoContent '[JSON] NoContent

instance LinksTo NoContent API2

api2 :: Proxy API2
api2 = Proxy

g2 :: ApiGraph
g2 = mkGraph [ (0, ApiNode "Root" NormalNode)
             , (1, ApiNode "NoContent" NormalNode)] []


spec :: Spec
spec = do
  describe "graph" $ do
    it "translates a single GET endpoint into a graph with one node" $
      graph api1 `shouldBe` g1
    it "translates a single GET endpoint with a Capture into a graph with one node" $
      graph api2 `shouldBe` g2
    it "translates an API type into a graph representation" $
      graph userAPI `shouldBe` userGraph
