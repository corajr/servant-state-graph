{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Servant.StateGraph.Graph.RichEndpointSpec where

import Servant.StateGraph.Graph.RichEndpoint
import Servant.StateGraph.Graph.Types
import Test.Hspec
import Servant.API
import Data.Proxy (Proxy(..))
import Data.Typeable

data MyReturn

myRet :: TypeRep
myRet = typeRep (Proxy :: Proxy MyReturn)

type EGet = Get '[JSON] MyReturn
type EPost = Post '[JSON] MyReturn

eGet :: Proxy EGet
eGet = Proxy

eGet' :: RichEndpoint
eGet' = RichEndpoint
  { _apiLink = emptyLink
  , _endpointMethod = GET
  , _returnType = myRet
  }

ePost :: Proxy EPost
ePost = Proxy

ePost' :: RichEndpoint
ePost' = eGet' { _endpointMethod = POST }

ePath :: Proxy ("path" :> EGet)
ePath = Proxy

ePath' :: RichEndpoint
ePath' = eGet' { _apiLink = ApiLink ["path"] [] }

eHeader :: Proxy (Header "header" String :> EGet)
eHeader = Proxy

eHeader' :: RichEndpoint
eHeader' = eGet'

eReqBody :: Proxy (ReqBody '[JSON] Int :> EPost)
eReqBody = Proxy

eReqBody' :: RichEndpoint
eReqBody' = ePost'

eCapture :: Proxy (Capture "id" Int :> EGet)
eCapture = Proxy

eCapture' :: RichEndpoint
eCapture' = eGet' { _apiLink = ApiLink [":id"] [] }

eCaptureAll :: Proxy (Capture "id" Int :> EGet)
eCaptureAll = Proxy

eCaptureAll' :: RichEndpoint
eCaptureAll' = eGet' { _apiLink = ApiLink [":id"] [] }

eQueryParam :: Proxy (QueryParam "sort_asc" Bool :> EGet)
eQueryParam = Proxy

eQueryParam' :: RichEndpoint
eQueryParam' = eGet' { _apiLink = ApiLink [] ["sort_asc"] }

eQueryParams :: Proxy (QueryParams "sort_asc" Bool :> EGet)
eQueryParams = Proxy

eQueryParams' :: RichEndpoint
eQueryParams' = eGet' { _apiLink = ApiLink [] ["sort_asc"] }

eQueryFlag :: Proxy (QueryFlag "sort_asc" :> EGet)
eQueryFlag = Proxy

eQueryFlag' :: RichEndpoint
eQueryFlag' = eGet' { _apiLink = ApiLink [] ["sort_asc"] }

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "RichEndpoint" $ do
    it "contains the HTTP method of an endpoint" $
      _endpointMethod eGet' `shouldBe` GET
    it "contains an ApiLink made up of path and query segments" $
      _apiLink eGet' `shouldBe` emptyLink
    it "contains the return type of the endpoint" $
      _returnType eGet' `shouldBe` myRet
  describe "getRichEndpoint" $ do
    it "returns information about an endpoint" $
      getRichEndpoint eGet `shouldBe` eGet'
    it "extracts the HTTP method of an endpoint" $
        getRichEndpoint ePost `shouldBe` ePost'
    it "ignores a Header" $
      getRichEndpoint eHeader `shouldBe` eHeader'
    it "ignores a ReqBody" $
      getRichEndpoint eReqBody `shouldBe` eReqBody'
    it "adds a path segment to the link" $
      getRichEndpoint ePath `shouldBe` ePath'
    it "adds a Capture to the link" $
      getRichEndpoint eCapture `shouldBe` eCapture'
    it "adds a QueryParam to the link" $
      getRichEndpoint eQueryParam `shouldBe` eQueryParam'
    it "adds a QueryParams to the link" $
      getRichEndpoint eQueryParams `shouldBe` eQueryParams'
    it "adds a QueryFlag to the link" $
      getRichEndpoint eQueryFlag `shouldBe` eQueryFlag'
