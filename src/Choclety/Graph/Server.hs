{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
The included graph viewer can be served directly from a Servant API.
-}
module Choclety.Graph.Server where

import Servant
import Network.Wai.Handler.Warp (run)
import Data.Proxy (Proxy(..))
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import Network.HTTP.Media ((//))
import Text.Mustache
import Text.Mustache.Compile (embedTemplate)
import Choclety.Graph
import Choclety.Graph.JSON

data HTML

type GraphViewAPI = Get '[HTML] ApiGraph

graphTemplate :: Template
graphTemplate = $(embedTemplate ["app"] "index.mustache")

instance ToMustache ApiGraph where
  toMustache gr = object
    [ "graph" ~> decodeUtf8 (graphToJSON gr)
    ]

instance Accept HTML where
  contentType _ = "text" // "html"

instance MimeRender HTML ApiGraph where
  mimeRender _ gr = encodeUtf8 . T.fromStrict $ substitute graphTemplate gr

graphApi :: Proxy GraphViewAPI
graphApi = Proxy

app :: ApiGraph -> Application
app gr = serve graphApi (return gr)

-- | Serve up the graph viewer on the specified port
serveGraph :: ApiGraph -> Int -> IO ()
serveGraph gr port = run port (app gr)
