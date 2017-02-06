{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
module Choclety
  ( module Choclety
  , module Choclety.Graph
  , module Choclety.Graph.JSON
  ) where

import qualified Data.ByteString.Lazy as BL
import Data.Default
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Typeable
import Choclety.Graph
import Choclety.Graph.JSON

data ChocletyCommand = WriteJSON FilePath
                  | WriteJS FilePath
                  | PrintJSON

data ChocletyConfig = ChocletyConfig
  { _command :: ChocletyCommand
  }

instance Default ChocletyConfig where
  def = ChocletyConfig (WriteJS "examples/shoppe1.js")

choclety :: (HasGraph api api, LinksFor api) => ChocletyConfig -> Proxy api -> IO ()
choclety (ChocletyConfig {_command}) api =
  case _command of
    WriteJSON path -> BL.writeFile path json
    WriteJS path -> BL.writeFile path js
    PrintJSON -> BL.putStr json
  where
    json = graphToJSON (graph api)
    js = "var ApiOutput = " <> json <> ";"

choclety' :: (HasGraph api api, LinksFor api) => Proxy api -> IO ()
choclety' = choclety def
