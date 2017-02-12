{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module SampleHALAPI where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger
import Servant
import Servant.HAL
import Servant.HAL.Server
import qualified Data.HashMap.Strict as HM
import SampleAPI

instance ToHAL Category where
  toHAL = halWithSelfID api (Proxy :: Proxy CategoryShow) categoryId

instance ToHAL Vendor where
  toHAL = halWithSelfID api (Proxy :: Proxy VendorShow) vendorId

instance ToHAL Product where
  toHAL = halWithSelfID api (Proxy :: Proxy ProductShow) productId

instance ToHAL Cart
instance ToHAL Invoice
instance ToHAL ErrorState
instance ToHAL Homepage where
  toHAL = halWithSelfRoute api (Proxy :: Proxy HomeRoute)

instance (ToHAL a) => ToHAL [a]

hyperApp :: Application
hyperApp = serve (hyper api) server

startHyperApp :: IO ()
startHyperApp =
    withStdoutLogger $ \aplogger -> do
        let settings = setPort 8080 $ setLogger aplogger defaultSettings
        runSettings settings hyperApp
