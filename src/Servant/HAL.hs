{-|
Data types based on HAL (Hypertext Application Language) IETF draft:
https://tools.ietf.org/html/draft-kelly-json-hal-08
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Servant.HAL where

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH
import Data.String (IsString(..))
import Data.Maybe (fromMaybe)
import Control.Arrow (second)
import Network.URI
import Network.HTTP.Media (MediaType, parseAccept)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM

instance ToJSON URI where
  toJSON = String . T.pack . show

instance FromJSON URI where
  parseJSON = withText "URI" f
    where f = maybe (fail "Could not parse URI") pure . parseURIReference . T.unpack

instance IsString URI where
  fromString xs = fromMaybe (error $ "invalid URI literal: " ++ xs) . parseURIReference $ xs

instance ToJSON MediaType where
  toJSON = String . T.pack . show

instance FromJSON MediaType where
  parseJSON = withText "MediaType" f
    where f = maybe (fail "Could not parse media type") pure . parseAccept . BS.pack . T.unpack

data LinkObject = LinkObject
  { _href :: URI
  , _templated :: Maybe Bool
  , _type :: Maybe MediaType
  , _deprecation :: Maybe URI
  , _name :: Maybe String
  , _profile :: Maybe URI
  , _title :: Maybe String
  , _hreflang :: Maybe String
  } deriving (Eq, Show)
$(deriveJSON defaultOptions{ fieldLabelModifier = drop 1, omitNothingFields = True } ''LinkObject)

data HALLink = HALLink LinkObject
             | HALLinks [LinkObject]
             deriving (Eq, Show)

instance ToJSON HALLink where
  toJSON (HALLink x) = toJSON x
  toJSON (HALLinks xs) = toJSON xs

instance FromJSON HALLink where
  parseJSON xs@(Array _) = fmap HALLinks $ listParser (parseJSON :: Value -> Parser LinkObject) xs
  parseJSON x@(Object _) = fmap HALLink $ (parseJSON :: Value -> Parser LinkObject) x
  parseJSON invalid = typeMismatch "HALLink" invalid

data HAL = HAL
  { resource :: Value
  , _links :: HM.HashMap Text HALLink
  , _embedded :: HM.HashMap Text HAL
  }
  | HALArray [HAL]
  deriving (Eq, Show)


instance ToJSON HAL where
  toJSON (HALArray xs) = toJSONList xs
  toJSON (HAL {..}) =
    case resource of
      Object ps -> Object (HM.unions [ps, links, embedded])
      _ -> error "HAL resources must contain JSON objects"
    where addProp :: (ToJSON a) => Text -> HM.HashMap Text a -> HM.HashMap Text Value
          addProp s xs = if not (HM.null xs)
                         then HM.singleton s (Object (HM.map toJSON xs))
                         else HM.empty
          links = addProp "_links" _links
          embedded = addProp "_embedded" _embedded

instance FromJSON HAL where
  parseJSON = withObject "HAL" f
    where f v = do links <- v .:? "_links"
                   embedded <- v .:? "_embedded"
                   let links' = fromMaybe HM.empty links
                       embedded' = fromMaybe HM.empty embedded
                       v' = HM.delete "_links" (HM.delete "_embedded" v)
                   return $ HAL (Object v') links' embedded'

toHAL :: (ToJSON a) => a -> HAL
toHAL x = HAL (toJSON x) HM.empty HM.empty

emptyLink :: LinkObject
emptyLink = LinkObject
  { _href = nullURI
  , _templated = Nothing
  , _type = Nothing
  , _deprecation = Nothing
  , _name = Nothing
  , _profile = Nothing
  , _title = Nothing
  , _hreflang = Nothing
  }

toLink :: URI -> HALLink
toLink x = HALLink (emptyLink { _href = x })
