{-|
Data types based on HAL (Hypertext Application Language) IETF draft:
https://tools.ietf.org/html/draft-kelly-json-hal-08
-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Servant.HAL where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict   as HM
import           Data.Maybe            (fromMaybe)
import           Data.Proxy            (Proxy (..))
import           Data.String           (IsString (..))
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Network.HTTP.Media    (MediaType, parseAccept, (//))
import           Network.URI           (URI (..), nullURI, parseURIReference,
                                        relativeTo)
import           Servant.API
import           Servant.Utils.Links

instance ToJSON URI where
  toJSON = String . T.pack . show

instance FromJSON URI where
  parseJSON = withText "URI" f
    where f = maybe (fail "Could not parse URI") pure . parseURIReference . T.unpack

instance ToJSON MediaType where
  toJSON = String . T.pack . show

instance FromJSON MediaType where
  parseJSON = withText "MediaType" f
    where f = maybe (fail "Could not parse media type") pure . parseAccept . BS.pack . T.unpack

data HALLink = HALLink
  { _href        :: URI
  , _templated   :: Maybe Bool
  , _type        :: Maybe MediaType
  , _deprecation :: Maybe URI
  , _name        :: Maybe String
  , _profile     :: Maybe URI
  , _title       :: Maybe String
  , _hreflang    :: Maybe String
  }
  | HALLinks [HALLink]
  deriving (Eq, Show)
$(deriveJSON defaultOptions{ fieldLabelModifier = drop 1
                           , omitNothingFields = True
                           , sumEncoding = UntaggedValue
                           } ''HALLink)

data HAL a = HAL
  { resource  :: a
  , _links    :: HM.HashMap Text HALLink
  , _embedded :: HM.HashMap Text (HAL Value)
  }
  | HALArray [HAL Value]
  deriving (Eq, Show)

class (ToJSON a) => ToHAL a where
  toHAL :: a -> HAL a
  toHAL x = HAL x HM.empty HM.empty

instance (ToJSON a) => ToJSON (HAL a) where
  toJSON (HALArray xs) = toJSONList xs
  toJSON (HAL {..}) =
    case toJSON resource of
      Object ps -> Object (HM.unions [ps, links, embedded])
      _         -> error "HAL resources must contain JSON objects"
    where addProp s xs = if not (HM.null xs)
                         then HM.singleton s (Object (HM.map toJSON xs))
                         else HM.empty
          links = addProp "_links" _links
          embedded = addProp "_embedded" _embedded

instance (FromJSON a) => FromJSON (HAL a) where
  parseJSON (Object v) = do links <- v .:? "_links"
                            embedded <- v .:? "_embedded"
                            let links' = fromMaybe HM.empty links
                                embedded' = fromMaybe HM.empty embedded
                                v' = HM.delete "_links" (HM.delete "_embedded" v)
                            HAL <$> parseJSON (Object v') <*> pure links' <*> pure embedded'
  parseJSON xs@(Array _) = fmap HALArray $ listParser parseJSON xs
  parseJSON invalid = typeMismatch "HAL" invalid

-- | HAL+JSON content type.
data HALJSON

instance Accept HALJSON where
  contentType _ = "application" // "hal+json"

instance (ToHAL a) => MimeRender HALJSON a where
  mimeRender _ v = encode (toHAL v)

emptyLink :: HALLink
emptyLink = HALLink
  { _href = nullURI
  , _templated = Nothing
  , _type = Nothing
  , _deprecation = Nothing
  , _name = Nothing
  , _profile = Nothing
  , _title = Nothing
  , _hreflang = Nothing
  }

uriFromString :: String -> URI
uriFromString xs = fromMaybe (error $ "invalid URI literal: " ++ xs) . parseURIReference $ xs

toHALLink :: (IsElem endpoint api, HasLink endpoint)
          => Proxy api
          -> Proxy endpoint
          -> (MkLink endpoint -> Link)
          -> HALLink
toHALLink a e f = emptyLink { _href = uri' }
  where uri = linkURI (f (safeLink a e))
        uri' = uri `relativeTo` uriFromString "/"

toHALLink' :: (IsElem endpoint api, HasLink endpoint, MkLink endpoint ~ Link)
           => Proxy api -> Proxy endpoint -> HALLink
toHALLink' a e = toHALLink a e id

halWithSelf :: ( ToJSON a
               , IsElem endpoint api
               , HasLink endpoint
               )
            => Proxy api
            -> Proxy endpoint
            -> (MkLink endpoint -> Link)
            -> a
            -> HAL a
halWithSelf api endpoint f o =
    HAL o (HM.singleton "self" selfLink) HM.empty
    where
      selfLink = toHALLink api endpoint f

halWithSelfID :: ( ToJSON a
                 , IsElem endpoint api
                 , HasLink endpoint
                 , MkLink endpoint ~ (i -> Link))
            => Proxy api -> Proxy endpoint -> (a -> i) -> a -> HAL a
halWithSelfID a e getI o = halWithSelf a e ($ (getI o)) o

halWithSelfRoute :: ( ToJSON a
                    , IsElem endpoint api
                    , HasLink endpoint
                    , MkLink endpoint ~ Link)
                 => Proxy api -> Proxy endpoint -> a -> HAL a
halWithSelfRoute a e o = halWithSelf a e id o
