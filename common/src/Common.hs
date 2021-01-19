{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Common
  ( api
  , API
  , convertToFilename
  , EpisodeNew (..)
  , Order (..)
  , SortBy (..)
  , Status (..)
  , Message (..)
  , formatDuration
  ) where

import           Control.Applicative      (Alternative (empty),
                                           Applicative (pure, (<*>)))
import           Data.Aeson               (FromJSON (..), KeyValue ((.=)),
                                           ToJSON (..), Value (..), object,
                                           (.:))
import           Data.Aeson.Types         (prependFailure, typeMismatch)
import qualified Data.ByteString.Lazy     as Lazy
import           Data.Either              (Either (..))
import           Data.Foldable            (Foldable (foldl'))
import           Data.Function            (id, ($))
import           Data.Functor             ((<$>))
import           Data.Int                 (Int)
import           Data.Monoid              ((<>))
import           Data.Proxy               (Proxy (Proxy))
import           Data.Text                (Text, replace)
import qualified Data.Text                as Text
import           GHC.Num                  (Num ((*)))
import           GHC.Real                 (Integral (mod))
import qualified GHC.Real                 as Real
import           Network.HTTP.Media       ((//), (/:))
import           Servant.API              ((:<|>), (:>), FromHttpApiData (..),
                                           Get, JSON, Post, ReqBody)
import           Servant.API.ContentTypes (MimeUnrender (..), Accept (..), MimeRender (..))
import           Text.Printf              (printf)

type API = "feed.xml" :> Get '[XML] Lazy.ByteString
      :<|> "api" :> "episode" :> "new" :> ReqBody '[JSON] EpisodeNew :> Post '[JSON] Message

data Message = Message
  { msgContent :: Text
  , msgStatus  :: Status
  }

instance ToJSON Message where
  toJSON Message{..} = object
    [ "content" .= msgContent
    , "status" .= msgStatus
    ]

instance FromJSON Message where
  parseJSON (Object v) = Message
    <$> v .: "content"
    <*> v .: "status"
  parseJSON invalid =
    prependFailure "parsing message failed, "
      (typeMismatch "Object" invalid)

data Status = StatusOK | StatusError

instance ToJSON Status where
  toJSON StatusOK = "OK"
  toJSON StatusError = "ERR"

instance FromJSON Status where
  parseJSON "OK"  = pure StatusOK
  parseJSON "ERR" = pure StatusError
  parseJSON _     = empty

data EpisodeNew = EpisodeNew
    { newCustomIndex :: Text
    , newTitle       :: Text
    , newDate        :: Text
    }

instance ToJSON EpisodeNew where
  toJSON EpisodeNew{..} = object
    [ "customIndex" .= newCustomIndex
    , "title" .= newTitle
    , "date" .= newDate
    ]

instance FromJSON EpisodeNew where
  parseJSON (Object v) = EpisodeNew
    <$> v .: "customIndex"
    <*> v .: "title"
    <*> v .: "date"
  parseJSON invalid =
    prependFailure "parsing EpisodeNew failed, "
      (typeMismatch "Object" invalid)

data XML

instance Accept XML where
  contentType _ = "application" // "xml" /: ("charset", "utf-8")

instance MimeRender XML Lazy.ByteString where
  mimeRender _ = id

instance MimeUnrender XML Lazy.ByteString where
  mimeUnrender _ bs = pure bs

api :: Proxy API
api = Proxy

data SortBy = SortByDate

instance FromHttpApiData SortBy where
  parseQueryParam "date" = Right SortByDate
  parseQueryParam str    = Left $ "unknown value for sortby: " <> str

data Order = OrderDescending | OrderAscending

instance FromHttpApiData Order where
  parseQueryParam "desc" = Right OrderDescending
  parseQueryParam "asc"  = Right OrderAscending
  parseQueryParam str    = Left $ "unknown value for order: " <> str

-- helpers

formatDuration :: Int -> Text
formatDuration d =
  let seconds = d `mod` 60
      minutes = (d `Real.div` 60) `mod` 60
      hours = d `Real.div` (60 * 60)
  in  Text.pack $ printf "%02d:%02d:%02d" hours minutes seconds

convertToFilename :: Text -> Text
convertToFilename str =
  let map = [ ("Ä", "A")
            , ("Ö", "O")
            , ("Ü", "U")
            , ("ß", "SS")
            , ("?", "_")
            , ("!", "_")
            , (".", "_")
            , (",", "_")
            , (";", "_")
            , (":", "_")
            , ("'", "_")
            , ("=", "_")
            , ("<", "_")
            , (">", "_")
            , ("/", "_")
            , ("\\", "_")
            , ("\"", "_")
            , ("&", "_")
            , ("@", "_")
            , ("%", "_")
            , ("+", "_")
            , ("*", "_")
            , ("$", "_")
            , (" ", "_")
            , ("(", "_")
            , (")", "_")
            , ("É", "E")
            , ("Á", "A")
            , ("Í", "I")
            , ("È", "E")
            , ("À", "A")
            , ("Ì", "I")
            ]
      acc str' (s, t) = replace s t str'
  in  foldl' acc str map
