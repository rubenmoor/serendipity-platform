{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Common
  ( api
  , EpisodeNew (..)
  , Order (..)
  , SortBy (..)
  , Status (..)
  , Message (..)
  ) where

-- import           Data.Aeson               (ToJSON (..))
import qualified Data.ByteString.Lazy     as Lazy
import           Data.Either              (Either (..))
import           Data.Function            (id, ($))
import           Data.Monoid              ((<>))
import           Data.Proxy               (Proxy (Proxy))
import           Data.Text                (Text)
import           Network.HTTP.Media       ((//), (/:))
import           Servant.API              ((:<|>), (:>), FromHttpApiData (..),
                                           Get, JSON, Post, ReqBody)
import           Servant.API.ContentTypes (Accept (..), MimeRender (..))

type API = "feed.xml" :> Get '[XML] Lazy.ByteString
      :<|> "api" :> "episode" :> "new" :> ReqBody '[JSON] EpisodeNew :> Post '[JSON] Message

data Message = Message
  { msgContent :: Text
  , msgStatus  :: Status
  }

data Status = StatusOK | StatusError

-- instance ToJSON Status where
--   toJSON StatusOK = "OK"
--   toJSON StatusError = "ERR"

data EpisodeNew = EpisodeNew
    { newCustomIndex :: Text
    , newTitle       :: Text
    , newDate        :: Text
    }

data XML

instance Accept XML where
  contentType _ = "application" // "xml" /: ("charset", "utf-8")

instance MimeRender XML Lazy.ByteString where
  mimeRender _ = id

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
