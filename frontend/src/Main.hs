{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MonoLocalBinds            #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}

module Main where

import           Control.Applicative  (Applicative((<*>), pure), (<$>))
import           Control.Monad        (when)
import           Control.Monad.Except (throwError)
import           Data.Data            (Proxy (Proxy))
import           Data.Either          (Either(Left, Right))
import           Data.Function        (flip, ($), (&), (.))
import qualified Data.Map             as Map
import           Data.Monoid          ((<>))
import           Data.Text            (Text, toUpper)
import qualified Data.Text            as Text
import           Data.Time            (getCurrentTime)
import           Data.Time.Format     (defaultTimeLocale, formatTime)
import           Data.Tuple           (fst, snd)
import           GHC.IO               (IO)
import           Reflex.Dom           (button, DomBuilder (DomBuilderSpace, inputElement),
                                       InputElement (..), InputElementConfig,
                                       MonadWidget, PostBuild,
                                       Reflex (Dynamic, Event), blank, constDyn,
                                       def, dynText, el, elAttr, elDynAttr,
                                       elementConfig_initialAttributes,
                                       inputElementConfig_elementConfig,
                                       inputElementConfig_initialValue,
                                       mainWidgetWithHead, text, (.~), (=:))
import           Servant.API          ((:<|>) (..))
import           Servant.Reflex       (BaseUrl (BasePath), ReqResult, client)
import           Text.RawString.QQ    (r)

import           Common               (API, EpisodeNew (..), Message,
                                       convertToFilename)
import Data.Traversable (Traversable(sequence))
import Data.Functor (Functor(fmap))

postEpisodeNew
  :: forall t (m :: * -> *). MonadWidget t m
  => Dynamic t (Either Text EpisodeNew)
  -> Event t ()
  -> m (Event t (ReqResult () Message))
(_ :<|> postEpisodeNew) = client (Proxy :: Proxy API)
                                 (Proxy :: Proxy (m :: * -> *))
                                 (Proxy :: Proxy ())
                                 (constDyn (BasePath "/"))

main :: IO ()
main = do
  now <- getCurrentTime
  mainWidgetWithHead headElement $ do
    el "h1" $ text "Create new episode"
    let today = Text.pack $ formatTime defaultTimeLocale "%F" now
    date <- _inputElement_value <$> input (def & inputElementConfig_initialValue .~ today) "Episode date: " "date"
    customIndex <- _inputElement_value <$> input def "Custom index: " "customIndex"
    title <- _inputElement_value <$> input def "Episode title: " "title"

    text "Title: "
    el "br" blank
    dynTitle customIndex title
    el "br" blank
    text "Episode slug (no special chars allowed): "
    el "br" blank
    dynText $ convertToFilename . toUpper <$> title

    el "br" blank
    sendButton <- button "send"

    -- Dynamic t Text x3
    -- Dynamic t (Either Err Text) x3
    -- Either
    -- Dynamic t (Either Err EpisodeNew)
    let eitherEpisodeNew = do
          -- ci <- fieldRequired "custom index required" customIndex
          -- t <- fieldRequired "title required" title
          -- d <- fieldRequired "date required" date
          ci <- customIndex
          t <- title
          d <- date
          -- EpisodeNew <$> pure ci <*> pure t <*> pure d
          pure $ Right $ EpisodeNew ci t d
    sendResult <- postEpisodeNew eitherEpisodeNew sendButton
    blank
  where
    headElement :: MonadWidget t m => m ()
    headElement = do
      elAttr "meta" ( "content" =: "text/html;charset=utf-8"
                   <> "httpequiv" =: "content-type"
                    ) blank
      elAttr "meta" ( "content" =: "utf-8"
                   <> "httpequiv" =: "encoding"
                    ) blank
      elAttr "link" ( "rel" =: "preconnect"
                   <> "href" =: "https://fonts.gstatic.com"
                    ) blank
      elAttr "link" ( "href" =: "https://fonts.googleapis.com/css2?family=Abel&display=swap"
                   <> "rel" =: "stylesheet"
                    ) blank
      elAttr "link" ( "rel" =: "shortcut icon"
                   <> "href" =: "/static/favicon.ico"
                    ) blank
      -- Font Awesome 5.13 free content -->
      elAttr "link" ( "rel" =: "stylesheet"
                   <> "href" =: "/static/FontAwesome/css/all.min.css"
                    ) blank
      el "title" $ text "serendipity.works"
      el "style" $ text [r|
body {
  font-family: 'Abel', sans-serif;
  background-color: lightgray;
  margin: 0;
}
                          |]

for = flip fmap


input
  :: DomBuilder t m
  => InputElementConfig er t (DomBuilderSpace m)
  -> Text
  -> Text
  -> m (InputElement er (DomBuilderSpace m) t)
input conf label id = do
  elAttr "label" ("for" =: id) $ text label
  el "br" blank
  i <- inputElement $ conf & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("id" =: id)
  el "br" blank
  pure i

dynTitle :: (DomBuilder t m, PostBuild t m) => Dynamic t Text -> Dynamic t Text -> m ()
dynTitle ci title =
    let a = ifEmpty ci title
    in  elDynAttr "span" (fst <$> a) $ dynText (snd <$> a)
  where
    ifEmpty customIndex title' = do
      c <- customIndex
      t <- title'
      pure $ if Text.null t
             then ("style" =: "font-style: italic", "empty")
             else (Map.empty, "#" <> c <> " " <> t)
