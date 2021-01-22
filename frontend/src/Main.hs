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

import           Control.Applicative (Applicative (pure, (<*>)), (<$>))
import           Data.Data           (Proxy (Proxy))
import           Data.Either         (Either (Left, Right))
import           Data.Function       (($), (&), (.))
import qualified Data.Map            as Map
import           Data.Monoid         ((<>))
import           Data.Text           (Text, toUpper)
import qualified Data.Text           as Text
import           Data.Time           (getCurrentTime)
import           Data.Time.Format    (defaultTimeLocale, formatTime)
import           Data.Tuple          (fst, snd)
import           GHC.IO              (IO)
import           Reflex.Dom          (DomBuilder (inputElement),
                                      InputElement (..), MonadHold (holdDyn),
                                      MonadWidget, Reflex (Dynamic, Event),
                                      blank, button, constDyn, def, dynText, el,
                                      elAttr, elDynAttr,
                                      elementConfig_initialAttributes,
                                      inputElementConfig_elementConfig,
                                      inputElementConfig_initialValue,
                                      mainWidgetWithHead, text, (.~), (=:))
import           Servant.API         ((:<|>) (..))
import           Servant.Reflex      (BaseUrl (BasePath), ReqResult, client,
                                      reqFailure)
import           Text.RawString.QQ   (r)

import           Common              (API, EpisodeNew (..), Message,
                                      convertToFilename)
import           Control.Monad       ((=<<))
import           Data.Functor        (Functor (fmap))
import           Data.Maybe          (Maybe (Just, Nothing), fromMaybe, maybe)
import           Data.Witherable     (mapMaybe)

postEpisodeNew
  :: forall t (m :: * -> *). MonadWidget t m
  => Dynamic t (Either Text EpisodeNew)
  -> Event t ()
  -> m (Event t (ReqResult () Message))
(_ :<|> postEpisodeNew :<|> _) =
  client (Proxy :: Proxy API)
         (Proxy :: Proxy (m :: * -> *))
         (Proxy :: Proxy ())
         (constDyn (BasePath "/"))

main :: IO ()
main = do
  now <- getCurrentTime
  mainWidgetWithHead headElement $ do

    let input conf label id = do
          elAttr "label" ("for" =: id) $ text label
          el "br" blank
          i <- inputElement $ conf
                 & inputElementConfig_elementConfig
                 . elementConfig_initialAttributes .~ ("id" =: id)
          el "br" blank
          let str = _inputElement_value i
          pure $ fmap (\s -> if Text.null s then Nothing else Just s) str

    el "h1" $ text "Create new episode"
    let today = Text.pack $ formatTime defaultTimeLocale "%F" now
    date <- input (def & inputElementConfig_initialValue .~ today) "Episode date: " "date"
    customIndex <- input def "Custom index: " "customIndex"
    title <- input def "Episode title: " "title"

    text "Title: "
    el "br" blank

    -- dynamic title
    let cfg = do
          mc <- customIndex
          mt <- title
          let mTitle = do
                c <- mc
                t <- mt
                pure (Map.empty, "#" <> c <> " " <> t)
          pure $ fromMaybe ("style" =: "font-style: italic", "empty") mTitle
    elDynAttr "span" (fst <$> cfg) $ dynText (snd <$> cfg)

    el "br" blank
    text "Episode slug (no special chars allowed): "
    el "br" blank
    dynText $ convertToFilename . toUpper . fromMaybe "" <$> title

    el "br" blank
    sendButton <- button "send"

    let eitherEpisodeNew = do
          mCustomIndex <- customIndex
          mTitle       <- title
          mDate        <- date
          let mEpisodeNew = EpisodeNew <$> mCustomIndex <*> mTitle <*> mDate
          pure $ maybe (Left "Missing required fields") Right mEpisodeNew
    res <- postEpisodeNew eitherEpisodeNew sendButton
    let err = mapMaybe reqFailure res
    elAttr "p" ("style" =: "color:red") $ dynText =<< holdDyn "" err
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
                   <> "href" =: "/favicon.ico"
                    ) blank
      -- Font Awesome 5.13 free content -->
      elAttr "link" ( "rel" =: "stylesheet"
                   <> "href" =: "/FontAwesome/css/all.min.css"
                    ) blank
      el "title" $ text "serendipity.works"
      el "style" $ text [r|
body {
  font-family: 'Abel', sans-serif;
  background-color: lightgray;
  margin: 0;
}
                          |]
