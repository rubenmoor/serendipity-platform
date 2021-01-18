{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import           Control.Applicative (Applicative (pure), (<$>))
import           Data.Foldable       (Foldable (foldl'))
import           Data.Function       (($), (&), (.))
import qualified Data.Map            as Map
import           Data.Monoid         ((<>))
import           Data.Text           (Text, replace, toUpper)
import qualified Data.Text           as Text
import           Data.Time           (getCurrentTime)
import           Data.Time.Format    (defaultTimeLocale, formatTime)
import           Data.Tuple          (fst, snd)
import           GHC.IO              (IO)
import           Reflex.Dom          (DomBuilder (DomBuilderSpace, inputElement),
                                      InputElement (..), InputElementConfig,
                                      MonadWidget, PostBuild, Reflex (Dynamic),
                                      blank, def, dynText, el, elAttr,
                                      elDynAttr,
                                      elementConfig_initialAttributes,
                                      inputElementConfig_elementConfig,
                                      inputElementConfig_initialValue,
                                      mainWidgetWithHead, text, (.~), (=:))
import           Text.RawString.QQ   (r)


main :: IO ()
main = do
  now <- getCurrentTime
  mainWidgetWithHead headElement $ do
    el "h1" $ text "Create new episode"
    let today = Text.pack $ formatTime defaultTimeLocale "%F" now
    _ <- input (def & inputElementConfig_initialValue .~ today) "Episode date: " "date"
    customIndex <- input def "Custom index: " "customIndex"
    title <- input def "Episode title: " "title"

    text "Title: "
    el "br" blank
    dynTitle (_inputElement_value customIndex) $ _inputElement_value title
    el "br" blank
    text "Episode slug (no special chars allowed): "
    el "br" blank
    dynText $ convertToFilename . toUpper <$> _inputElement_value title
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
