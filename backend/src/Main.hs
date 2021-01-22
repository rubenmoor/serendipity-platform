{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Applicative          (Applicative (pure), (<**>))
import           Control.Monad.Logger         (NoLoggingT (..))
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Char8        as Char8
import qualified Data.ByteString.Lazy         as Lazy
import           Data.Default                 (def)
import           Data.FileEmbed               (makeRelativeToProject)
import           Data.Function                (flip, ($), (.))
import           Data.Int                     (Int)
import           Data.Maybe                   (Maybe (..), maybe)
import           Data.Monoid                  ((<>))
import           Data.Pool                    (Pool)
import           Data.Text                    (Text, breakOn, drop, replace,
                                               toUpper)
import qualified Data.Text                    as Text
import           Database.Persist.MySQL       (ConnectInfo (..),
                                               defaultConnectInfo,
                                               withMySQLPool)
import           Database.Persist.Sql         (SqlBackend, runMigration,
                                               runSqlPool)
import           Network.Wai.Handler.Warp     (run)
import           Options                      (Options (..), parseMyOptions)
import           Options.Applicative          (execParser, helper)
import           Options.Applicative.Builder  (fullDesc, info, progDesc)
import           Servant                      ((:<|>) (..), Application,
                                               hoistServer, serve)
import qualified Servant
import           Servant.Server               (ServerError (..), err400)
import           System.IO                    (FilePath, IO, putStrLn)
import           Text.Heterocephalus          (compileHtmlFile)
import           Text.Show                    (Show (show))

import           Common                       (EpisodeNew (..), Message (..),
                                               Status (StatusOK), api,
                                               convertToFilename,
                                               formatDuration)
import           Control.Monad.Except         (MonadError (throwError))
import           Control.Monad.Reader         (ReaderT (runReaderT), asks)
import           Control.Monad.Trans          (MonadIO (liftIO))
import           Data.Bool                    (Bool (False))
import           Data.Eq                      (Eq ((==)))
import           Data.Functor                 ((<$>))
import           Data.List                    (sortOn)
import           Data.Ord                     (Down (Down))
import           Data.Time                    (defaultTimeLocale, formatTime,
                                               rfc822DateFormat)
import           Data.Time.Clock              (getCurrentTime)
import           Data.Time.Format             (parseTimeM)
import           Data.Tuple                   (snd)
import           Database.Gerippe             (PersistStoreWrite (insert),
                                               getAllValues)
import           Safe                         (headMay)
import           Text.Blaze.Renderer.Utf8     (renderMarkup)

import           Model                        (Episode (..), migrateAll)
import Control.Monad (when, Monad((>>=)))
import Database.MySQL.Base.Types (Option(CharsetName))

data AppConfig = AppConfig
  { cfgPool     :: Pool SqlBackend
  , cfgMediaDir :: FilePath
  , cfgUrl      :: Text
  }

type Handler = ReaderT AppConfig Servant.Handler
type DbAction = ReaderT SqlBackend IO

app :: AppConfig -> Application
app state = serve api $ hoistServer api (flip runReaderT state) $
       handleFeedXML
  :<|> handleEpisodeNew

runDb :: DbAction a -> Handler a
runDb action = do
  pool <- asks cfgPool
  liftIO $ runSqlPool action pool

main :: IO ()
main = do
  Options{..} <- execParser $ info (parseMyOptions <**> helper) (
       fullDesc
    <> progDesc "Welcome to the homepage server of the podcast project"
    )
  putStrLn $ "Serving at port " <> show optPort
  let connectInfo = defaultConnectInfo
        { connectHost = optHost
        , connectUser = Char8.unpack optUser
        , connectPassword = Char8.unpack optPwd
        , connectDatabase = Char8.unpack optDbName
        , connectOptions = [ CharsetName "utf8mb4" ]
        }
  runNoLoggingT $ withMySQLPool connectInfo 10 $ \pool -> do
    runResourceT $ runSqlPool (runMigration migrateAll) pool
    let config = AppConfig
          { cfgPool = pool
          , cfgMediaDir = optMediaDir
          , cfgUrl = optUrl
          }
    NoLoggingT $ run optPort $ app config

handleFeedXML :: Handler Lazy.ByteString
handleFeedXML = do
  episodeList <- runDb getAllValues
  url <- asks cfgUrl
  let contents = renderMarkup (
        let title = "full serendipity" :: Text
            img = "podcast-logo.jpg" :: Text
            imgUrl = url <> img
            description = "Wir reden hier über Themen" :: Text
            copyright = "Rubm & Luke" :: Text
            email = "luke.rubm@gmail.com (Luke & Rubm)" :: Text
            pubDate = "Thu, 17 Dec 2020 02:00:00 GMT" :: Text
            itunesSubtitle = "Wir reden hier über Themen (Subtitle)" :: Text
            itunesSummary = "Wir reden hier über Themen (Summary)" :: Text
            authors = "Luke & Rubm" :: Text
            itunesOwnerNames = "Luke and Rubm" :: Text
            episodeData = getEpisodeFeedData url <$>
              sortOn  (Down . episodeCreated) episodeList
            latestDate = maybe pubDate efdRFC822 $ headMay episodeData
        in  $(makeRelativeToProject "feed.xml.tpl" >>= compileHtmlFile))
  pure contents

data EpisodeFeedData = EpisodeFeedData
    { efdRFC822            :: Text
    , efdSlug              :: Text
    , efdFtExtension       :: Text -- filetype extension (.m4a)
    , efdAudioFileUrl      :: Text
    , efdPageUrl           :: Text
    , efdTitle             :: Text
    , efdThumbnailFile     :: Text
    , efdDescription       :: Text
    , efdAudioContentType  :: Text
    , efdDurationSeconds   :: Int
    , efdDurationFormatted :: Text
    , efdFileSize          :: Int
    }

getEpisodeFeedData :: Text -> Model.Episode -> EpisodeFeedData
getEpisodeFeedData url Model.Episode{..} =
  let efdRFC822 = replace "UTC" "UT" $
        Text.pack $ formatTime defaultTimeLocale rfc822DateFormat episodeCreated
      efdSlug = episodeSlug
      efdFtExtension = episodeFtExtension
      efdAudioFileUrl = mkFileUrl url efdFtExtension efdSlug
      efdPageUrl = url <> efdSlug
      efdTitle = episodeTitle
      efdThumbnailFile = url <>
        if episodeThumbnailFile == "" then "podcast-logo.jpg"
        else Text.pack episodeThumbnailFile
      efdDescription = episodeDescriptionShort
      efdAudioContentType = episodeAudioContentType
      efdDurationSeconds = episodeDuration
      efdDurationFormatted = formatDuration episodeDuration
      efdFileSize = episodeFileSize
  in  EpisodeFeedData{..}

-- https://dts.podtrac.com/redirect.m4a/podcast-static.rubenmoor.net/media/2020-11-15_BANANE.m4a
mkFileUrl :: Text -> Text -> Text -> Text
mkFileUrl url filetypeExtension slug =
  let mediaLink' = drop 3 $ snd $ breakOn "://" url
  in     "https://"
      <> "dts.podtrac.com/redirect"
      <> filetypeExtension
      <> "/" <> mediaLink' <> "/"
      <> slug <> filetypeExtension

handleEpisodeNew :: EpisodeNew -> Handler Message
handleEpisodeNew EpisodeNew{..} = do
  now <- liftIO getCurrentTime
  when (newTitle == "") $
    throwError $ err400 { errBody = "title field is mandatory" }
  pubdate <- case parseTimeM False defaultTimeLocale "%F" (Text.unpack newDate) of
    Just d  -> pure d
    Nothing -> throwError $ err400 { errBody = "could not parse date" }
  let day = Text.pack $ formatTime defaultTimeLocale "%F" pubdate
      episode = def
        { episodeSlug = day <> "_" <> convertToFilename (toUpper newTitle)
        , episodeCustomIndex = newCustomIndex
        , episodeTitle = newTitle
        , episodePubdate = pubdate
        , episodeCreated = now
        }
  _ <- runDb $ insert episode
  pure $ Message
    { msgContent = "new episode created successfully"
    , msgStatus = StatusOK
    }
