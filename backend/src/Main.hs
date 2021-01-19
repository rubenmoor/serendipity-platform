{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Applicative          ((<**>))
import           Control.Monad.Logger         (NoLoggingT (..))
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Char8        as Char8
import qualified Data.ByteString.Lazy         as Lazy
import           Data.Function                (flip, ($))
import           Data.Int                     (Int)
import           Data.Monoid                  ((<>))
import           Data.Pool                    (Pool)
import           Data.Text                    (Text)
import qualified Data.Text as Text
import           Database.Persist.MySQL       (ConnectInfo (..),
                                               defaultConnectInfo,
                                               withMySQLPool)
import           Database.Persist.Sql         (SqlBackend, runMigration,
                                              runSqlPool)
import qualified GHC.Real as Real
import           Network.Wai.Handler.Warp     (run)
import           Options                      (Options (..), parseMyOptions)
import           Options.Applicative          (execParser, helper)
import           Options.Applicative.Builder  (fullDesc, info, progDesc)
import           Servant                      ((:<|>) (..), Application,
                                               hoistServer, serve)
import qualified Servant
import           System.IO                    (FilePath, IO, putStrLn)
import           Text.Heterocephalus          (compileHtmlFile)
import           Text.Show                    (Show (show))

import           Common                       (Message, EpisodeNew, api)
import           Control.Monad.Reader         (ReaderT (runReaderT))
import           Model                        (migrateAll)
import qualified Model

data AppConfig = AppConfig
  { cfgPool     :: Pool SqlBackend
  , cfgMediaDir :: FilePath
  , cfgUrl      :: Text
  }

type Handler = ReaderT AppConfig Servant.Handler

app :: AppConfig -> Application
app state = serve api $ hoistServer api (flip runReaderT state) $
       handleFeedXML
  :<|> handleEpisodeNew

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
        }
  runNoLoggingT $ withMySQLPool connectInfo 10 $ \pool -> do
    runResourceT $ runSqlPool (runMigration migrateAll) pool
    let config = AppConfig
          { cfgPool = pool
          , cfgMediaDir = optMediaDir
          , cfgUrl = optUrl
          }
    NoLoggingT $ run optPort $ app config

handleFeedXML :: Text -> Handler Lazy.ByteString
handleFeedXML url = do
  episodeList <- runDb getAllValues
  let contents = renderMarkup (
        let title = "full serendipity" :: Text
            img = "podcast-logo.jpg" :: Text
            imgUrl = url <> "static/" <> img
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
        in  $(compileHtmlFile "feed.xml.tpl"))
  return contents

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
      efdPageUrl = protocol <> podcastLink <> "/" <> efdSlug
      efdTitle = episodeTitle
      efdThumbnailFile = url <> "static/" <>
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
  let mediaLink' = drop 3 $ snd $ breakOn "://" $ url <> "static/"
  in     "https://"
      <> "dts.podtrac.com/redirect"
      <> filetypeExtension
      <> "/" <> mediaLink' <> "/"
      <> slug <> filetypeExtension

formatDuration :: Int -> Text
formatDuration d =
  let seconds = d `mod` 60
      minutes = (d `Real.div` 60) `mod` 60
      hours = d `Real.div` (60 * 60)
  in  Text.pack $ printf "%02d:%02d:%02d" hours minutes seconds

handleEpisodeNew :: EpisodeNew -> Handler Message
handleEpisodeNew EpisodeUpload{..} = do
  mediaDir <- asks cfgMediaDir
  now <- liftIO getCurrentTime
  -- TODO: these check don't have any effect
  when (uploadAudioFilename == "\"\"") $
    throwError $ err400 { errBody = "audio file field mandatory" }
  when (uploadTitle == "") $
    throwError $ err400 { errBody = "title field is mandatory" }
  episodePubdate <- case parseTimeM False defaultTimeLocale "%F" (Text.unpack uploadDate) of
    Just d  -> pure d
    Nothing -> throwError $ err400 { errBody = "could not parse date" }
  let day = Text.pack $ formatTime defaultTimeLocale "%F" episodePubdate
      slug = day <> "_" <> convertToFilename (toUpper uploadTitle)
      episodeFtExtension = Text.pack $ takeExtensions $ Text.unpack uploadAudioFilename
      audioFile = mediaDir </> Text.unpack slug <> Text.unpack episodeFtExtension
      -- fill Model.episode
      episodeCustomIndex = uploadCustomIndex
      episodeTitle = uploadTitle
      episodeThumbnailFile =
        if uploadThumbnailFilename /= "\"\""
        then mediaDir </> Text.unpack (slug
               <> Text.pack (takeExtensions $ Text.unpack uploadThumbnailFilename))
        else ""
      episodeDescriptionShort = uploadDescription
      episodeDescriptionLong = uploadDescription
      episodeAudioContentType = uploadAudioContentType
      episodeSlug = slug
      episodeDuration = uploadDuration
      episodeCreated = now
      episodeVideoUrl = ""
  episodeFileSize <- liftIO $ fromIntegral . fileSize <$> getFileStatus uploadAudioFile
  let episode = Model.Episode{..}
  liftIO $ do
    putStrLn $ "Copying " <> uploadAudioFile <> " to " <> audioFile
    copyFile uploadAudioFile audioFile
    unless (uploadThumbnailFilename == "\"\"") $ do
      putStrLn $ "Filename: " <> Text.unpack uploadThumbnailFilename <> ". Copying " <> uploadThumbnailFile <> " to " <> episodeThumbnailFile
      copyFile uploadThumbnailFile episodeThumbnailFile
  runDb $ insert episode
  return $ Text.unpack uploadAudioContentType
