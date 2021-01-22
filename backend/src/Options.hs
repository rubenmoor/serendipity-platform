{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Options where

import           Control.Applicative ((<$>), (<*>))
import           Data.ByteString     (ByteString)
import           Data.Int            (Int)
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import           Network.Socket      (HostName)
import           Options.Applicative (switch, Parser, auto, help, long, metavar, option,
                                      short, showDefault, strOption, value)
import           System.IO           (FilePath)
import Data.Bool (Bool)

data Options = Options
    { optPort            :: Int
    , optUrl             :: Text
    , optPublicDir       :: FilePath
    , optMediaDir        :: FilePath
    , optHost            :: HostName
    , optUser            :: ByteString
    , optPwd             :: ByteString
    , optDbName          :: ByteString
    , optGetDataFilePath :: Bool
    }

parseMyOptions :: Parser Options
parseMyOptions = Options
  <$> option auto (
           long "port"
        <> short 'p'
        <> metavar "PORT"
        <> value 3000
        <> help "serve at specified port"
        <> showDefault
        )
  <*> strOption (
           long "url"
        <> metavar "URL"
        <> value "http://localhost:3000/"
        <> help "the url of the homepage"
        <> showDefault
        )
  <*> strOption (
           long "public-directory"
        <> metavar "PUBLICDIR"
        <> value "public"
        <> help "filesystem directory, contains compiled frontend files (index.html, *.js)"
        <> showDefault
        )
  <*> strOption (
            long "media-directory"
        <> metavar "MEDIADIR"
        <> value "media"
        <> help "audio files will be copied here"
        <> showDefault
        )
  <*> strOption (
           long "mysql-host"
        <> metavar "HOST"
        <> value "localhost"
        <> help "mysql host"
        <> showDefault
        )
  <*> strOption (
           long "mysql-user"
        <> metavar "USER"
        <> value "podcast"
        <> help "mysql user"
        <> showDefault
        )
  <*> strOption (
           long "mysql-password"
        <> metavar "PASSWORD"
        <> value ""
        <> help "mysql password for given user"
        <> showDefault
        )
  <*> strOption (
           long "mysql-database"
        <> metavar "DATABASE_NAME"
        <> value "podcast"
        <> help "mysql database name"
        <> showDefault
        )
  <*> switch (
           long "get-data-file-path"
        <> help "show path to data files and exit"
        <> showDefault
        )
