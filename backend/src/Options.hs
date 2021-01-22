{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Options where

import Data.Int (Int)
import System.IO (FilePath)
import Data.ByteString (ByteString)
import Options.Applicative (strOption, showDefault, help, value, metavar, short, long, auto, option, Parser)
import Data.Monoid ((<>))
import Network.Socket (HostName)
import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)

data Options = Options
    { optPort      :: Int
    , optUrl       :: Text
    , optMediaDir  :: FilePath
    , optHost      :: HostName
    , optUser      :: ByteString
    , optPwd       :: ByteString
    , optDbName    :: ByteString
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
