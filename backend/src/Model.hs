{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Model where

import Data.Text ( Text )
import           Data.Time           (UTCTime)
import           Data.Time.Calendar  (Day)

import Database.Persist.TH
    ( mkMigrate, mkPersist, persistLowerCase, share, sqlSettings )

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Episode
  title            Text
  slug             Text
  UniqueSlug slug
  customIndex      Text
  UniqueCustomIndex customIndex
  ftExtension      Text
  audioContentType Text
  thumbnailFile    FilePath
  descriptionShort Text
  descriptionLong  Text
  duration         Int           -- duration in seconds
  fileSize         Int           -- file size in bytes
  pubdate          Day           -- day of recording
  created          UTCTime
  videoUrl         Text
|]
