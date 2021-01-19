{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Custom
  ( Visibility (..)
  ) where

import Database.Persist.MySQL (PersistFieldSql, PersistFieldSql (..))
import Database.Persist (SqlType(SqlString), PersistValue(PersistText), PersistField (..))
import Data.Either (Either(Left, Right))
import Data.Eq (Eq((==)))
import Data.Function (($))
import Data.Bool (otherwise)
import Data.Monoid ((<>))

data Visibility = VisibilityPublic | VisibilityHidden

instance PersistField Visibility where
  toPersistValue VisibilityPublic = PersistText "public"
  toPersistValue VisibilityHidden = PersistText "hidden"
  fromPersistValue (PersistText t) | t == "public" = Right VisibilityPublic
                                   | t == "hidden" = Right VisibilityHidden
                                   | otherwise     = Left $ "No parse: " <> t
  fromPersistValue _ = Left "No parse"

instance PersistFieldSql Visibility where
  sqlType _ = SqlString
