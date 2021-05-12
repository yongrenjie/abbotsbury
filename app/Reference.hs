{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Reference
  ( module Reference
  , module Abbotsbury.Work
  ) where

import           Abbotsbury.Work
import           Data.Aeson
import           Data.Text                      ( Text )
import           Data.Time.Clock
import           GHC.Generics
import           Lens.Micro.Platform

type Tag = Text

data Reference = Reference
  { _work       :: Work
  , _tags       :: [Tag]
  , _timeAdded  :: UTCTime
  , _timeOpened :: UTCTime
  }
  deriving (Generic, Show)

makeLenses ''Reference

-- | Instances which allow references to be serialised as YAML.
instance ToJSON Reference where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Reference
