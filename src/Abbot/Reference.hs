{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Abbot.Reference
  ( module Abbot.Reference
  ) where

import           Data.Aeson
import           Data.List.NonEmpty             ( NonEmpty )
import           Data.Text                      ( Text )
import           Data.Time.Clock
import           GHC.Generics
import           Lens.Micro.Platform

type DOI = Text

type Tag = Text

data Reference = Reference
  { _work       :: Work
  , _tags       :: [Tag]
  , _timeAdded  :: UTCTime
  , _timeOpened :: UTCTime
  }
  deriving (Generic, Show)

data Work = Article
  { _title        :: Text
  , _authors      :: NonEmpty Author
  , _journalLong  :: Text
  , _journalShort :: Text
  , _year         :: Int
  , _volume       :: Text
  , _issue        :: Text
  , _pages        :: Text
  , _doi          :: DOI
  }
  deriving (Generic, Show)

data Author = Author
  { _given  :: Maybe Text  -- Not everyone has a given name.
  , _family :: Text
  }
  deriving (Generic, Show, Ord, Eq)

makeLenses ''Reference
makeLenses ''Work
makeLenses ''Author

-- | Instances which allow references to be serialised as YAML.
instance ToJSON Reference where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Reference
instance ToJSON Work where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Work
instance ToJSON Author where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Author
