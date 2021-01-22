{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Abbot.Reference
  ( module Abbot.Reference
  ) where

import           Data.Aeson
import           Data.Text                      ( Text )
import           Data.Time.Clock
import           GHC.Generics
import           Lens.Micro.Platform

data Reference = Article
  { _title        :: Text
  , _authors      :: [Author]
  , _journalLong  :: Text
  , _journalShort :: Text
  , _year         :: Int
  , _volume       :: Text
  , _issue        :: Text
  , _pages        :: Text
  , _doi          :: DOI
  , _timeAdded    :: UTCTime
  , _timeOpened   :: UTCTime
  }
  deriving (Generic, Show)

type DOI = Text
data Author = Author
  { _given  :: Text
  , _family :: Text
  }
  deriving (Generic, Show)

makeLenses ''Reference
makeLenses ''Author

instance ToJSON Reference where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Reference
instance ToJSON Author where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Author
