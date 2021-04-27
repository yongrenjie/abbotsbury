{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Abbot.Work
  ( module Abbot.Work
  ) where

import           Data.Aeson
import           Data.List.NonEmpty             ( NonEmpty )
import           Data.Text                      ( Text )
import           GHC.Generics
import           Lens.Micro.Platform

type DOI = Text

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
  deriving (Generic, Show, Eq)

data Author = Author
  { _given  :: Maybe Text  -- Not everyone has a given name.
  , _family :: Text
  }
  deriving (Generic, Show, Ord, Eq)

makeLenses ''Work
makeLenses ''Author

instance ToJSON Work where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Work
instance ToJSON Author where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Author
