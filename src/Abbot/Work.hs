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


-- from http://api.crossref.org/types
data WorkType = BookSection | Monograph | Report | PeerReview | BookTrack
              | JournalArticle | Part | Other | Book | JournalVolume | BookSet
              | ReferenceEntry | ProceedingsArticle | Journal | Component
              | BookChapter | ProceedingsSeries | ReportSeries | Proceedings
              | Standard | ReferenceBook | PostedContent | JournalIssue
              | Dissertation | Dataset | BookSeries | EditedBook | StandardSeries
              deriving (Generic, Eq, Show)

-- | TODO: Add more fields here.
-- See https://github.com/Crossref/rest-api-doc/blob/master/api_format.md
data Metadata = Metadata
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
  deriving (Generic, Eq, Show)

-- | Technically, from the Crossref point of view, the "work type" is encoded in the metadata as the
-- "type" entry. However, we separate it from the Metadata so that we can pattern match on the work
-- type easily.
data Work = Work WorkType Metadata
  deriving (Generic, Show, Eq)

-- | Technically, this should be renamed to "contributor".
data Author = Author
  { _given  :: Maybe Text  -- Not everyone has a given name.
  , _family :: Text
  }
  deriving (Generic, Show, Ord, Eq)

makeLenses ''Metadata
makeLenses ''Author

-- Manually make some lenses for Work.
-- Of the two, metadata seems to make more sense.
metadata :: Lens Work Work Metadata Metadata
metadata = lens (\(Work _ md) -> md) (\(Work wt _) md' -> Work wt md')
workType :: Lens Work Work WorkType WorkType
workType = lens (\(Work wt _) -> wt) (\(Work _ md) wt' -> Work wt' md)


instance ToJSON WorkType where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON Metadata where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON Work where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON Author where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON WorkType
instance FromJSON Metadata
instance FromJSON Work
instance FromJSON Author
