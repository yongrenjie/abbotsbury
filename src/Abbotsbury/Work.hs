{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Abbotsbury.Work
  ( module Abbotsbury.Work
  ) where

import           Data.Aeson
import           Data.List.NonEmpty             ( NonEmpty )
import           Data.Text                      ( Text )
import           GHC.Generics
import           Lens.Micro

type DOI = Text
type ISBN = Text


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
data Work = Work
  { _workType      :: WorkType
  , _title         :: Text
  , _authors       :: NonEmpty Author
  , _journalLong   :: Text
  , _journalShort  :: Text
  , _year          :: Int
  , _volume        :: Text
  , _issue         :: Text
  , _pages         :: Text
  , _doi           :: DOI
  , _isbn          :: ISBN
  , _articleNumber :: Text
  }
  deriving (Generic, Show, Eq)
workType :: Lens' Work WorkType
workType = lens _workType (\w x -> w { _workType = x })
title :: Lens' Work Text
title = lens _title (\w x -> w { _title = x })
authors :: Lens' Work (NonEmpty Author)
authors = lens _authors (\w x -> w { _authors = x })
journalLong :: Lens' Work Text
journalLong = lens _journalLong (\w x -> w { _journalLong = x })
journalShort :: Lens' Work Text
journalShort = lens _journalShort (\w x -> w { _journalShort = x })
year :: Lens' Work Int
year = lens _year (\w x -> w { _year = x })
volume :: Lens' Work Text
volume = lens _volume (\w x -> w { _volume = x })
issue :: Lens' Work Text
issue = lens _issue (\w x -> w { _issue = x })
pages :: Lens' Work Text
pages = lens _pages (\w x -> w { _pages = x })
doi :: Lens' Work DOI
doi = lens _doi (\w x -> w { _doi = x })
isbn :: Lens' Work ISBN
isbn = lens _isbn (\w x -> w { _isbn = x })
articleNumber :: Lens' Work Text
articleNumber = lens _articleNumber (\w x -> w { _articleNumber = x })


-- | Technically, this should be renamed to "contributor".
data Author = Author
  { _given  :: Maybe Text  -- Not everyone has a given name.
  , _family :: Text
  }
  deriving (Generic, Show, Ord, Eq)
given :: Lens' Author (Maybe Text)
given = lens _given (\a g -> a {_given = g})
family :: Lens' Author Text
family = lens _family (\a f -> a {_family = f})


instance ToJSON WorkType where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON Work where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON Author where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON WorkType
instance FromJSON Work
instance FromJSON Author
