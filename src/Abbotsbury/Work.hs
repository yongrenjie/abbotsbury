{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module    : Abbotsbury.Work
-- Copyright : (C) 2021 Jonathan Yong
-- License   : MIT
--
-- This module defines the core datatypes of @abbotsbury@. All of the
-- functionality here is re-exported by the top-level module ("Abbotsbury").
module Abbotsbury.Work
  ( -- * The core datatypes
    -- $work-datatypes
    DOI
  , ISBN
  , WorkType(..)
  , Work(..)
  , Author(..)
    -- * Lenses
    -- $work-lenses
  , workType
  , title
  , publisher
  , authors
  , journalLong
  , journalShort
  , year
  , volume
  , issue
  , pages
  , doi
  , isbn
  , articleNumber
  , given
  , family
  ) where

import           Data.Aeson
import           Data.List.NonEmpty             ( NonEmpty )
import           Data.Text                      ( Text )
import           GHC.Generics
import           Lens.Micro

-- $work-datatypes
-- @abbotsbury@'s core datatype is a 'Work', which is simply a record type which
-- contains every relevant field needed for citations (it ignores all the excess
-- metadata from Crossref). The other important datatype is an 'Author', which
-- generally refers to any person who contributed to the 'Work'.
--
-- In theory, 'Work's can be constructed manually by entering each field:
-- however, in practice it is much easier to fetch the data from Crossref using
-- the 'fetchWork' family of functions. This function, however, only works with
-- 'JournalArticle's and not other types of works (yet). Likewise, citation only
-- works with 'JournalArticle's for now.
--
-- Defining a work as a simple record type like this unfortunately leads to many
-- redundant fields: for example, a journal article does not have an ISBN, and a
-- book does not have a long or short journal name. However, this representation
-- has been chosen, as the alternative (@data Work = Article {...} | Book {...}
-- | ...@) would require @Prism@s to navigate, and I would rather not have
-- @lens@ as a dependency (@abbotsbury@ uses
-- [microlens](http://hackage.haskell.org/package/microlens)).

-- | A type synonym for Digital Object Identifiers (DOIs). Given that abbotsbury
-- doesn't provide a "smart constructor" for DOIs, it seems that the sensible
-- option for these is to use a plain type synonym instead of a @newtype@. See
-- also [Alexis King's blogpost on this](https://git.io/JsfwJ).
type DOI = Text

-- | As above, but for International Standard Book Numbers (ISBNs).
type ISBN = Text

-- | The list of possible work types is taken from Crossref
-- (<https://api.crossref.org/types>). Only 'JournalArticle' is supported right
-- now. Although we do not support all of them, they are enumerated here as they
-- allow a more useful error message to be returned if data for an unsupported
-- work type is fetched from Crossref (see
-- 'Abbotsbury.Crossref.CRUnknownWorkException').
data WorkType
  = BookSection
  | Monograph
  | Report
  | PeerReview
  | BookTrack
  | JournalArticle
  | Part
  | Other
  | Book
  | JournalVolume
  | BookSet
  | ReferenceEntry
  | ProceedingsArticle
  | Journal
  | Component
  | BookChapter
  | ProceedingsSeries
  | ReportSeries
  | Proceedings
  | Standard
  | ReferenceBook
  | PostedContent
  | JournalIssue
  | Dissertation
  | Dataset
  | BookSeries
  | EditedBook
  | StandardSeries
  deriving (Generic, Eq, Show)

-- | A @Work@ represents one single work from Crossref, whether it is a journal
-- article, book, or any other type of work. Lenses are provided for each field.
--
-- TODO: Add more fields here for other information (e.g. editors). See
-- <https://github.com/Crossref/rest-api-doc/blob/master/api_format.md>.
data Work = Work
  { _workType      :: WorkType
  , _title         :: Text
  , _publisher     :: Text
  ,
    -- | There has to be at least one author!
    _authors       :: NonEmpty Author
  ,
    -- | The full name of the journal, e.g. "Journal of the American Chemical
    -- Society".
    _journalLong   :: Text
  ,
    -- | The short name of the journal, e.g. "J. Am. Chem. Soc.". This short
    -- form should be taken from [CASSI](https://cassi.cas.org/); however,
    -- Crossref often has incorrect information for this field, which motivates
    -- the 'fixJournalShort' function.
    _journalShort  :: Text
  , _year          :: Int
  ,
    -- | The volume and issue cannot be simple Ints, because sometimes they are
    -- a range.
    _volume        :: Text
  , _issue         :: Text
  , _pages         :: Text
  , _doi           :: DOI
  , _isbn          :: ISBN
  ,
    -- | Some online-only articles do not have page numbers, and are indexed by
    -- article numbers instead.
    _articleNumber :: Text
  }
  deriving (Generic, Show, Eq)

-- | This is an incomplete representation of an author.
--
-- TODO: Rename this to @Contributor@ and add the @suffix@ and @role@ records.
-- This will allow us to deal with editors, etc. more properly.
data Author = Author
  { -- | Not everyone has a given name.
    _given  :: Maybe Text
  ,
    -- | But everyone has at least one name.
    _family :: Text
  }
  deriving (Generic, Show, Ord, Eq)

-- $work-lenses
-- The following lenses are provided for convenience. You don't have to use them
-- if you don't want to: the record fields are simply the same as these but
-- prefixed with an underscore.

workType :: Lens' Work WorkType
workType = lens _workType (\w x -> w { _workType = x })

title :: Lens' Work Text
title = lens _title (\w x -> w { _title = x })

publisher :: Lens' Work Text
publisher = lens _publisher (\w x -> w { _publisher = x })

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

given :: Lens' Author (Maybe Text)
given = lens _given (\a g -> a { _given = g })

family :: Lens' Author Text
family = lens _family (\a f -> a { _family = f })

instance ToJSON WorkType where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Work where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Author where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON WorkType

instance FromJSON Work

instance FromJSON Author
