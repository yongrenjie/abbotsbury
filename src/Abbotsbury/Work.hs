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
  , Work(..)
  , Article(..)
  , Book(..)
  , Author(..)
  ) where

import           Data.Aeson
import           Data.List.NonEmpty             ( NonEmpty )
import           Data.Text                      ( Text )
import           GHC.Generics

-- $work-datatypes
-- TODO: Update this because the whole thing has changed! Please ignore the
-- following.
--
-- @abbotsbury@'s core datatype is a 'Work', which is simply a record type which
-- contains every relevant field needed for citations (it ignores all the excess
-- metadata from Crossref). The other important datatype is an 'Author', which
-- generally refers to any person who contributed to the 'Work'.
--
-- In theory, 'Work's can be constructed manually by entering each field:
-- however, in practice it is much easier to fetch the data from Crossref using
-- the 'fetchWork' family of functions. This function, however, only works with
-- 'Article's and not other types of works (yet). Likewise, citation only
-- works with 'Article's for now.
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

-- | A @Work@ represents one single work from Crossref.
-- TODO: Document this again.
--
-- TODO: Add more fields here for other information (e.g. editors). See
-- <https://github.com/Crossref/rest-api-doc/blob/master/api_format.md>.
data Work = Article Article
          | Book Book
          deriving (Generic, Show, Eq)

-- | A journal article.
data Article = MkArticle
  { _title         :: Text
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
  ,
    -- | Some online-only articles do not have page numbers, and are indexed by
    -- article numbers instead.
    _articleNumber :: Text
  }
  deriving (Generic, Show, Eq)

-- | A book.
data Book = MkBook
  { _title        :: Text
  , _publisher    :: Text
  -- | Publisher location.
  , _publisherLoc :: Text
    -- | A book doesn't have to have an author.
  , _authors      :: [Author]
  , _year         :: Int
  , _edition      :: Text
  , _isbn         :: ISBN
  }
  deriving (Generic, Show, Eq)

-- | This is an incomplete representation of an author.
--
-- TODO: Add the @suffix@ field, and rename it to @Person@ or @Contributor@ so
-- that it can be generalised to editors, etc.
data Author = Author
  { -- | Not everyone has a given name.
    _given  :: Maybe Text
  ,
    -- | But everyone has at least one name.
    _family :: Text
  }
  deriving (Generic, Show, Ord, Eq)

instance ToJSON Work where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Article where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Book where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Author where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Work

instance FromJSON Article

instance FromJSON Book

instance FromJSON Author
