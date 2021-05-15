{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

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
    -- * Work -> datatype 'Traversal's
  , _article
  , _book
    -- * Datatype -> field 'Lens'es
  , authors
  , doi
  , issue
  , journalLong
  , journalShort
  , number
  , pages
  , title
  , volume
  , year
  , edition
  , isbn
  , publisher
  , publisherLoc
    -- * Author -> field 'Lens'es
  , given
  , family
    -- * Typeclasses
    -- $work-typeclasses
  , Bibliographic(..)
  ) where

import           Data.Aeson
import           Data.Char                      ( isDigit )
import           Data.List.NonEmpty             ( NonEmpty )
import qualified Data.List.NonEmpty            as NE
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           GHC.Generics
import           Lens.Micro
import           Lens.Micro.TH

-- $work-datatypes
-- 'Work's are sum types of record types, and this is unfortunately one area
-- where Haskell definitely does /not/ shine. If it were purely a series of
-- nested record types, then the [RecordDotSyntax
-- proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0282-record-dot-syntax.rst)
-- would help a great deal, but this is not good enough because Works themselves
-- are sum types.
--
-- Therefore, I have opted to provide a more powerful lens-based interface.
-- Specifically, for getting the components of a sum type out, we can use
-- @Prism@s (or technically @Traversal@s, because I'm using the less powerful
-- [microlens](http://hackage.haskell.org/package/microlens)) library). Then,
-- for getting the record fields out, conventional lenses are provided.
--
-- I plan to write a tutorial on using @abbotsbury@'s lenses/traversals in due
-- course.

-- | A type synonym for Digital Object Identifiers (DOIs). Given that abbotsbury
-- doesn't provide a "smart constructor" for DOIs, it seems that the sensible
-- option for these is to use a plain type synonym instead of a @newtype@. See
-- also [Alexis King's blogpost on this](https://git.io/JsfwJ).
type DOI = Text

-- | As above, but for International Standard Book Numbers (ISBNs).
type ISBN = Text

-- | A @Work@ represents one single work from Crossref, which can either be an
-- 'Article' or a 'Book'. Technically, there are many more work types available
-- on Crossref. All these other work types are officially unsupported, though.
--
-- To get the constituent 'Article' or 'Book' from a 'Work', you can either
-- pattern match:
--
-- @
-- getDoi :: Work -> DOI
-- getDoi (ArticleWork a) = _articleDOI a  -- or a ^. doi
-- getDoi (BookWork b)    = _bookDOI b     -- or b ^. doi
-- @
--
-- or you can use the @Traversal@s '_article' and '_book':
--
-- @
-- getDoi' :: Work -> DOI
-- getDoi' = w ^?! failing (_article . doi) (_book . doi)
-- @
--
-- Realistically, pattern matching is going to be the solution 99% of the time,
-- because you will want to implement separate functions for Articles and Books
-- anyway. Consider the problem of citation generation: you probably want to
-- have different functions, like this. (This is not how @abbotsbury@ does it,
-- it's just an example!)
--
-- @
-- citeArticle :: Article -> Text
-- citeArticle a = a ^. title <> ...
--
-- citeBook :: Book -> Text
-- citeBook b = b ^. title <> ...
--
-- cite :: Work -> Text
-- cite (ArticleWork a) = citeArticle a
-- cite (BookWork b)    = citeBook b
-- @
data Work = ArticleWork Article
          | BookWork Book
          deriving (Generic, Show, Eq)

-- | A representation of a journal article. Note that (in accordance with the
-- default configuration of @microlens@) the lenses for this datatype don't have
-- the @_article@ prefix, and the first remaining letter is uncapitalised. For
-- example, if you want to get the title, you should write @a ^. title@.
--
-- TODO: Convert pages and number into a single field. They should be mutually
-- exclusive.
data Article = MkArticle
  { _articleTitle         :: Text
  ,
    -- | There has to be at least one author!
    _articleAuthors       :: NonEmpty Author
  ,
    -- | The full name of the journal, e.g. @"Journal of the American Chemical
    -- Society"@.
    _articleJournalLong   :: Text
  ,
    -- | The short name of the journal, e.g. @"J. Am. Chem. Soc."@. This short
    -- form should be taken from [CASSI](https://cassi.cas.org/); Crossref often
    -- has incorrect information for this field. See "Abbotsbury.Cite" for more
    -- information.
    _articleJournalShort  :: Text
  , _articleYear          :: Int
  ,
    -- | The volume and issue cannot be simple @Int@s, because sometimes they
    -- are a range.
    _articleVolume        :: Text
  , _articleIssue         :: Text
  , _articlePages         :: Text
  , _articleDoi           :: DOI
  ,
    -- | Some online-only articles do not have page numbers, and are indexed by
    -- article numbers instead.
    _articleNumber :: Text
  }
  deriving (Generic, Show, Eq)

-- | A representation of a book. Note that (in accordance with the default
-- configuration of @microlens@) the lenses for this datatype don't have the
-- @_book@ prefix, and the first remaining letter is uncapitalised. For example,
-- if you want to get the publisher, you should write @b ^. publisher.
--
-- TODO: Add more fields here for other information (e.g. editors). See
-- <https://github.com/Crossref/rest-api-doc/blob/master/api_format.md>.
data Book = MkBook
  { _bookTitle        :: Text
  , _bookPublisher    :: Text
  -- | Publisher location.
  , _bookPublisherLoc :: Text
    -- | A book doesn't have to have an author.
  , _bookAuthors      :: [Author]
  , _bookYear         :: Int
  , _bookEdition      :: Text
  , _bookIsbn         :: ISBN
  }
  deriving (Generic, Show, Eq)

-- | A representation of a person (it could be an author or an editor, depending
-- on the field in which this is found).
--
-- TODO: Add the @suffix@ field, and rename it to @Person@ or @Contributor@.
data Author = Author
  { -- | Not everyone has a given name.
    _given  :: Maybe Text
  ,
    -- | But everyone has at least one name.
    _family :: Text
  }
  deriving (Generic, Show, Ord, Eq)

-- | If @w@ is a 'Work', then @w ^?! _article@ extracts an 'Article' from inside
-- a 'Work' and throws an exception if it doesn't actually contain an 'Article'.
-- The 'safe' version @w ^? _article@ returns @Maybe Article@, i.e. @Just@ the
-- article if it is one and @Nothing@ if it isn't one.
_article :: Traversal' Work Article
_article f (ArticleWork a) = ArticleWork <$> f a
_article _ x = pure x

-- | Same as '_article', except that it extracts a 'Book'.
_book :: Traversal' Work Book
_book f (BookWork b) = BookWork <$> f b
_book _ x = pure x

-- | I generate the rest with TH, because honestly, I can't be bothered to
-- define all the HasField stuff myself.
makeFields ''Article
makeFields ''Book
makeLenses ''Author

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

-- $work-typeclasses
-- We also export a 'Bibliographic' typeclass, which contains functions which
-- should work on /all/ types of works (for now this means articles and books,
-- but it may be extended in the future). We also make 'Work' itself an instance
-- of 'Bibliographic' by simply pattern matching on its constructors and calling
-- the appropriate function on whatever type of work is contained inside it. This
-- helps make the interface slightly more flexible.

-- | A typeclass for functions which make sense on any type of 'Work'.
class Bibliographic x where
  -- | Return all the people who contributed to the work.
  -- 
  -- TODO: How to express their role? (Person, Role) or make Role a field of
  -- Person?
  getContributors :: x -> [Author]
  -- | Return all the authors of a work.
  getAuthors      :: x -> [Author]
  -- | Return all the editors of a work.
  getEditors      :: x -> [Author]
  -- | Return the year of the work.
  getYear         :: x -> Int
  -- | Return the title of the work.
  getTitle        :: x -> Text
  -- | A function which generates a unique identifier for each type of work.
  mkIdentifier    :: x -> Text

instance Bibliographic Article where
  getContributors = getAuthors
  getAuthors      = NE.toList . (^. authors)
  getEditors      = const []
  mkIdentifier    = T.replace "/" "#" . (^. doi)
  getYear         = (^. year)
  getTitle        = (^. title)

instance Bibliographic Book where
  getContributors = (++) <$> getAuthors <*> getEditors
  getAuthors      = (^. authors)
  getEditors      = const []
  mkIdentifier    = T.filter isDigit . (^. isbn)
  getYear         = (^. year)
  getTitle        = (^. title)

instance Bibliographic Work where
  getContributors = _work getContributors getContributors
  getAuthors      = _work getAuthors getAuthors
  getEditors      = _work getEditors getEditors
  mkIdentifier    = _work mkIdentifier mkIdentifier
  getYear         = _work getYear getYear
  getTitle        = _work getTitle getTitle

-- | Something like 'either', but for a Work. Not exported.
_work :: (Article -> r) -> (Book -> r) -> Work -> r
_work f1 f2 w = case w of
                     ArticleWork a -> f1 a
                     BookWork    b -> f2 b
