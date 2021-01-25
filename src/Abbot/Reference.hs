{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Abbot.Reference
  ( module Abbot.Reference
  ) where

import           Data.Aeson
import Data.Char (isSpace)
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
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
  { _given  :: Maybe Text  -- Not everyone has a given name.
  , _family :: Text
  }
  deriving (Generic, Show)

makeLenses ''Reference
makeLenses ''Author

-- | Instances which allow references to be serialised as YAML.
instance ToJSON Reference where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Reference
instance ToJSON Author where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Author


-- | Methods of formatting author names.
data AuthorFormatting = ListCmd  -- For the list command.
                      deriving (Ord, Eq, Show)

-- | Formats an Author according to the specified AuthorFormatting mode.
formatAuthor :: AuthorFormatting -> Author -> Text
formatAuthor fmt auth = case auth ^. given of
  Nothing  -> auth ^. family
  Just gvn -> case fmt of
    ListCmd ->
      T.pack (map T.head (T.split (\c -> isSpace c || c == '-') gvn))
        <> " "
        <> auth ^. family
