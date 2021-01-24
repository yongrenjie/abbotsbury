{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Abbot.Reference
  ( module Abbot.Reference
  ) where

import           Data.Aeson
import           Data.Char                      ( isAlphaNum
                                                , isSpace
                                                )
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
  { _given  :: Text
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

-- | Produce as short a journal name as possible, by removing special characters
-- (only alphanumeric characters and spaces are retained), as well as using some
-- acronyms such as "NMR".
getShortestJournalName :: Reference -> Text
getShortestJournalName ref = T.strip $ T.filter ((||) <$> isAlphaNum <*> isSpace) (ref ^. journalShort)
