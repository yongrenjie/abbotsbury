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

data Reference = Reference
  { _work       :: Work
  , _timeAdded  :: UTCTime
  , _timeOpened :: UTCTime
  }
  deriving (Generic, Show)

data Work = Article
  { _title        :: Text
  , _authors      :: [Author]
  , _journalLong  :: Text
  , _journalShort :: Text
  , _year         :: Int
  , _volume       :: Text
  , _issue        :: Text
  , _pages        :: Text
  , _doi          :: DOI
  }
  deriving (Generic, Show)

type DOI = Text

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


-- | Methods of formatting author names.
data AuthorFormatting = ListCmd          -- For the list command.
                      | FamilyInitials   -- ACS style.
                      | InitialsFamily   -- ACIE style.
                      | BibLaTeX         -- For .bib files.
                      deriving (Ord, Eq, Show)

-- | Formats an Author according to the specified AuthorFormatting mode.
formatAuthor :: AuthorFormatting -> Author -> Text
formatAuthor fmt auth =
  let fam = _family auth
  in
    case _given auth of
      Nothing  -> fam
      Just gvn -> case fmt of
        ListCmd ->
          T.pack (map T.head (T.split (\c -> isSpace c || c == '-') gvn))
            <> " "
            <> fam
        FamilyInitials -> fam <> ", " <> makeInitials gvn
        InitialsFamily -> makeInitials gvn <> " " <> fam
        BibLaTeX -> T.replace ". " ".\\ " (fam <> ", " <> gvn)


-- | Extracts the initials from a given name.
-- 
-- >>> makeInitials "Jonathan Ren Jie"
-- "J. R. J."
-- >>> makeInitials "Jean-Baptiste Simon"
-- "J.-B. S."
makeInitials :: Text -> Text
makeInitials name0 = name4
  where
    name1 = map (T.split (== '-')) (T.split isSpace name0)  -- [["Jean", "Baptiste"], ["Simon"]]
    name2 = (fmap . fmap) ((`T.cons` ".") . T.head) name1   -- [["J.", "B."], ["S."]]
    name3 = map (T.intercalate "-") name2                   -- ["J.-B.", "S."]
    name4 = T.intercalate " " name3                         -- "J.-B. S."
