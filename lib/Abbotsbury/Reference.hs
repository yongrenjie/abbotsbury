module Abbotsbury.Reference
  ( module Abbotsbury.Reference
  ) where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Time.Clock

data Reference = Article
  { title        :: Text
  , authors      :: [Author]
  , journalLong  :: Text
  , journalShort :: Text
  , year         :: Int
  , volume       :: Text
  , issue        :: Text
  , pages        :: Text
  , doi          :: DOI
  , timeAdded    :: UTCTime
  , timeOpened   :: UTCTime
  }

type DOI = Text
data Author = Author
  { given  :: Text
  , family :: Text
  }
