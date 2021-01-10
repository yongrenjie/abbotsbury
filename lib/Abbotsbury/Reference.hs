module Abbotsbury.Reference
  ( module Abbotsbury.Reference
  ) where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T

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
  , timeAdded    :: Text
  , timeOpened   :: Text
  }

type DOI = Text
data Author = Author
  { given  :: Text
  , family :: Text
  }
