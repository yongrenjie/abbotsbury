module Internal.Types
  ( RName(..)
  , WidgetID(..)
  ) where

data RName = ViewportRefs
  deriving (Eq, Show, Ord)

data WidgetID = References | Command | Search
              deriving (Eq, Show, Ord)
