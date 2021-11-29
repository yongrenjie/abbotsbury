module Internal.Types
  ( RName(..)
  ) where

data RName = ViewportRefs | CacheRefs
           deriving (Eq, Show, Ord)
