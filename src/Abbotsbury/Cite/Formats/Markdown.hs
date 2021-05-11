module Abbotsbury.Cite.Formats.Markdown where

import Abbotsbury.Cite.Formats.Internal
  ( surroundWith,
  )
import Abbotsbury.Cite.Internal (Format (..))

-- | Markdown formatter.
markdownFormat :: Format
markdownFormat =
  Format
    { plainFormatter = id,
      boldFormatter = surroundWith "**",
      italicFormatter = surroundWith "*",
      linkFormatter = \url disp -> "[" <> disp <> "](" <> url <> ")"
    }
