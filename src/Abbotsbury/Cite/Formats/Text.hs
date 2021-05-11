module Abbotsbury.Cite.Formats.Text where

import Abbotsbury.Cite.Internal (Format (..))

-- | Plain text, i.e. no formatting whatsoever.
textFormat :: Format
textFormat =
  Format
    { plainFormatter = id,
      boldFormatter = id,
      italicFormatter = id,
      linkFormatter = const id
    }
