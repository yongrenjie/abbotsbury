module Abbotsbury.Cite.Formats.Restructured where

import           Abbotsbury.Cite.Formats.Internal
                                                ( surroundWith )
import           Abbotsbury.Cite.Internal       ( Format(..) )

-- | RestructuredText formatter.
restructuredFormat :: Format
restructuredFormat = Format
  { plainFormatter  = id
  , boldFormatter   = surroundWith "**"
  , italicFormatter = surroundWith "*"
  , linkFormatter   = \url disp -> "`" <> disp <> " <" <> url <> ">`_"
  }
