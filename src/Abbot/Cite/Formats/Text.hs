module Abbot.Cite.Formats.Text where


import           Abbot.Cite.Internal            ( Format(..) )


-- | Plain text, i.e. no formatting whatsoever.
textFormat :: Format
textFormat = Format { plainFormatter  = id
                    , boldFormatter   = id
                    , italicFormatter = id
                    , linkFormatter   = const id
                    }
