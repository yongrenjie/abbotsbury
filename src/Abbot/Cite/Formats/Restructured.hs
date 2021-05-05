module Abbot.Cite.Formats.Restructured where


import           Abbot.Cite.Formats.Internal    ( render
                                                , surroundWith
                                                )
import           Abbot.Cite.Internal            ( Format(..) )


-- | RestructuredText formatter.
restructuredFormat :: Format
restructuredFormat = Format
  { plainFormatter  = id
  , boldFormatter   = surroundWith "**"
  , italicFormatter = surroundWith "*"
  , linkFormatter   = \url disp -> "`" <> disp <> " <" <> render url <> ">`_"
  }
