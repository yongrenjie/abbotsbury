module Abbot.Cite.Formats.Markdown where


import           Abbot.Cite.Formats.Internal    ( render
                                                , surroundWith
                                                )
import           Abbot.Cite.Internal            ( Format(..) )


-- | Markdown formatter.
markdownFormat :: Format
markdownFormat = Format
  { plainFormatter  = id
  , boldFormatter   = surroundWith "**"
  , italicFormatter = surroundWith "*"
  , linkFormatter   = \url disp -> "[" <> disp <> "](" <> render url <> ")"
  }
