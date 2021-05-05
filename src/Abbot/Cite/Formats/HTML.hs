module Abbot.Cite.Formats.HTML where


import           Abbot.Cite.Formats.Internal    ( render
                                                , surroundWithTag
                                                )
import           Abbot.Cite.Internal            ( Format(..) )


-- | HTML formatter.
htmlFormat :: Format
htmlFormat = Format
  { plainFormatter  = id
  , boldFormatter   = surroundWithTag "b" []
  , italicFormatter = surroundWithTag "i" []
  , linkFormatter   = \url -> surroundWithTag "a" [("href", Just (render url))]
  }
