module Abbotsbury.Cite.Formats.HTML where


import           Abbotsbury.Cite.Formats.Internal
                                                ( surroundWithTag )
import           Abbotsbury.Cite.Internal       ( Format(..) )


-- | HTML formatter.
htmlFormat :: Format
htmlFormat = Format
  { plainFormatter  = id
  , boldFormatter   = surroundWithTag "b" []
  , italicFormatter = surroundWithTag "i" []
  , linkFormatter   = \url -> surroundWithTag "a" [("href", Just url)]
  }
