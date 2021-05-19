module Abbotsbury.Cite.Formats.HTML
  ( htmlFormat
  ) where

import           Abbotsbury.Cite.Formats.Internal
                                                ( surroundWithTag )
import           Abbotsbury.Cite.Internal       ( Format(..) )

-- | HTML formatter.
--
-- >>> import qualified Data.Text.IO as TIO
-- >>> Right orgLett <- fetchWork "your@email.com" "10.1021/acs.orglett.9b00971"
-- >>> TIO.putStrLn $ cite acsStyle htmlFormat orgLett
-- Mansfield, S. J.; Smith, R. C.; Yong, J. R. J.; Garry, O. L.; Anderson, E. A. A General Copper-Ca
-- talyzed Synthesis of Ynamides from 1,2-Dichloroenamides. <i>Org. Lett.</i> <b>2019,</b> <i>21</i>
--  (8), 2918-2922. DOI: <a href="https://doi.org/10.1021/acs.orglett.9b00971">10.1021/acs.orglett.9
-- b00971</a>.
htmlFormat :: Format
htmlFormat = Format
  { plainFormatter  = id
  , boldFormatter   = surroundWithTag "b" []
  , italicFormatter = surroundWithTag "i" []
  , linkFormatter   = \url -> surroundWithTag "a" [("href", Just url)]
  }
