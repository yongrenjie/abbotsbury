module Abbotsbury.Cite.Formats.Restructured
  ( restructuredFormat
  ) where

import           Abbotsbury.Cite.Formats.Internal
                                                ( surroundWith )
import           Abbotsbury.Cite.Internal       ( Format(..) )

-- | reStructuredText formatter.
--
-- >>> import qualified Data.Text.IO as TIO
-- >>> Right orgLett <- fetchWork "your@email.com" "10.1021/acs.orglett.9b00971"
-- >>> TIO.putStrLn $ cite acsStyle restructuredFormat orgLett
-- Mansfield, S. J.; Smith, R. C.; Yong, J. R. J.; Garry, O. L.; Anderson, E. A. A General Copper-Ca
-- talyzed Synthesis of Ynamides from 1,2-Dichloroenamides. *Org. Lett.* **2019,** *21* (8), 2918-29
-- 22. DOI: `10.1021/acs.orglett.9b00971 <https://doi.org/10.1021/acs.orglett.9b00971>`_.
restructuredFormat :: Format
restructuredFormat = Format
  { plainFormatter  = id
  , boldFormatter   = surroundWith "**"
  , italicFormatter = surroundWith "*"
  , linkFormatter   = \url disp -> "`" <> disp <> " <" <> url <> ">`_"
  }
