module Abbotsbury.Cite.Formats.Text where

import           Abbotsbury.Cite.Internal       ( Format(..) )

-- | Plain text, i.e. no formatting whatsoever.
--
-- >>> import qualified Data.Text.IO as TIO
-- >>> Right orgLett <- fetchWork "your@email.com" "10.1021/acs.orglett.9b00971"
-- >>> TIO.putStrLn $ cite acsStyle textFormat orgLett
-- Mansfield, S. J.; Smith, R. C.; Yong, J. R. J.; Garry, O. L.; Anderson, E. A. A General Copper-Ca
-- talyzed Synthesis of Ynamides from 1,2-Dichloroenamides. Org. Lett. 2019, 21 (8), 2918-2922. DOI:
--  10.1021/acs.orglett.9b00971.
textFormat :: Format
textFormat = Format { plainFormatter  = id
                    , boldFormatter   = id
                    , italicFormatter = id
                    , linkFormatter   = const id
                    }
