module Abbot.Cite.JInfo where


import           Abbot.Cite.Part
import           Abbot.Reference         hiding ( work )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Lens.Micro.Platform


-- more to be added...
data JInfoStyle = ACS
                deriving (Eq, Show, Enum, Bounded)


formatJInfo :: JInfoStyle -> Work -> [CitationPart]
formatJInfo style work = case style of
  ACS     -> formatACS
 where
  theYear  = work ^. year
  thePages = work ^. pages
  formatACS :: [CitationPart]
  formatACS = case (work ^. volume, work ^. issue) of
                   ("", "")     -> [Plain thePages]
                   ("", theIss) -> [Plain $ "No. " <> theIss <> ", " <> thePages]
                   (theVol, "") -> [Italic theVol, Plain "unfinished!!"]
