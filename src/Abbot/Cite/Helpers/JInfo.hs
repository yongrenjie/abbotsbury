module Abbot.Cite.JInfo where


import           Abbot.Cite.Part
import           Abbot.Work
-- import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Lens.Micro.Platform


import qualified Data.List.NonEmpty as NE
authTim, authEriks :: Author
authTim = Author (Just "Tim D. W.") "Claridge"
authEriks = Author (Just "Ēriks") "Kupče"             -- test Unicode
testWork :: Work
testWork = Article
  { _title        = "NOAH: NMR Supersequences for Small Molecule Analysis and Structure Elucidation"
  , _authors      = authEriks NE.:| [authTim]
  , _journalLong  = "Angewandte Chemie International Edition"
  , _journalShort = "Angew. Chem. Int. Ed."
  , _year         = 2017
  , _volume       = "56"
  , _issue        = "39"
  , _pages        = "11779-11783"
  , _doi          = "10.1002/anie.201705506"
  }


fJInfoACS :: Work -> [CitationPart]
fJInfoACS work = [theJName, theYear] ++ theVolInfo ++ [thePages]
  where
    theJName = CText $ work ^. journalShort
    theYear  = Bold $ CText (T.pack (show (work ^. year) ++ ","))
    thePages = CText $ work ^. pages
    theVolInfo = case (work ^. volume, work ^. issue) of
                      (""    , ""    ) -> []
                      (""    , theIss) -> [CText $ "No. " <> theIss <> ", "]
                      (theVol, ""    ) -> [Italic (CText (theVol <> ","))]
                      (theVol, theIss) ->
                        [Italic (CText theVol), CText $ " (" <> theIss <> ")"]
