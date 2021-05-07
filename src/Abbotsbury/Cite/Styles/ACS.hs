-- | Defines the ACS citation style. This refers to the "long" citation style, i.e. includes full
-- author list, title, year, journal volume/issue, pages, DOI.
module Abbotsbury.Cite.Styles.ACS where


import           Abbotsbury.Cite.Helpers.Author
import           Abbotsbury.Cite.Internal
import           Abbotsbury.Work
import qualified Data.List                     as L
import qualified Data.List.NonEmpty            as NE
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import           Lens.Micro.Platform
import qualified Text.URI                      as URI


acsStyle :: Style
acsStyle = Style { articleConstructor = articleConstructorACS }


articleConstructorACS :: Work -> [CitationPart]
articleConstructorACS work = L.intercalate
  [space]
  [authorP, titleP, journalInfoP, doiP]
 where
  authorP, titleP, journalInfoP, doiP :: [CitationPart]
  authorP =
    [ CText $ T.intercalate
        "; "
        (fmap (formatAuthor FamilyInitials) (NE.toList $ work ^. authors))
    ]
  titleP =
    let t   = work ^. title
        end = if (not . T.null $ t) && (T.last t == '.') then "" else "."
    in  [CText $ (work ^. title) <> end]
  journalInfoP = formatJInfoACS work
  doiP         = [CText "DOI: ", mkDoiUri (work ^. doi), CText "."]


mkDoiUri :: DOI -> CitationPart
mkDoiUri doi' = case URI.mkURI ("https://doi.org/" <> doi') of
  Just uri -> Link uri (CText doi')
  Nothing  -> CText doi'


formatJInfoACS :: Work -> [CitationPart]
formatJInfoACS work = L.intercalate [space]
                                    [theJName, theYear, theVolInfo, thePages]
 where
  theJName = [Italic $ CText $ work ^. journalShort]
  theYear  = [Bold $ CText (T.pack (show (work ^. year) ++ ","))]
  thePages = case (work ^. pages, work ^. articleNumber) of
    ("", "") -> [CText "."]
    ("", aN) -> [CText ("No. " <> aN <> ".")]
    (pg, _ ) -> [CText (pg <> ".")]
  -- Whether the pagination part is empty will determine the punctuation used at the end of the
  -- volume info.
  endPunct :: Text
  endPunct   = if thePages == [CText "."] then "" else ","
  theVolInfo = case (work ^. volume, work ^. issue) of
    (""    , ""    ) -> []
    (""    , theIss) -> [CText $ "No. " <> theIss <> endPunct]
    (theVol, ""    ) -> [Italic (CText (theVol <> endPunct))]
    (theVol, theIss) ->
      [Italic (CText theVol), CText $ " (" <> theIss <> ")" <> endPunct]


space :: CitationPart
space = CText " "