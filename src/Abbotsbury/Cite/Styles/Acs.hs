-- | Defines the ACS citation style. This refers to the "long" citation style,
-- i.e. includes full author list, title, year, journal volume/issue, pages,
-- DOI.
module Abbotsbury.Cite.Styles.Acs
  ( acsStyle
  , acsShortStyle
  ) where

import           Abbotsbury.Cite.Helpers.Person
import           Abbotsbury.Cite.Internal
import           Abbotsbury.Work
import qualified Data.List                     as L
import qualified Data.List.NonEmpty            as NE
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Lens.Micro

data AcsLength = Short | Long

-- | The American Chemical Society style, as specified in the 3rd edition of the
-- ACS Style Guide. ACS seem to have removed this from their website in favour
-- of their [new version](https://pubs.acs.org/doi/book/10.1021/acsguide), but
-- it can still be accessed at 
-- <http://www.jlakes.org/config/hpkx/news_category/2017-02-14/ACS-StyleGuide.pdf>.
-- As far as I am aware, this is consistent with the newest Style Guide,
-- although I only have access to [a quick 'cheat sheet' of the new
-- version](https://pubs.acs.org/doi/full/10.1021/acsguide.40303).
--
-- >>> import qualified Data.Text.IO as TIO
-- >>> Right orgLett <- fetchWork "your@email.com" "10.1021/acs.orglett.9b00971"
-- >>> TIO.putStrLn $ cite acsStyle markdownFormat orgLett
-- Mansfield, S. J.; Smith, R. C.; Yong, J. R. J.; Garry, O. L.; Anderson, E. A. A General Copper-Ca
-- talyzed Synthesis of Ynamides from 1,2-Dichloroenamides. *Org. Lett.* **2019,** *21* (8), 2918-29
-- 22. DOI: [10.1021/acs.orglett.9b00971](https://doi.org/10.1021/acs.orglett.9b00971).
acsStyle :: Style
acsStyle = Style { articleConstructor = articleConstructorACS Long
                 , bookConstructor    = bookConstructorACS Long
                 }

-- A shorter version of the ACS style, useful for presentations etc. where
-- there's just not so much space.
acsShortStyle :: Style
acsShortStyle = Style { articleConstructor = articleConstructorACS Short
                      , bookConstructor    = bookConstructorACS Short
                      }

articleConstructorACS :: AcsLength -> Article -> CitationPart
articleConstructorACS lgth a = mconcat . L.intersperse space $ case lgth of
  Long  -> [authorP, titleP, journalInfoP, doiP]
  Short -> [authorP, journalInfoP]
 where
  authorP, titleP, journalInfoP, doiP :: CitationPart
  authorP = plain . addEndingDot . T.intercalate "; " $ fmap
    (formatPerson FamilyInitials)
    (NE.toList $ a ^. authors)
  titleP       = let t = a ^. title in plain (addEndingDot t)
  journalInfoP = formatJInfoACS a
  doiP         = plain "DOI: " <> mkDoiUri (a ^. doi) <> plain "."
  mkDoiUri :: DOI -> CitationPart
  mkDoiUri doi' = Link ("https://doi.org/" <> doi') (plain doi')

formatJInfoACS :: Article -> CitationPart
formatJInfoACS a = mconcat
  $ L.intersperse space [theJName, theYear, theVolInfo, thePages]
 where
  theJName = italic (a ^. journalShort)
  theYear  = bold (T.pack (show (a ^. year) ++ ","))
  thePages = plain . T.replace "-" "–" $ displayPages (a ^. pages) <> "."
  -- Whether the pagination part is empty will determine the punctuation used at
  -- the end of the volume info.
  endPunct   = if thePages == plain "." then "" else ","
  theVolInfo = case (a ^. volume, a ^. issue) of
    (""    , ""    ) -> mempty
    (""    , theIss) -> plain ("No. " <> theIss <> endPunct)
    (theVol, ""    ) -> italic (theVol <> endPunct)
    (theVol, theIss) ->
      italic theVol <> space <> plain (addParen theIss) <> plain endPunct
  addParen :: Text -> Text
  addParen t = "(" <> t <> ")"

bookConstructorACS :: AcsLength -> Book -> CitationPart
bookConstructorACS lgth b =
  mconcat . L.intersperse space . filter (/= mempty) $ case lgth of
    Long  -> [authorP, titleP, editorP, seriesP, publishP]
    Short -> [authorP, titleP, editorP, yearP]
 where
  authorP = plain . addEndingDot . T.intercalate "; " $ fmap
    (formatPerson FamilyInitials)
    (b ^. authors)
  titleP = case b ^. edition of
    ""  -> italic (b ^. title <> ";")
    edn -> italic (b ^. title <> ",") <> space <> plain (edn <> " ed.;")
  editorP = case b ^. editors of
    [] -> mempty
    [ed] ->
      plain . (<> ", Ed.;") . addEndingDot . formatPerson FamilyInitials $ ed
    eds -> plain . (<> ", Eds.;") . addEndingDot . T.intercalate ", " $ fmap
      (formatPerson FamilyInitials)
      eds
  seriesP = case (b ^. series, b ^. number) of
    ("" , "" ) -> mempty
    (ser, "" ) -> plain (ser <> ";")
    (ser, num) -> plain (ser <> " " <> num <> ";")
  yearText = T.pack . show $ b ^. year
  publishP = plain
    (b ^. publisher <> ": " <> b ^. publisherLoc <> ", " <> yearText <> ".")
  yearP = plain (yearText <> ".")

space :: CitationPart
space = plain " "

addEndingDot :: Text -> Text
addEndingDot t = if not (T.null t) && T.last t /= '.' then t <> "." else t
