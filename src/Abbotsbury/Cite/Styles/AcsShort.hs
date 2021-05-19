-- | Defines the ACS citation style. This refers to a "short" citation style,
-- which omits the title and the DOI.
module Abbotsbury.Cite.Styles.AcsShort
  ( acsShortStyle
  ) where

import           Abbotsbury.Cite.Helpers.Person
import           Abbotsbury.Cite.Internal
import           Abbotsbury.Work
import qualified Data.List                     as L
import qualified Data.List.NonEmpty            as NE
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Lens.Micro

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
-- >>> TIO.putStrLn $ cite acsShortStyle markdownFormat orgLett
-- Mansfield, S. J.; Smith, R. C.; Yong, J. R. J.; Garry, O. L.; Anderson, E. A. *Org. 
-- Lett.* **2019,** *21* (8), 2918–2922. DOI: [10.1021/acs.orglett.9b00971](https://d
-- oi.org/10.1021/acs.orglett.9b00971).
acsShortStyle :: Style
acsShortStyle = Style { articleConstructor = articleConstructorACSShort
                      , bookConstructor    = bookConstructorACSShort
                      }

articleConstructorACSShort :: Article -> CitationPart
articleConstructorACSShort a = mconcat
  $ L.intersperse space [authorP, journalInfoP]
 where
  authorP, journalInfoP :: CitationPart
  authorP = plain . addEndingDot . T.intercalate "; " $ fmap
    (formatPerson FamilyInitials)
    (NE.toList $ a ^. authors)
  journalInfoP = formatJInfoACS a

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

bookConstructorACSShort :: Book -> CitationPart
bookConstructorACSShort b =
  mconcat
    . L.intersperse space
    . filter (/= mempty)
    $ [authorP, titleP, editorP, yearP]
 where
  authorP = plain . addEndingDot . T.intercalate "; " $ fmap
    (formatPerson FamilyInitials)
    (b ^. authors)
  titleP = case b ^. edition of
    ""  -> italic (b ^. title <> ";")
    edn -> italic (b ^. title <> ", ") <> plain (edn <> ";")
  editorP = case b ^. editors of
    []  -> mempty
    eds -> plain . (<> ", Eds.;") . addEndingDot . T.intercalate "; " $ fmap
      (formatPerson FamilyInitials)
      eds
  yearP = plain ((T.pack . show $ b ^. year) <> ".")

space :: CitationPart
space = plain " "

addEndingDot :: Text -> Text
addEndingDot t = if not (T.null t) && T.last t /= '.' then t <> "." else t
