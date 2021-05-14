-- | Defines the ACS citation style. This refers to the "long" citation style, i.e. includes full
-- author list, title, year, journal volume/issue, pages, DOI.
module Abbotsbury.Cite.Styles.ACS where

import           Abbotsbury.Cite.Helpers.Author
import           Abbotsbury.Cite.Internal
import           Abbotsbury.Work
import qualified Data.List                     as L
import qualified Data.List.NonEmpty            as NE
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           GHC.Records

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
acsStyle = Style { articleConstructor = articleConstructorACS
                 , bookConstructor = const (plain "not yet done") }

articleConstructorACS :: JournalArticle -> CitationPart
articleConstructorACS a = mconcat
  $ L.intersperse space [authorP, titleP, journalInfoP, doiP]
 where
  authorP, titleP, journalInfoP, doiP :: CitationPart
  authorP = plain $ T.intercalate
    "; "
    (fmap (formatAuthor FamilyInitials) (NE.toList $ getField @"_authors" a))
  titleP =
    let t   = getField @"_title" a
        end = if (not . T.null $ t) && (T.last t == '.') then "" else "."
    in  plain $ t <> end
  journalInfoP = formatJInfoACS a
  doiP         = plain "DOI: " <> mkDoiUri (getField @"_doi" a) <> plain "."
  mkDoiUri :: DOI -> CitationPart
  mkDoiUri doi' = Link ("https://doi.org/" <> doi') (plain doi')

formatJInfoACS :: JournalArticle -> CitationPart
formatJInfoACS a = mconcat
  $ L.intersperse space [theJName, theYear, theVolInfo, thePages]
 where
  theJName = italic (_journalShort a)
  theYear  = bold (T.pack (show (getField @"_year" a) ++ ","))
  thePages = case (_pages a, _articleNumber a) of
    ("", "") -> plain "."
    ("", aN) -> plain ("No. " <> aN <> ".")
    (pg, _ ) -> plain (pg <> ".")
  -- Whether the pagination part is empty will determine the punctuation used at
  -- the end of the volume info.
  endPunct   = if thePages == plain "." then "" else ","
  theVolInfo = case (_volume a, _issue a) of
    (""    , ""    ) -> mempty
    (""    , theIss) -> plain ("No. " <> theIss <> endPunct)
    (theVol, ""    ) -> italic (theVol <> endPunct)
    (theVol, theIss) ->
      italic theVol <> space <> plain (addParen theIss) <> plain endPunct
  addParen :: Text -> Text
  addParen t = "(" <> t <> ")"

space :: CitationPart
space = plain " "
