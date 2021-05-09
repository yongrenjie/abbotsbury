module Abbotsbury.Cite
  ( cite
  , makeParts
  , formatParts
  , formatOnePart
  , Abbotsbury.Cite.Internal.Style(..)
  , Abbotsbury.Cite.Styles.ACS.acsStyle
  , Abbotsbury.Cite.Styles.Biblatex.bibStyle
  , Abbotsbury.Cite.Internal.Format(..)
  , Abbotsbury.Cite.Formats.Text.textFormat
  , Abbotsbury.Cite.Formats.Markdown.markdownFormat
  , Abbotsbury.Cite.Formats.Restructured.restructuredFormat
  , Abbotsbury.Cite.Formats.HTML.htmlFormat
  ) where


import           Abbotsbury.Cite.Formats.HTML
import           Abbotsbury.Cite.Formats.Markdown
import           Abbotsbury.Cite.Formats.Restructured
import           Abbotsbury.Cite.Formats.Text
import           Abbotsbury.Cite.Internal
import           Abbotsbury.Cite.Styles.ACS
import           Abbotsbury.Cite.Styles.Biblatex
import           Abbotsbury.Work
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Lens.Micro


-- | Generate a citation for a work.
cite
  :: Style -- ^ Citation style to use.
  -> Format -- ^ Output formatting to use.
  -> Work -- ^ The work to cite.
  -> Text -- ^ The citation.
cite style format = formatParts format . makeParts style


-- | Using a citation style, generate a list of CitationParts (i.e. Abbotsbury's internal abstract
-- representation of formatted text).
makeParts :: Style -> Work -> [CitationPart]
makeParts style work = case wt of
  JournalArticle -> articleConstructor style work
  _              -> [CText ("work type " <> tshow wt <> " not supported yet")]
 where
  wt    = work ^. workType
  tshow = T.pack . show


-- | Using a citation format, generate text that has concrete formatting from the abstractly
-- formatted CitationParts.
formatParts :: Format -> [CitationPart] -> Text
formatParts format = T.concat . map (formatOnePart format)


-- | Using a citation format, generate text that has concrete formatting from one single
-- CitationPart.
formatOnePart
  :: Format -- ^ The citation output format to be used.
  -> CitationPart -- ^ The citation part to format.
  -> Text
formatOnePart fmt@(Format plainFormatter boldFormatter italicFormatter linkFormatter) part
  = case part of
    (CText  t      ) -> plainFormatter t
    (Bold   part'  ) -> boldFormatter (formatOnePart fmt part')
    (Italic part'  ) -> italicFormatter (formatOnePart fmt part')
    (Link uri part') -> linkFormatter uri (formatOnePart fmt part')
