module Abbot.Cite
  ( cite
  , makeParts
  , formatParts
  , formatOnePart
  , Abbot.Cite.Formats.Text.textFormat
  , Abbot.Cite.Formats.Markdown.markdownFormat
  , Abbot.Cite.Formats.Restructured.restructuredFormat
  , Abbot.Cite.Formats.HTML.htmlFormat
  , Abbot.Cite.Styles.ACS.acsStyle
  , Abbot.Cite.Internal.Rules(..)
  ) where


import           Abbot.Cite.Internal
import           Abbot.Cite.Formats.Text
import           Abbot.Cite.Formats.Markdown
import           Abbot.Cite.Formats.Restructured
import           Abbot.Cite.Formats.HTML
import           Abbot.Cite.Styles.ACS
import           Abbot.Work
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Lens.Micro.Platform


-- | Generate a citation for a work.
cite :: Rules -- ^ Citation rules to use.
  -> Work -- ^ The work to cite.
  -> Text -- ^ The citation.
cite (Rules style format) = formatParts format . makeParts style


-- | Using a citation style (part of the Rules), generate a list of CitationParts (i.e. Abbot's
-- internal abstract representation of formatted text).
makeParts :: Style -> Work -> [CitationPart]
makeParts style work = case wt of
  JournalArticle -> articleConstructor style work
  _              -> [CText ("work type " <> tshow wt <> " not supported yet")]
 where
  wt    = work ^. workType
  tshow = T.pack . show


-- | Using a citation format (part of the Rules), generate text that has concrete formatting from
-- the abstractly formatted CitationParts.
formatParts :: Format -> [CitationPart] -> Text
formatParts format = T.concat . map (formatOnePart format)


-- | Using a citation format (part of the Rules), generate text that has concrete formatting from
-- one single CitationPart.
formatOnePart
  :: Format -- ^ The citation output format to be used.
  -> CitationPart -- ^ The citation part to format.
  -> Text
formatOnePart fmt@(Format plainFormatter boldFormatter italicFormatter linkFormatter) part
  = case part of
         (CText t) -> plainFormatter t
         (Bold part') -> boldFormatter (formatOnePart fmt part')
         (Italic part') -> italicFormatter (formatOnePart fmt part')
         (Link uri part') -> linkFormatter uri (formatOnePart fmt part')
