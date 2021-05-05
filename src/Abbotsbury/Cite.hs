module Abbotsbury.Cite
  ( cite
  , makeParts
  , formatParts
  , formatOnePart
  , Abbotsbury.Cite.Internal.Rules(..)
  ) where


import           Abbotsbury.Cite.Internal
import           Abbotsbury.Work
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Lens.Micro.Platform


-- | Generate a citation for a work.
cite
  :: Rules -- ^ Citation rules to use.
  -> Work -- ^ The work to cite.
  -> Text -- ^ The citation.
cite (Rules style format) = formatParts format . makeParts style


-- | Using a citation style (part of the Rules), generate a list of CitationParts (i.e. Abbotsbury's
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
    (CText  t      ) -> plainFormatter t
    (Bold   part'  ) -> boldFormatter (formatOnePart fmt part')
    (Italic part'  ) -> italicFormatter (formatOnePart fmt part')
    (Link uri part') -> linkFormatter uri (formatOnePart fmt part')
