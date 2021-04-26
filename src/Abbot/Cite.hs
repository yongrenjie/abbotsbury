module Abbot.Cite (
  cite,
  makeParts,
  formatParts,
  formatOnePart,
  ) where


import           Abbot.Cite.Internal
import           Abbot.Work
import           Data.Text                      ( Text )
import qualified Data.Text                     as T


cite :: Rules -> Work -> Text
cite (Rules style format) = formatParts format . makeParts style


-- | Use a Format 
formatParts :: Format -> [CitationPart] -> Text
formatParts format = T.concat . map (formatOnePart format)


formatOnePart
  :: Format -- ^ The citation output format to be used.
  -> CitationPart -- ^ The citation part to format.
  -> Text
formatOnePart fmt@(Format pFr bFr iFr lFr) part
  = case part of
         (CText t) -> pFr t
         (Bold part') -> bFr (formatOnePart fmt part')
         (Italic part') -> iFr (formatOnePart fmt part')
         (Link uri part') -> lFr uri (formatOnePart fmt part')


makeParts :: Style -> Work -> [CitationPart]
makeParts _ _ = []
