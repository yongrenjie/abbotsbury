module Abbotsbury.Cite.Internal where

import           Abbotsbury.Work
import           Data.Text                      ( Text )

-- | A Style refers to the citation style, i.e. ACS, Wiley (here called ACIE), RSC, etc.
data Style = Style
  { articleConstructor :: Work -> [CitationPart]
  }

-- | A Format refers to the output file format, i.e. what kind of markup is to be applied to the
-- result. Note that the formatting is entirely orthogonal to the citation style (which is encoded
-- as Style). For example, an ACS-style citation (Style = ACS) can be formatted in plain text (for
-- use in Word), in RST (for use in Sphinx etc.) and so on.
--
-- A Format can be specified completely by providing four functions which describe how plain text,
-- bolded text, italic text, and hyperlinks are to be rendered.
--
-- Once the full list of CitationParts has been generated (by applying the various Style rules), we
-- can then convert all the CitationParts into Text using the correct formatter.
data Format = Format
  { plainFormatter  :: Text -> Text
  , boldFormatter   :: Text -> Text
  , italicFormatter :: Text -> Text
  ,
    -- | The first argument is the URI. The second argument is the displayed text.
    linkFormatter   :: Text -> Text -> Text
  }

-- | "Formatted" parts of a citation, which can later be converted to real Text objects based on the
-- CiteFormat used.
data CitationPart
  = CText Text
  | Bold CitationPart
  | Italic CitationPart
  | Link Text CitationPart
  deriving (Eq, Show)
