module Abbot.Cite.Internal where


import           Abbot.Work
import           Data.Text                      ( Text )
import           Text.URI                       ( URI )
-- Note that the URI imported here is from the `modern-uri` package.


-- | A set of Rules fully specifies how a citation (in the form of Text) is to be generated from
-- a Work. For more information, please see the documentation of the relevant type.
data Rules = Rules Style Format


-- | A Style refers to the citation style, i.e. ACS, Wiley (here called ACIE), RSC, etc.
data Style = Style
           {
               articleConstructor :: Work -> [CitationPart]
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
data Format = Format {
  plainFormatter  :: Text -> Text,
  boldFormatter   :: Text -> Text,
  italicFormatter :: Text -> Text,
  linkFormatter   :: URI  -> Text -> Text  -- ^ The second argument is the displayed text.
}


-- | "Formatted" parts of a citation, which can later be converted to real Text objects based on the
-- CiteFormat used.
data CitationPart = CText Text
                  | Bold CitationPart
                  | Italic CitationPart
                  | Link URI CitationPart
                  deriving (Eq, Show)
