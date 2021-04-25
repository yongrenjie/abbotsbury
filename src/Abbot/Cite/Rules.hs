module Abbot.Cite.Rules where


import           Abbot.Work
import           Abbot.Cite.Part
import           Data.Text                      ( Text )


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
-- Once the full list of CitationParts has been generated (by applying the various Style rules), we
-- can then convert all the CitationParts into Text using the correct formatter.
data Format = PlainText
                | Markdown
                | Restructured
                | HTML
                deriving (Ord, Eq, Show, Enum, Bounded)


makeCitationParts :: Style -> Work -> [CitationPart]
makeCitationParts style work = undefined


cite :: Rules -> Work -> Text
cite (Rules style format) work = formatParts format cParts
  where
    formatParts = undefined
    cParts = makeCitationParts style work
