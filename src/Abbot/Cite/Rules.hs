module Abbot.Cite.Rules where


import           Abbot.Work
import           Abbot.Cite.Author
import           Abbot.Cite.JInfo
import           Abbot.Cite.Part


-- | A set of CiteRules fully specifies how a citation (in the form of Text) is to be generated
-- from a Work. For information about the individual fields, please see the documentation of the
-- relevant type constructor.
data CiteRules = CiteRules
  { cStyle  :: CiteStyle  -- ^ The citation style to be used, i.e. ACS or ACIE or so on.
  , cFormat :: CiteFormat -- ^ The way to format the citation, i.e. plain text or a markup language.
  }


-- | A CiteStyle refers to the citation style, i.e. ACS, Wiley (here called ACIE), RSC, etc. This
-- citation style is itself defined by several components, which tell us how to generate the
-- individual parts of the citation.
data CiteStyle = CiteStyle
               {
                   -- | How to style each individual author.
                   authorStyle :: AuthorStyle
               ,
                   -- | A function which joins the individual authors into a list.
                   authorGlue  :: [CitationPart] -> CitationPart
               ,
                   -- | How to style the journal information (name, year volume, issue, pages).
                   jinfoStyle  :: JInfoStyle
               ,
                   -- | Whether to include the article title in the citation.
                   showTitle   :: Bool
               ,
                   -- | Whether to include the DOI in the citation.
                   showDOI     :: DOIRule
               ,
                   -- | See below.
                   glue        :: GlueFunction
               }


-- | All of the Style instructions tell us how to generate individual components of the citation.
-- The last thing to do is to string them together with appropriate punctuation, etc. between them:
-- this is accomplished by the "glue" function.
type GlueFunction = CitationPart   -- ^ Authors (already joined with authorGlue)
                 -> CitationPart   -- ^ Title
                 -> CitationPart   -- ^ Journal name
                 -> CitationPart   -- ^ Journal info
                 -> CitationPart   -- ^ DOI
                 -> [CitationPart] -- ^ The citation output


-- | A DOIRule specifies when the DOI should be included in the citation.
data DOIRule = Never
             | OnlyASAP  -- ^ i.e. only for newly published articles which don't have full volume info.
             | Always
             deriving (Ord, Eq, Show, Enum, Bounded)


-- | A CiteFormat refers to the output file format, i.e. what kind of markup is to be applied to the
-- result. Note that the formatting is entirely orthogonal to the citation style (which is encoded
-- as CiteStyle). For example, an ACS-style citation (CiteStyle = ACS) can be formatted in plain
-- text (for use in Word), in RST (for use in Sphinx etc.) and so on.
-- 
-- Once the full list of CitationParts has been generated (by applying the various CiteStyle rules),
-- we can then convert all the CitationParts into Text using the correct formatter.
data CiteFormat = PlainText
                | Markdown
                | Restructured
                | HTML
                deriving (Ord, Eq, Show, Enum, Bounded)


makeCitation :: CiteRules -> Work -> [CitationPart]
makeCitation crules work = undefined
