module Abbotsbury.Cite.Internal where

import           Abbotsbury.Work
import           Data.Sequence                  ( (<|)
                                                , (><)
                                                , Seq(..)
                                                , (|>)
                                                )
import qualified Data.Sequence                 as Seq
import           Data.Text                      ( Text )

-- | A 'Style' is a citation style, i.e. a set of rules which defines what text
-- is to be used and how it is to be formatted. This is roughly analogous with
-- the idea of a CSL file.
--
-- BibLaTeX entries also count as a 'style': please see the module-level
-- Haddocks of "Abbotsbury" for an explanation.
--
-- A 'Style' can be created by specifying a series of rules which convert a
-- 'Work' to a list of 'CitationPart's, which are abstract representations of
-- formatted text. This is quite intricate, so is not really covered here. Also,
-- 'CitationPart' is not exported from the top-level "Abbotsbury" module: you
-- will have to import it from "Abbotsbury.Cite.Internal".
data Style = Style
  { articleConstructor :: Article -> CitationPart
  , bookConstructor    :: Book -> CitationPart
  }

-- | A 'Format' dictates how the abstract formatting is to be realised, which is
-- entirely orthogonal to the citation 'Style' itself. For example, different
-- 'Format's render bold text in different ways: 'plainFormat' ignores it,
-- 'markdownFormat' surrounds it in @**@, and so on.
--
-- A 'Format' can be specified completely by providing four functions which
-- describe how plain text, bolded text, italic text, and hyperlinks are to be
-- rendered.
--
-- For example, this is (roughly) the definition of 'markdownFormat':
--
-- @
-- markdownFormat :: Format
-- markdownFormat = Format
--   { plainFormatter  = id
--   , boldFormatter   = \\t -> "**" <> t <> "**"
--   , italicFormatter = \\t -> "**" <> t <> "**"
--   , linkFormatter   = \\url disp -> "[" <> disp <> "](" <> url <> ")"
--   }
-- @
data Format = Format
  { -- | This is almost always 'id'.
    plainFormatter  :: Text -> Text
  , boldFormatter   :: Text -> Text
  , italicFormatter :: Text -> Text
  ,
    -- | The first argument is the URL. The second argument is the displayed
    -- text.
    linkFormatter   :: Text -> Text -> Text
  }

-- | An abstract representation of formatted text, which can later be converted
-- to real "Data.Text.Text" objects based on the 'Format' used.
data CitationPart
  = CText Text
  | Bold CitationPart
  | Italic CitationPart
  | Link Text CitationPart
  | CSeq (Seq CitationPart)
  deriving (Eq, Show)

instance Semigroup CitationPart where
  (<>) :: CitationPart -> CitationPart -> CitationPart
  -- Symmetric cases.
  CText  t1  <> CText  t2  = CText (t1 <> t2)
  Bold   cp1 <> Bold   cp2 = Bold (cp1 <> cp2)
  Italic cp1 <> Italic cp2 = Italic (cp1 <> cp2)
  CSeq   cs1 <> CSeq   cs2 = CSeq (cs1 >< cs2)
  -- Appending or prepending to a CSeq.
  CSeq   cs  <> other      = case cs of
    Empty        -> other
    anythingElse -> CSeq (anythingElse |> other)
  other <> CSeq cs = case cs of
    Empty        -> other
    anythingElse -> CSeq (other <| anythingElse)
  -- Anything else, just form a new Seq.
  other1 <> other2 = CSeq (Seq.fromList [other1, other2])

instance Monoid CitationPart where
  mempty  = CText ""
  mappend = (<>)

-- | A more descriptive substitute for @CText@.
plain :: Text -> CitationPart
plain = CText

-- | Helper function to create a @Bold (CText t)@.
bold :: Text -> CitationPart
bold = Bold . CText

-- | Helper function to create an @Italic (CText t)@.
italic :: Text -> CitationPart
italic = Italic . CText

-- | Using a citation style, generate a 'CitationPart' (i.e. Abbotsbury's
-- internal abstract representation of formatted text) which represents the
-- entire citation.
makeCitationPart :: Style -> Work -> CitationPart
makeCitationPart style work = case work of
  ArticleWork a -> articleConstructor style a
  BookWork    b -> bookConstructor style b

-- | Using a specific output 'Format', generate text that has concrete
-- formatting from a 'CitationPart' (which could be a "Data.Sequence.Seq" of
-- 'CitationPart's).
formatCitationPart
  ::
  -- | The citation output format to be used.
     Format
  ->
  -- | The citation part to format.
     CitationPart
  -> Text
formatCitationPart fmt@(Format plainFormatter boldFormatter italicFormatter linkFormatter) part
  = case part of
    (CText  t      ) -> plainFormatter t
    (Bold   part'  ) -> boldFormatter (formatCitationPart fmt part')
    (Italic part'  ) -> italicFormatter (formatCitationPart fmt part')
    (Link uri part') -> linkFormatter uri (formatCitationPart fmt part')
    (CSeq parts'   ) -> foldMap (formatCitationPart fmt) parts'
