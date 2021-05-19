{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Defines the BibLaTeX "citation style". In practice, this actually gives an entry for a .bib
-- file (it isn't a true citation style per se).
module Abbotsbury.Cite.Styles.Biblatex
  ( bibStyle
  ) where

import           Abbotsbury.Cite.Helpers.Person
import           Abbotsbury.Cite.Internal
import           Abbotsbury.LatexEscapes
import           Abbotsbury.Work
import           Data.Char                      ( isAscii
                                                , isUpper
                                                )
import           Data.Foldable                  ( toList )
import           Data.Maybe                     ( mapMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Normalize            ( NormalizationMode(..)
                                                , normalize
                                                )
import           Lens.Micro

-- | This generates a BibLaTeX entry. The output 'Format' chosen doesn't affect
-- the BibLaTeX entry in any way, so that can be arbitrarily selected.
--
-- __Note__ that the output of this is only designed for use with @BibLaTeX@.
-- There is no guarantee that it will be compatible with the classical @BibTeX@
-- format.
--
-- >>> import qualified Data.Text.IO as TIO
-- >>> Right orgLett <- fetchWork "your@email.com" "10.1021/acs.orglett.9b00971"
-- >>> TIO.putStrLn $ cite bibStyle textFormat orgLett
-- @article{MansfieldOL2019,
--     doi = {10.1021/acs.orglett.9b00971},
--     author = {Mansfield, Steven J.\ and Smith, Russell C.\ and Yong, Jonathan R.\ J.\ and Garry,
-- Olivia L.\ and Anderson, Edward A.},
--     journaltitle = {Org.\ Lett.},
--     title = {A General Copper-Catalyzed Synthesis of Ynamides from 1,2-Dichloroenamides},
--     year = {2019},
--     volume = {21},
--     number = {8},
--     pages = {2918-2922},
-- }
bibStyle :: Style
bibStyle = Style { articleConstructor = articleConstructorBib
                 , bookConstructor    = bookConstructorBib
                 }

-- BibLaTeX package documentation about the article.
-- An article in a journal, magazine, newspaper, or other periodical which forms
-- a self-contained unit with its own title. The title of the periodical is
-- given in the journaltitle field. If the issue has its own title in addition
-- to the main title of the periodical, it goes in the issuetitle field. Note
-- that editor and related fields refer to the journal while translator and
-- related fields refer to the article.
--
-- Required fields: author, title, journaltitle, year/date
--
-- Optional fields: translator, annotator,
-- commentator, subtitle, titleaddon, editor, editora, editorb, editorc,
-- journalsubtitle, journaltitleaddon, issuetitle, issuesubtitle,
-- issuetitleaddon, language, origlanguage, series, volume, number, eid, issue,
-- month, pages, version, note, issn, addendum, pubstate, doi, eprint,
-- eprintclass, eprinttype, url, urldate

-- | In practice, we do all of the work as "Data.Text.Text", before converting
-- it to a 'CitationPart'.
articleConstructorBib :: Article -> CitationPart
articleConstructorBib a = plain (latexReplaceEscapes t)
 where
  t :: Text
  t = T.intercalate "\n" ([headerL] ++ fields ++ ["}"])
  bibIdentifier :: Text
  bibIdentifier =
    toAscii (a ^. authors . ix 0 . family)
      <> (T.pack . show) (a ^. year)
      <> T.filter isUpper (a ^. journalShort)
  headerL :: Text
  headerL = "@article{" <> bibIdentifier <> ","
  -- The components of these tuples are the arguments to makeMaybeBibFieldWith.
  rules :: [(Text -> Bool, Text, Text)]
  rules =
    [ (always   , "doi"         , a ^. doi)
    , (always   , "author"      , makePersonValue $ a ^. authors)
    , (always   , "title"       , a ^. title)
    , (always   , "journaltitle", latexReplaceSpaces $ a ^. journalShort)
    , (always   , "year"        , T.pack . show $ a ^. year)
    , (ifNotNull, "volume"      , a ^. volume)
    , (ifNotNull, "number"      , a ^. issue)
    , (always, "pages", T.replace "-" "--" . displayPages $ a ^. pages)
    ]
  fields :: [Text]
  fields = mapMaybe (\(a, b, c) -> makeMaybeBibFieldWith a b c) rules
  -- Note that the bibtex 'number' field should be used for the issue, even if
-- it is non-numeric. See section 2.3.11 of the BibLaTeX package documentation.

-- BibLaTeX package documentation about the book.
-- A single-volume book with one or more authors where the authors share credit
-- for the work as a whole. This entry type also covers the function of the
-- @inbook type of traditional BibTeX, see § 2.3.1 for details.
--
-- Required fields: author, title, year/date
--
-- Optional fields: editor, editora, editorb, editorc, translator, annotator,
-- commentator, introduction, foreword, afterword, subtitle, titleaddon,
-- maintitle, mainsubtitle, maintitleaddon, language, origlanguage, volume,
-- part, edition, volumes, series, number, note, publisher, location, isbn, eid,
-- chapter, pages, pagetotal, addendum, pubstate, doi, eprint, eprintclass,
-- eprinttype, url, urldate

-- | For books.
bookConstructorBib :: Book -> CitationPart
bookConstructorBib b = plain (latexReplaceEscapes t)
 where
  t :: Text
  t = T.intercalate "\n" ([headerL] ++ fields ++ ["}"])
  bibIdentifier :: Text
  bibIdentifier =
    toAscii (getContributors b ^. _head . family) <> T.pack (show (b ^. year))
  headerL :: Text
  headerL = "@book{" <> bibIdentifier <> ","
  -- We need to precalculate whether to include an 'edition' key.
  doEdition :: Bool
  doEdition = not . T.null $ b ^. edition
  rules :: [(Text -> Bool, Text, Text)]
  rules =
    [ (ifNotNull      , "author"   , makePersonValue $ b ^. authors)
    , (ifNotNull      , "editor"   , makePersonValue $ b ^. editors)
    , (always         , "title"    , b ^. title)
    , (const doEdition, "edition"  , b ^. edition <> " ed.")
    , (always         , "year"     , T.pack . show $ b ^. year)
    , (always         , "publisher", b ^. publisher)
    , (always         , "location" , b ^. publisherLoc)
    , (always         , "isbn"     , b ^. isbn)
    , (ifNotNull      , "series"   , b ^. series)
    , (ifNotNull      , "number"   , b ^. number)
    ]
  fields :: [Text]
  fields = mapMaybe (\(a, b, c) -> makeMaybeBibFieldWith a b c) rules

-- | Shortcut to make a BibLaTeX key-value pair. If the value is not guaranteed
-- to be present, then use 'makeMaybeBibField'.
makeBibField
  :: Text -- ^ The key.
  -> Text -- ^ The value.
  -> Text -- ^ The line to be printed to the bib file.
makeBibField key val = "    " <> key <> " = {" <> val <> "},"

-- | Makes a BibLaTeX field if a certain predicate is satisfied. Generalised form of
-- 'makeMaybeBibField'.
makeMaybeBibFieldWith
  :: (Text -> Bool) -- ^ The predicate
  -> Text -- ^ The key.
  -> Text -- ^ The value (which the predicate is tested against).
  -> Maybe Text -- ^ Just the line, or Nothing if predicate fails.
makeMaybeBibFieldWith pred key val =
  if pred val then Just (makeBibField key val) else Nothing

-- | Convert a name to plain ASCII. As best as we can. That is, separate all the
-- diacritics from the original letters (that's what NFD does), and then remove
-- all the diacritics, leaving the original letters behind.
toAscii :: Text -> Text
toAscii = T.filter isAscii . normalize NFD

-- | Generate the BibLaTeX-formatted value for the @author@ key, i.e. joined by
-- @and@s.
makePersonValue :: Foldable t => t Person -> Text
makePersonValue = T.intercalate " and " . fmap (formatPerson BibLaTeX) . toList

-- | A helpful predicate.
always :: Text -> Bool
always = const True

-- | Another helpful predicate.
ifNotNull :: Text -> Bool
ifNotNull = not . T.null
