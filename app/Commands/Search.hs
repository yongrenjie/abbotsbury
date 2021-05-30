module Commands.Search
  ( runSearch
  ) where

import           Commands.Shared
import           Data.Char                      ( isAscii
                                                , isUpper
                                                )
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.Maybe                     ( catMaybes )
import qualified Data.Set                      as S
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Text.Normalize
import           Internal.Monad
import           Internal.PrettyRef
import           Lens.Micro.Platform
import           Reference

prefix :: Text
prefix = "search: "

runSearch :: Args -> CmdInput -> CmdOutput
runSearch args input = do
  let refs    = refsin input
      cwd     = cwdin input
      queries = T.words . searchNormalise $ args
  -- If no refs present, or no search terms specified error immediately
  errorOnNoRefs prefix input
  when (null queries) $ throwError (prefix <> "no search terms given")
  -- TODO: do the search
  let refsMatchingAllQueries = IM.filter (queries `allFoundIn`) refs
  if not (IM.null refsMatchingAllQueries)
    then printMatches cwd "all" refsMatchingAllQueries
    else do
      let refsMatchingAnyQueries = IM.filter (queries `anyFoundIn`) refs
      if not (IM.null refsMatchingAnyQueries)
        then printMatches cwd "any" refsMatchingAnyQueries
        else throwError $ prefix <> "no articles found"
  -- Return basically nothing
  pure $ SCmdOutput (refsin input) Nothing

printMatches :: MonadIO m => FilePath -> Text -> IntMap Reference -> m ()
printMatches cwd desc refs = liftIO $ do
  let n = IM.size refs
  TIO.putStrLn
    $  prefix
    <> "found "
    <> (T.pack . show $ n)
    <> " references matching "
    <> desc
    <> " queries"
  prettify cwd Nothing (IM.assocs refs) >>= TIO.putStrLn

-- | Makes a series of Text pieces containing reference metadata, against which
-- the search query/queries can be matched.
mkHaystack :: Reference -> [Text]
mkHaystack ref = map searchNormalise $ shared ++ tags' ++ individual
 where
  shared = [ T.unwords . map mkFullName . getContributors $ ref, getTitle ref]
  tags'  = S.toList (ref ^. tags)
  individual = case ref ^. work of
    ArticleWork a ->
      [ a ^. journalShort
      , a ^. journalLong
      , T.filter isUpper $ a ^. journalShort
      ]
    BookWork b -> [b ^. publisher, b ^. edition]
  -- | Generate a person's full name.
  mkFullName :: Person -> Text
  mkFullName p =
    T.unwords . catMaybes $ [p ^. given, Just (p ^. family), p ^. suffix]

-- | Strip diacritics and convert to 'folded case' to allow for case-insensitive
-- comparison.
searchNormalise :: Text -> Text
searchNormalise = T.toCaseFold . T.filter isAscii . normalize NFD

-- * The search queries passed to the following three functions MUST already
-- have been passed through the searchNormalise function. This condition is not
-- enforced.

foundIn :: Text -> Reference -> Bool
foundIn query ref = any (query `T.isInfixOf`) (mkHaystack ref)

allFoundIn :: [Text] -> Reference -> Bool
allFoundIn queries ref = all (`foundIn` ref) queries

anyFoundIn :: [Text] -> Reference -> Bool
anyFoundIn queries ref = any (`foundIn` ref) queries
