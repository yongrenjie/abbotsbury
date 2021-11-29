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
import           Text.Unidecode                 ( unidecode )

prefix :: Text
prefix = "search: "

runSearch :: Args -> CmdInput -> CmdOutput
runSearch args input = do
  let refs    = refsin input
      cwd     = cwdin input
      queries = T.words . searchNormalise $ args
      refsToSearchIn = case varin input of
                            Just s -> refs `IM.restrictKeys` s
                            Nothing -> refs
  -- If no refs present, or no search terms specified error immediately
  errorOnNoRefs prefix input
  when (null queries) $ throwError (prefix <> "no search terms given")
  -- Lazy evaluation means we won't run the second unnecessarily...
  let refsMatchingAll = IM.filter (queries `allFoundIn`) refsToSearchIn
      refsMatchingAny = IM.filter (queries `anyFoundIn`) refsToSearchIn
  if not (IM.null refsMatchingAll)
    then do
      printMatches cwd "all" refsMatchingAll
      pure $ SCmdOutput (refsin input) (Just $ IM.keysSet refsMatchingAll)
    else if not (IM.null refsMatchingAny)
      then do
        printMatches cwd "any" refsMatchingAny
        pure $ SCmdOutput (refsin input) (Just $ IM.keysSet refsMatchingAny)
      else throwError $ prefix <> "no articles found"

printMatches :: MonadIO m => FilePath -> Text -> IntMap Reference -> m ()
printMatches cwd desc refs = liftIO $ TIO.putStrLn "found matches"

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
searchNormalise = T.toCaseFold . T.concatMap (T.pack . unidecode)

-- * The search queries passed to the following three functions MUST already
-- have been passed through the searchNormalise function. This condition is not
-- enforced.

foundIn :: Text -> Reference -> Bool
foundIn query ref = any (query `T.isInfixOf`) (mkHaystack ref)

allFoundIn :: [Text] -> Reference -> Bool
allFoundIn queries ref = all (`foundIn` ref) queries

anyFoundIn :: [Text] -> Reference -> Bool
anyFoundIn queries ref = any (`foundIn` ref) queries
