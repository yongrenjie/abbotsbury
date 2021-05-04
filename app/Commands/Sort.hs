module Commands.Sort
  ( module Commands.Sort
    ) where

import           Commands.Shared
import           Reference
import           Control.Applicative            ( (<|>)
                                                , liftA3
                                                )
import           Control.Monad.Except
import           Data.Char                      ( isUpper )
import qualified Data.IntMap                   as IM
import           Data.List                      ( sortBy )
import qualified Data.Map                      as M
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Ord                       ( comparing )
import           Lens.Micro.Platform
import           Text.Megaparsec                ( eof
                                                , errorBundlePretty
                                                , parse
                                                , lookAhead
                                                , anySingle
                                                )


{-|
A @SortCriterion@ represents a criterion by which the list of references may be
sorted.
-}
data SortCriterion
  = SortCriterion
      SortKey    -- ^ The key to use for sorting.
      SortOrder  -- ^ Whether to sort in ascending or descending order.


{-|
There are currently three keys by which the references may be sorted.
-}
data SortKey
  -- | Sorting by year, using the journal name and then first author's family name to
  -- break ties.
    = SKYearJournalAuthor
  -- | Sorting by the time a reference was last opened.
    | SKTimeOpened
  -- | Sorting by the time a reference was last added.
    | SKTimeAdded


{-|
The sort order.
-}
data SortOrder = Ascending | Descending


{- | Convert a 'SortCriterion' into a Text instance which is displayed to the user when
sorting is performed.
-}
showT :: SortCriterion -> T.Text
showT (SortCriterion key order) = showKey key <> showOrder order
  where
    showKey :: SortKey -> T.Text
    showKey SKYearJournalAuthor = "year, journal, author"
    showKey SKTimeOpened        = "time opened"
    showKey SKTimeAdded         = "time added"
    showOrder :: SortOrder -> T.Text
    showOrder Ascending = ""
    showOrder Descending = " (reversed)"


{-|
Sorts the currently loaded database of references.

Command-line usage:

@
sort [criterion]
@

where @criterion@ is one of the following:

    * @year@ or @y@: by year, using the journal and first author's family name to break ties
    * @opened@ or @o@: by time last opened
    * @added@ or @a@: by time added to the reference list

To sort by descending order, capitalise the first letter.
If no criterion is specified, then defaults to @year@ in ascending order.
-}
runSort :: Args              -- ^ Arguments passed to the 'sort' command.
        -> CmdInput          -- ^ The inputs to the 'sort' command.
        -> CmdOutput
runSort args input = do
  let refs = refsin input
  -- If no refs present, error immediately
  when (IM.null refs) (throwError "sort: no references found")
  -- Parse arguments: detect whether reversed order is desired...
  case parse pSort "" args of
    Left bundle -> throwError $ T.pack ("open: " ++ errorBundlePretty bundle)  -- parse error
    Right criterion -> do
      let originalRefs = IM.elems refs       -- no refnos
      let sortedRefs = sortBy (getComparisonFn criterion) originalRefs
      liftIO $ TIO.putStrLn ("sorted references by " <> showT criterion)
      pure $ SCmdOutput (IM.fromList $ zip [1 ..] sortedRefs) Nothing


{-|
Parser for command-line arguments that determines what sort criterion is to be used.
-}
pSort :: Parser SortCriterion
pSort = do
  -- Check the first character to see if it is uppercase (which indicates descending
  -- order). If it isn't present, then lookAhead will fail and we can return a lowercase
  -- character to signify the default, i.e. ascending order.
  firstChar <- lookAhead anySingle <|> pure 'y'
  let sortOrder = if isUpper firstChar then Descending else Ascending
  -- Get the sort key.
  sortKey <- pOneFormat abbrevs (Just SKTimeAdded) <* eof
  -- Return.
  pure $ SortCriterion sortKey sortOrder
 where
  abbrevs = M.fromList
    [ ("year", SKYearJournalAuthor)
    , ("y", SKYearJournalAuthor)
    , ("opened", SKTimeOpened)
    , ("o", SKTimeOpened)
    , ("added", SKTimeAdded)
    , ("a", SKTimeAdded)
    ]


{-|
Generates the comparison function to use for reference sorting.
-}
getComparisonFn :: SortCriterion  -- ^ The sort criterion to be used.
                -> (Reference -> Reference -> Ordering)  -- ^ The comparison function.
getComparisonFn (SortCriterion key order) ref1 ref2 =
  let flipOrder :: Ordering -> Ordering
      flipOrder LT = GT
      flipOrder GT = LT
      flipOrder EQ = EQ
      reverseFn :: Ordering -> Ordering
      reverseFn = case order of
                       Ascending -> id
                       Descending -> flipOrder
  in  reverseFn $ case key of
        SKTimeOpened        -> comparing (view timeOpened) ref1 ref2
        SKTimeAdded         -> comparing (view timeAdded) ref1 ref2
        SKYearJournalAuthor -> comparing (liftA3 (,,)
                                                 (view $ work . year)
                                                 (view $ work . journalLong)
                                                 (view $ work . authors . ix 0 . family)
                                         ) ref1 ref2
