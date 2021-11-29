module Commands.Sort
  ( runSort
  ) where

import           Commands.Shared
import           Data.Char                      ( isUpper )
import qualified Data.IntMap                   as IM
import           Data.List                      ( sortBy )
import qualified Data.Map                      as M
import           Data.Maybe                     ( isJust )
import           Data.Ord                       ( comparing )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Internal.Monad
import           Lens.Micro.Platform
import           Reference
import           Text.Megaparsec                ( anySingle
                                                , lookAhead
                                                )

prefix :: Text
prefix = "sort: "

-- | Sorts the currently loaded database of references.
--
-- Command-line usage:
--
-- @
-- sort [criterion]
-- @
--
-- where @criterion@ is one of the following:
--
--     * @year@ or @y@: by year, using the journal and first author's family name to break ties
--     * @opened@ or @o@: by time last opened
--     * @added@ or @a@: by time added to the reference list
--
-- To sort by descending order, capitalise the first letter.
-- If no criterion is specified, then defaults to @year@ in ascending order.
runSort :: Args -> CmdInput -> CmdOutput
runSort args input = do
  let refs = refsin input
      vIn  = varin input
      cwd  = cwdin input
  -- If no refs present, error immediately
  errorOnNoRefs prefix input
  -- Parse arguments: detect whether reversed order is desired...
  criterion <- parseInCommand pSort args prefix
  -- Determine which refs to output
  let refsToSort = IM.assocs $ case vIn of
        Just vIn' -> refs `IM.restrictKeys` vIn'
        Nothing   -> refs
  -- If data was piped into 'sort' (i.e. vIn is Just _), then we *shouldn't*
  -- renumber the global reference list. Otherwise, if 'sort' was the only (or
  -- first) command, then we should.
  let sortedRefs = sortBy (getComparisonFn criterion) refsToSort
  let sortedRenumberedRefs = if isJust vIn
        then sortedRefs   -- was piped into, local sort only
        else zip [1 ..] (map snd sortedRefs) -- global sort
  -- The reference list we return should only be modified if 'sort' wasn't piped
  -- into.
  let refsout = if isJust vIn
        then refs        -- was piped into, local sort only
        else IM.fromList sortedRenumberedRefs  -- global sort
  pure $ SCmdOutput refsout vIn

-- | Parser for command-line arguments that determines what sort criterion is to
-- be used.
pSort :: Parser SortCriterion
pSort = do
  -- Check the first character to see if it is uppercase (which indicates
  -- descending order). If it isn't present, then lookAhead will fail and we can
  -- return a lowercase character to signify the default, i.e. ascending order.
  firstChar <- lookAhead anySingle <|> pure 'y'
  let sortOrder = if isUpper firstChar then Descending else Ascending
  -- Get the sort key.
  sortKey <- pOneFormat abbrevs (Just SKTimeAdded)
  -- Return.
  pure $ SortCriterion sortKey sortOrder
 where
  abbrevs = M.fromList
    [ ("year"  , SKYear)
    , ("y"     , SKYear)
    , ("opened", SKTimeOpened)
    , ("o"     , SKTimeOpened)
    , ("added" , SKTimeAdded)
    , ("a"     , SKTimeAdded)
    ]

-- | A @SortCriterion@ represents a criterion by which the list of references
-- may be sorted.
data SortCriterion = SortCriterion SortKey SortOrder

-- | There are currently three keys by which the references may be sorted.
data SortKey
  = -- | Sorting by year, using the journal name and then first author's family
    -- name to break ties.
    SKYear
  | -- | Sorting by the time a reference was last opened.
    SKTimeOpened
  | -- | Sorting by the time a reference was last added.
    SKTimeAdded

-- | The sort order.
data SortOrder = Ascending | Descending

-- | Convert a 'SortCriterion' into a Text instance which is displayed to the user when
-- sorting is performed.
showT :: SortCriterion -> T.Text
showT (SortCriterion key order) = showKey key <> showOrder order
 where
  showKey :: SortKey -> T.Text
  showKey SKYear = "year, journal, author"
  showKey SKTimeOpened        = "time opened"
  showKey SKTimeAdded         = "time added"
  showOrder :: SortOrder -> T.Text
  showOrder Ascending  = ""
  showOrder Descending = " (reversed)"

-- | Generates the comparison function to use for reference sorting.
getComparisonFn
  :: SortCriterion  -- ^ The sort criterion to be used.
  -> ((Int, Reference) -> (Int, Reference) -> Ordering)  -- ^ The comparison function.
getComparisonFn (SortCriterion key order) (_, ref1) (_, ref2) =
  let flipOrder :: Ordering -> Ordering
      flipOrder LT = GT
      flipOrder GT = LT
      flipOrder EQ = EQ
      reverseFn :: Ordering -> Ordering
      reverseFn = case order of
        Ascending  -> id
        Descending -> flipOrder
  in  reverseFn $ case key of
        SKTimeOpened -> comparing (^. timeOpened) ref1 ref2
        SKTimeAdded  -> comparing (^. timeAdded) ref1 ref2
        SKYear       -> comparing getYear ref1 ref2
