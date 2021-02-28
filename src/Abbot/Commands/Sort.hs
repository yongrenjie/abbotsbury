module Abbot.Commands.Sort
  ( module Abbot.Commands.Sort
    ) where

import           Abbot.Commands.Shared
import           Abbot.Reference
import           Control.Monad.Except
import           Data.Char                      ( isUpper )
import qualified Data.IntMap                   as IM
import           Data.IntMap                    ( IntMap )
import           Data.List                      ( sortBy )
import qualified Data.Map                      as M
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Ord                       ( comparing )
import           Data.Time.Clock                ( )
import           Lens.Micro.Platform
import           Text.Megaparsec                ( eof
                                                , errorBundlePretty
                                                , parse
                                                )


data SortCriterion = YearJournalAuthor
                   | TimeOpened
                   | TimeAdded
                   deriving (Ord, Eq, Show)

showT :: SortCriterion -> T.Text
showT YearJournalAuthor = "year, journal, author"
showT TimeOpened        = "time opened"
showT TimeAdded         = "time added"


runSort :: ReplArgs -> FilePath -> IntMap Reference -> CmdOutput
runSort args _ refs = do
  -- If no refs present, error immediately
  when (IM.null refs) (throwError "sort: no references found")
  -- Parse arguments: detect whether reversed order is desired...
  case parse pSort "" args of
    Left bundle -> throwError $ T.pack ("open: " ++ errorBundlePretty bundle)  -- parse error
    Right criterion -> do
      let reversed =
            not (T.null . T.stripStart $ args)
              && (isUpper . T.head . T.stripStart $ args)
      let originalRefs = IM.elems refs        -- no refnos
      let sortedRefs = sortBy (getSortOrdering criterion reversed) originalRefs
      liftIO $ TIO.putStrLn
        ("sorted references by " <> showT criterion <> if reversed
          then " (reversed)"
          else ""
        )
      pure (IM.fromList $ zip [1 ..] sortedRefs, Nothing)


pSort :: Parser SortCriterion
pSort = pOneFormat abbrevs (Just TimeAdded) <* eof
 where
  abbrevs = M.fromList
    [ ("year", YearJournalAuthor)
    , ("y", YearJournalAuthor)
    , ("opened", TimeOpened)
    , ("o", TimeOpened)
    , ("added", TimeAdded)
    , ("a", TimeAdded)
    ]

getSortOrdering :: SortCriterion -> Bool -> Reference -> Reference -> Ordering
getSortOrdering crit reversed x y =
  let flipOrder :: Ordering -> Ordering
      flipOrder LT = GT
      flipOrder GT = LT
      flipOrder EQ = EQ
  in  (if reversed then flipOrder else id) $ case crit of
        TimeOpened        -> comparing (view timeOpened) x y
        TimeAdded         -> comparing (view timeAdded) x y
        YearJournalAuthor -> comparing ((,,) <$> view year <*> view journalLong <*> view (authors . _head . family)) x y
