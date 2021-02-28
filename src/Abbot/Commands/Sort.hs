module Abbot.Commands.Sort
  ( module Abbot.Commands.Sort
    ) where

import           Abbot.Commands.Shared
import           Abbot.Reference
import           Control.Monad.Except
import           Data.Char                      ( isUpper )
import qualified Data.IntMap                   as IM
import           Data.IntMap                    ( IntMap )
import           Data.List                      ( sortOn )
import qualified Data.Map                      as M
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
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
    Left  bundle -> throwError $ T.pack ("open: " ++ errorBundlePretty bundle)  -- parse error
    Right format -> do
      let safeThead x = if T.null x then Nothing else Just $ T.head x
      let reverseFn = maybe id (\x -> if isUpper x then reverse else id) (safeThead args)
      let originalRefs = IM.elems refs   -- no refnos
      let sortedRefs = case format of
            YearJournalAuthor -> sortOn (view year) originalRefs -- TODO this is not fully done
            TimeOpened        -> sortOn (view timeOpened) originalRefs
            TimeAdded         -> sortOn (view timeAdded) originalRefs
      liftIO $ TIO.putStrLn ("sorted references by " <> showT format)
      pure (IM.fromList $ zip [1..] (reverseFn sortedRefs), Nothing)


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
