module Abbot.Commands.Open
  ( module Abbot.Commands.Open
    ) where

import           Abbot.Commands.Shared
import           Abbot.Path                     ( PDFType(..)
                                                , getPDFPath
                                                )
import           Abbot.Reference
import           Control.Monad.Except
import qualified Data.IntMap                   as IM
import           Data.IntMap                    ( IntMap )
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IS
-- import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.Text                      ( Text )
-- import qualified Data.Text.IO                  as TIO
import qualified Data.Text                     as T
import           Lens.Micro.Platform
import           System.Process                 ( proc
                                                , readCreateProcessWithExitCode
                                                )
import           Text.Megaparsec                ( eof
                                                , errorBundlePretty
                                                , parse
                                                )
import           System.Exit                    ( ExitCode(..) )


data OpenFormat = OpenFullText
                | OpenSI
                | OpenWebURL
                deriving (Ord, Eq, Show)

runOpen :: ReplArgs -> FilePath -> IntMap Reference -> CmdOutput
runOpen args cwd refs = do
  -- If no refs present, error immediately
  when (IM.null refs) (throwError "open: no references found")
  case parse pOpen "" args of
    Left bundle -> throwError $ T.pack ("open: " ++ errorBundlePretty bundle)  -- parse error
    Right (refnos, formats) -> do
      -- First, check for any refnos that don't exist
      let unavailableRefnos = refnos IS.\\ IM.keysSet refs
      unless
        (IS.null unavailableRefnos)
        (throwError
          (  "open: reference(s) "
          <> (T.intercalate "," . map (T.pack . show) . IS.toList $ refnos)
          <> " not found"
          )
        )
      -- Then, check if refnos is empty
      when (IS.null refnos) (throwError "open: no references selected")
      -- Construct the links to be opened.
      let openLinks =
            [ getOpenLink fmt (refs IM.! rno) cwd
            | fmt <- S.toList formats
            , rno <- IS.toList refnos
            ]
          openCommands = map (\x -> proc "open" [T.unpack x]) openLinks
      processReturns <- liftIO
        $ mapM (`readCreateProcessWithExitCode` "") openCommands
      -- Check exit codes and return an error if any of them failed.
      -- TODO: I would really like if this were smarter, i.e. it told us *exactly*
      -- which reference failed. We would need to preserve this information in
      -- openLinks, perhaps as a tuple.
      let exitCodes = processReturns ^.. each . _1
      when (any (/= ExitSuccess) exitCodes)
           (throwError "open: one or more references failed to open")
      pure (refs, Nothing)


pOpen :: Parser (IntSet, Set OpenFormat)
pOpen = ((,) <$> pRefnos <*> pFormats abbrevs (Just OpenFullText)) <* eof
 where
  abbrevs = M.fromList
    [ ("p"  , OpenFullText)
    , ("pdf", OpenFullText)
    , ("s"  , OpenSI)
    , ("si" , OpenSI)
    , ("w"  , OpenWebURL)
    , ("web", OpenWebURL)
    ]


getOpenLink
  :: OpenFormat  -- The format to open in (full text, SI, web)
  -> Reference   -- The reference to open
  -> FilePath    -- The current working directory
  -> Text        -- The link
getOpenLink fmt ref cwd = case fmt of
  OpenFullText -> T.pack $ getPDFPath FullText cwd ref
  OpenSI       -> T.pack $ getPDFPath SI cwd ref
  OpenWebURL   -> "https://doi.org/" <> ref ^. doi
