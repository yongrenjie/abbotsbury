module Abbot.Commands.Open
  ( module Abbot.Commands.Open
    ) where

import           Abbot.Commands.Shared
import           Abbot.Path                     ( PDFType(..)
                                                , getPDFPath
                                                )
import           Abbot.Reference
import           Control.Monad.Except
import           Data.Bifunctor                 ( bimap )
import qualified Data.IntMap                   as IM
import           Data.IntMap                    ( IntMap )
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IS
import           Data.List                      ( partition )
-- import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as TIO
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

showT :: OpenFormat -> Text
showT OpenFullText = "full text"
showT OpenSI       = "SI"
showT OpenWebURL   = "web URL"


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
      let jobs =
            [ (rno, fmt) | fmt <- S.toList formats, rno <- IS.toList refnos ]
      let openCommands = do
            (rno, fmt) <- jobs
            let openLink = T.unpack $ getOpenLink fmt (refs IM.! rno) cwd
            pure $ proc "open" [openLink]
      -- Run the commands.
      -- TODO: This is not parallelised. How to?
      processReturns <- liftIO
        $ mapM (`readCreateProcessWithExitCode` "") openCommands
      -- Check exit codes and return an error if any of them failed.
      let (successJobs, failedJobs) =
            bimap (map fst) (map fst)
              . partition ((== ExitSuccess) . view (_2 . _1))
              $ zip jobs processReturns
      let showJob :: (Int, OpenFormat) -> Text
          showJob (r, f) =
            mconcat ["       refno ", T.pack (show r), ", ", showT f]
      -- Print success message. In theory we could also tell the user which ones opened
      -- successfully, but I think it's too much noise.
      let successMsg =
            "open: successfully opened "
              <> (T.pack . show $ length successJobs)
              <> " reference(s)"
      unless (null successJobs) (liftIO $ TIO.putStrLn successMsg)
      -- Print error message.
      unless
        (null failedJobs)
        (do
          let errMsg =
                "open: failed to open the following references:\n"
                  <> T.intercalate "\n" (map showJob failedJobs)
          throwError errMsg
        )
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
