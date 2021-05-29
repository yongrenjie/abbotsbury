module Commands.Open
  ( runOpen
  ) where

import           Commands.Shared
import           Data.Bifunctor                 ( bimap )
import qualified Data.IntMap                   as IM
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IS
import           Data.List                      ( foldl'
                                                , nub
                                                , partition
                                                )
import qualified Data.Map                      as M
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Time.Clock                ( getCurrentTime )
import           Internal.Monad
import           Internal.Path                  ( PDFType(..)
                                                , getPDFPath
                                                )
import           Lens.Micro.Platform
import           Reference
import           System.Exit                    ( ExitCode(..) )
import           System.Process                 ( proc
                                                , readCreateProcessWithExitCode
                                                )

prefix :: Text
prefix = "open: "

runOpen :: Args -> CmdInput -> CmdOutput
runOpen args input = do
  let cwd  = cwdin input
      refs = refsin input
  -- If no refs present, error immediately
  errorOnNoRefs prefix input
  -- Parse arguments
  (refnos, formats) <- parseInCommand pOpen args prefix
  let argsRefnos = resolveRefnosWith refs refnos
  -- Figure out which refnos to open
  refnosAndRefs <- getActiveRefs prefix argsRefnos True input
  -- Construct the links to be opened.
  let jobs =
        [ (rno, ref, fmt) | fmt <- S.toList formats, (rno, ref) <- refnosAndRefs ]
  let openCommands = do
        -- list monad
        (rno, ref, fmt) <- jobs
        let openLink = T.unpack <$> getOpenLink fmt ref cwd
        case openLink of
          Just x  -> pure $ proc "open" [x]
          Nothing -> []
  -- Run the commands.
  processReturns <- liftIO
    $ mapM (`readCreateProcessWithExitCode` "") openCommands
  -- Check exit codes and return an error if any of them failed.
  let (successJobs, failedJobs) =
        bimap (map fst) (map fst)
          . partition ((== ExitSuccess) . view (_2 . _1))
          $ zip jobs processReturns
  let showJob :: (Int, Reference, OpenFormat) -> Text
      showJob (r, _, f) =
        mconcat ["       ", refnoT r, showT f]
  -- Print success message. In theory we could also tell the user which ones opened
  -- successfully, but I think it's too much noise.
  let successMsg =
        "open: successfully opened "
          <> (T.pack . show $ length successJobs)
          <> " reference(s)"
  unless (null successJobs) (liftIO $ TIO.putStrLn successMsg)
  -- Deal with failed references if there are any.
  unless
    (null failedJobs)
    (  printError
    $  "open: failed to open the following references:\n"
    <> T.intercalate "\n" (map showJob failedJobs)
    )
  -- We don't ever want to throwError from within runOpen, because the last
  -- opened times of the refs always have to be updated, which we do here.
  currentTime <- liftIO getCurrentTime
  let updatedRefnos = nub $ successJobs ^.. each . _1
      updatedRefs   = foldl'
        (\rs rno -> set (ix rno . timeOpened) currentTime rs)
        refs
        updatedRefnos
  -- Return the updated refs.
  pure $ SCmdOutput updatedRefs Nothing

pOpen :: Parser (Refnos, Set OpenFormat)
pOpen = (,) <$> pRefnos <*> pFormats abbrevs (Just OpenFullText)
 where
  abbrevs = M.fromList
    [ ("p"  , OpenFullText)
    , ("pdf", OpenFullText)
    , ("s"  , OpenSI)
    , ("si" , OpenSI)
    , ("w"  , OpenWebURL)
    , ("web", OpenWebURL)
    ]

data OpenFormat
  = OpenFullText
  | OpenSI
  | OpenWebURL
  deriving (Ord, Eq, Show)

showT :: OpenFormat -> Text
showT OpenFullText = "full text"
showT OpenSI       = "SI"
showT OpenWebURL   = "web URL"

getOpenLink
  :: OpenFormat -- ^ The format to open in (full text, SI, web)
  -> Reference  -- ^ The reference to open
  -> FilePath   -- ^ Current working directory
  -> Maybe Text
getOpenLink fmt ref cwd = case fmt of
  OpenFullText -> Just (T.pack $ getPDFPath FullText cwd ref)
  OpenSI       -> Just (T.pack $ getPDFPath SI cwd ref)
  OpenWebURL   -> case ref ^. work of
    ArticleWork a -> Just $ "https://doi.org/" <> (a ^. doi)
    BookWork    b -> Nothing
