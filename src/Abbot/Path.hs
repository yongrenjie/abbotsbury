module Abbot.Path
  ( module Abbot.Path
  , System.Directory.setCurrentDirectory
  ) where

import           Abbot.Reference

import           Control.Monad.Except
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.List                      ( isInfixOf )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Yaml
import           Lens.Micro.Platform
import           System.Directory
import           System.FilePath

-- | Expands relative paths and tildes in directories. Tilde expansion does not
-- work with arbitrary users (`~user/path/to/file`), only the currently logged
-- in user (`~/path/to/file`). Relative paths are resolved with respect to the
-- current working directory.
expandDirectory :: FilePath -> IO FilePath
expandDirectory fp =
  let components = splitPath . dropTrailingPathSeparator $ fp
  in  (case components of
        []            -> getHomeDirectory
        ["/"        ] -> pure fp
        ["~"        ] -> getHomeDirectory
        ("~/" : rest) -> do
          home <- getHomeDirectory
          pure $ joinPath ((home ++ "/") : rest)
        _ -> pure fp
      )
        >>= canonicalizePath

-- | The file name where abbotsbury stores its information. In theory this
-- could be customisable, but in practice I can't be bothered.
yamlFileName :: FilePath
yamlFileName = "abbot.yaml"

-- | My own set of YAML errors, which is meant to make error reporting less
-- complicated, as we don't need the full set of ParseExceptions.
data MyYamlError = YamlFileNotFound
                 | InvalidYamlInFile
                 | OtherYamlError

-- | Simplify the full set of ParseExceptions into MyYamlError.
categoriseYamlError :: ParseException -> MyYamlError
categoriseYamlError (InvalidYaml (Just (YamlException excText)))
  | "Yaml file not found" `isInfixOf` excText = YamlFileNotFound
  | "mapping values are not allowed" `isInfixOf` excText = InvalidYamlInFile
  | otherwise = OtherYamlError
categoriseYamlError (AesonException _) = InvalidYamlInFile
categoriseYamlError _                  = OtherYamlError

-- | Reads in a list of references from the YAML file in a folder, but
-- also does some pattern matching on the returned result so that error
-- messages are easier to deal with in the main loop.
readRefs :: FilePath -> ExceptT Text IO (IntMap Reference)
readRefs cwd = do
  let fname = cwd </> yamlFileName
      invalidYamlErrMsg =
        "The file "
          <> T.pack fname
          <> " was found, but it does not "
          <> "contain articles in the correct "
          <> "format for abbotsbury."
  refs <- liftIO $ decodeFileEither fname
  case refs of
    -- Successfully parsed.
    Right newRefs  -> pure (IM.fromList $ zip [1..] newRefs)
    -- Some error
    Left  parseExc -> case categoriseYamlError parseExc of
      YamlFileNotFound  -> pure IM.empty
      InvalidYamlInFile -> throwError invalidYamlErrMsg
      OtherYamlError    -> throwError $ T.pack
        (show parseExc <> "\n This error is unexpected; please file a bug!")

-- | Saves a list of references to the given FilePath, as long as it's not empty.
-- This prevents us from creating files which didn't exist in the first place.
saveRefs :: IntMap Reference -> FilePath -> IO ()
saveRefs refs cwd = unless (IM.null refs)
                           (encodeFile (cwd </> yamlFileName) (IM.elems refs))

data PDFType = FullText
             | SI
             deriving (Ord, Eq, Show)

-- | Find the path to a PDF file belonging to a reference.
getPDFPath :: PDFType    -- Full text or SI.
           -> FilePath   -- Current working directory.
           -> Reference  -- The reference.
           -> FilePath   -- Path to the file.
getPDFPath pdfType cwd ref = cwd </> dirName </> fileName
  where
    dirName = case pdfType of
                   FullText -> "pdf-abbot"
                   SI       -> "si-abbot"
    -- TODO: The 'doi' getter will only work with articles. Books should use ISBN.
    fileName = T.unpack . flip T.append ".pdf" . T.replace "/" "#" $ ref ^. (work . doi)
