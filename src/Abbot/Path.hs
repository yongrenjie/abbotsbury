module Abbot.Path
  ( module Abbot.Path
  , System.Directory.setCurrentDirectory
  ) where

import           Abbotsbury

import           Control.Monad                  ( unless )
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.List                      ( isInfixOf )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Yaml
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
yamlFileName = "peep.yaml"

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
readRefs :: FilePath -> IO (Either Text (IntMap Reference))
readRefs fp = do
  let fname = fp </> yamlFileName
      invalidYamlErrMsg =
        "The file "
          <> T.pack fname
          <> " was found, but it does not "
          <> "contain articles in the correct "
          <> "format for abbotsbury."
  refs <- decodeFileEither fname
  case refs of
    -- Successfully parsed.
    Right newRefs  -> pure $ Right (IM.fromList $ zip [1..] newRefs)
    -- Some error
    Left  parseExc -> case categoriseYamlError parseExc of
      YamlFileNotFound  -> pure $ Right IM.empty
      InvalidYamlInFile -> pure $ Left invalidYamlErrMsg
      OtherYamlError    -> pure $ Left $ T.pack
        (show parseExc <> "\n This error is unexpected; please file a bug!")

-- | Saves a list of references to the given FilePath.
saveRefs :: IntMap Reference -> FilePath -> IO ()
saveRefs refs fp = unless (IM.null refs)
                          (encodeFile (fp </> yamlFileName) (IM.elems refs))
