{-# LANGUAGE OverloadedStrings #-}

module Abbot.Path
  ( module Abbot.Path
  , System.Directory.setCurrentDirectory
  ) where

import           Abbotsbury

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

-- | Reads in a list of references from the YAML file in a folder, but
-- also does some pattern matching on the returned result so that error
-- messages are easier to deal with in the main loop.
readRefs :: FilePath -> IO (Either Text [Reference])
readRefs fp = do
  let fname = fp </> yamlFileName
  refs <- decodeFileEither fname
  case refs of
    Right newRefs -> pure $ Right newRefs
    Left (InvalidYaml (Just (YamlException excText))) ->
      if "Yaml file not found" `isInfixOf` excText
        then pure $ Right []    -- no file, refs are therefore empty
        else pure $ Left "other error"
    Left (AesonException _) ->
      pure $ Left ("The file "
                  <> T.pack fname
                  <> " was found, but it does not "
                  <> "contain articles in the correct format for abbotsbury."
                  )
    Left otherParseExc ->
      pure $ Left (T.pack $ show otherParseExc
                  <> "\nThis error is unexpected; please file a bug!"
                  )

-- | Saves a list of references to the given FilePath.
saveRefs :: [Reference] -> FilePath -> IO ()
saveRefs rs fp = case rs of
                      [] -> pure ()
                      _  -> encodeFile (fp </> yamlFileName) rs
