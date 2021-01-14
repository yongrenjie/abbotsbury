module Abbot.Path
  ( module Abbot.Path
  , System.Directory.setCurrentDirectory
  ) where

import           Abbotsbury

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


-- | Reads in a list of references from a FilePath.
readRefs :: FilePath -> IO [Reference]
readRefs = undefined

-- | Saves a list of references to the given FilePath.
writeRefs :: [Reference] -> FilePath -> IO ()
writeRefs = undefined
