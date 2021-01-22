module Abbot.Commands.List
  ( module Abbot.Commands.List
  ) where

import           Abbot.Commands.Shared
import           Abbot.Reference

import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import qualified Data.IntSet                   as IS
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Lens.Micro.Platform
import           Text.Megaparsec

runList :: ReplArgs -> IntMap Reference -> CmdOutput
runList args refs = if IM.null refs
  then cmdErrS "list: no references found"
  else
    let parsedArgs = parse pRefnos "" args
        numRefs    = fst $ IM.findMax refs
    in  case parsedArgs of
          Left  bundle -> cmdErrS ("list: " ++ errorBundlePretty bundle)  -- parse error
          Right refnos -> if IS.null refnos
            then do
              prettyPrintRefHead
              mapM_ (prettyPrintRef refs) [1 .. numRefs]
              pure $ Right (refs, Just $ IS.fromList [1 .. numRefs])
            else case IS.lookupGT numRefs refnos <|> IS.lookupLT 1 refnos of
              Just x ->
                cmdErrS ("list: reference " <> show x <> " is out of bounds")
              Nothing -> do
                prettyPrintRefHead
                mapM_ (prettyPrintRef refs) (IS.toList refnos)
                pure $ Right (refs, Just refnos)

-- | Pretty-print the header of the reference list.
prettyPrintRefHead :: IO ()
prettyPrintRefHead = putStrLn "REFERENCES\n=========="

-- | Pretty-print a specific reference from the reference list, given a
-- (1-indexed) number. The output is (well, should be) based on the terminal
-- width.
prettyPrintRef :: IntMap Reference -> Int -> IO ()
prettyPrintRef refs index = do
  TIO.putStr (T.pack (show index) <> ": ")
  TIO.putStrLn (refs ^. ix index . title)
