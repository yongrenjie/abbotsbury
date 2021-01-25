module Abbot.Commands.Open
  ( module Abbot.Commands.Open
    ) where

import           Abbot.Commands.Shared
import           Abbot.Reference
import           Control.Monad.Except
-- import qualified Data.IntMap                   as IM
import           Data.IntMap                    ( IntMap )
import           Data.IntSet                    ( IntSet )
-- import qualified Data.IntSet                   as IS
-- import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Set                       ( Set )
-- import qualified Data.Set                      as S
import           Text.Megaparsec                ( eof
                                                , parse
                                                )

data OpenFormat = OpenFullText
                | OpenSI
                | OpenWebURL
                deriving (Ord, Eq, Show)

runOpen :: ReplArgs -> FilePath -> IntMap Reference -> CmdOutput
runOpen args cwd refs = throwError "open: not yet implemented"

pOpen :: Parser (IntSet, Set OpenFormat)
pOpen = ((,) <$> pRefnos <*> pFormats abbrevs) <* eof
  where
    abbrevs = M.fromList [ ('p', OpenFullText)
                         , ('s', OpenSI)
                         , ('w', OpenWebURL) ]
