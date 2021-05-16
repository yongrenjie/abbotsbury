module Crossref where

import           Abbotsbury.Crossref
import           Abbotsbury.Crossref.Internal
import           Abbotsbury.Work
import qualified Data.Aeson                    as Aeson
import qualified Data.ByteString.Lazy          as BL
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe                     ( fromJust )
import           Data.Text                      ( Text )
import qualified Data.IntMap                   as IM
import           Lens.Micro
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified TestWorks                     as TW

-- Testing parsing of various Crossref returned JSON.
-- Note that these are cached, i.e. the JSON is obtained from a file rather than dynamically via the
-- Internet. This is to make tests quicker and more reliable.
tests :: TestTree
tests = testGroup
  "Crossref"
  (map mkParseTest (IM.keys TW.testWorks))

mkParseTest :: Int -> TestTree
mkParseTest n = testCase ("JSON parsing - work "<> show n) $ do
  work <- fetchWorkFile ("tests/test-data/test" <> show n <> ".json")
  work @?= Right (TW.testWorks IM.! n)
