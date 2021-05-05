{-# LANGUAGE QuasiQuotes #-}

module Cite.Helpers.Author
  ( tests
  ) where


import           Abbot.Cite
import           Abbot.Cite.Helpers.Author
import           Abbot.Work


import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@?=)
                                                , testCase
                                                )
import           Text.RawString.QQ


-- Define all possible inputs
authJon, authTim, authAli, authJB, authEriks :: Author
authJon = Author (Just "Jonathan Ren Jie") "Yong"
authTim = Author (Just "Tim D. W.") "Claridge"
authAli = Author (Just "Mohammadali") "Foroozandeh"
authJB = Author (Just "Jean-Baptiste") "Verstraete"   -- useful for hyphens. Thanks JB
authEriks = Author (Just "Ēriks") "Kupče"             -- test Unicode


allAuthors :: [Author]
allAuthors = [authJon, authTim, authAli, authJB, authEriks]


allFormats :: [AuthorStyle]
allFormats = [minBound .. maxBound]


expectedOutputsFamilyInitials :: Map Author Text
expectedOutputsFamilyInitials = M.fromList
  [ (authJon, "Yong, J. R. J.")
  , (authTim, "Claridge, T. D. W.")
  , (authAli, "Foroozandeh, M.")
  , (authJB , "Verstraete, J.-B.")
  , (authEriks, "Kupče, Ē.")
  ]


expectedOutputsInitialsFamily :: Map Author Text
expectedOutputsInitialsFamily = M.fromList
  [ (authJon, "J. R. J. Yong")
  , (authTim, "T. D. W. Claridge")
  , (authAli, "M. Foroozandeh")
  , (authJB , "J.-B. Verstraete")
  , (authEriks, "Ē. Kupče")
  ]


expectedOutputsBibLaTeX :: Map Author Text
expectedOutputsBibLaTeX = M.fromList
  [ (authJon, "Yong, Jonathan Ren Jie")
  , (authTim, "Claridge, Tim D.\\ W.")
  , (authAli, "Foroozandeh, Mohammadali")
  , (authJB , "Verstraete, Jean-Baptiste")
  , (authEriks, [r|Kup{\v{c}}e, {\=E}riks|])
  ]


allExpectedOutputs :: Map AuthorStyle (Map Author Text)
allExpectedOutputs = M.fromList
  [ (FamilyInitials, expectedOutputsFamilyInitials)
  , (InitialsFamily, expectedOutputsInitialsFamily)
  , (BibLaTeX      , expectedOutputsBibLaTeX)
  ]


-- Helper function to make tests from the expected outputs
mkTestCase :: AuthorStyle -> Author -> TestTree
mkTestCase fmt auth = testCase label (actual @?= expected)
 where
  actual   = formatOnePart textFormat (formatAuthor fmt auth)
  expected = allExpectedOutputs M.! fmt M.! auth
  label    = show fmt ++ "_" ++ T.unpack (_family auth)


tests :: TestTree
tests = testGroup
  "Author"
  [ mkTestCase fmt auth | fmt <- allFormats, auth <- allAuthors ]
