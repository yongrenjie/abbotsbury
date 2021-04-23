module Reference
  ( tests
  ) where

import           Abbot.Reference                ( Author(Author, _family)
                                                , AuthorFormatting(..)
                                                , formatAuthor
                                                )

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


-- Define all possible inputs
authJon, authTim, authAli, authJB :: Author
authJon = Author (Just "Jonathan Ren Jie") "Yong"
authTim = Author (Just "Tim D. W.") "Claridge"
authAli = Author (Just "Mohammadali") "Foroozandeh"
authJB = Author (Just "Jean-Baptiste") "Verstraete"   -- useful for hyphens. Thanks JB

-- Define all possible (input, output) pairs
expectedOutputsListCmd :: Map Author Text
expectedOutputsListCmd = M.fromList
  [ (authJon, "JRJ Yong")
  , (authTim, "TDW Claridge")
  , (authAli, "M Foroozandeh")
  , (authJB , "JB Verstraete")
  ]

expectedOutputsFamilyInitials :: Map Author Text
expectedOutputsFamilyInitials = M.fromList
  [ (authJon, "Yong, J. R. J.")
  , (authTim, "Claridge, T. D. W.")
  , (authAli, "Foroozandeh, M.")
  , (authJB , "Verstraete, J.-B.")
  ]

expectedOutputsInitialsFamily :: Map Author Text
expectedOutputsInitialsFamily = M.fromList
  [ (authJon, "J. R. J. Yong")
  , (authTim, "T. D. W. Claridge")
  , (authAli, "M. Foroozandeh")
  , (authJB , "J.-B. Verstraete")
  ]

expectedOutputsBibLaTeX :: Map Author Text
expectedOutputsBibLaTeX = M.fromList
  [ (authJon, "Yong, Jonathan Ren Jie")
  , (authTim, "Claridge, Tim D.\\ W.")
  , (authAli, "Foroozandeh, Mohammadali")
  , (authJB , "Verstraete, Jean-Baptiste")
  ]

allExpectedOutputs :: Map AuthorFormatting (Map Author Text)
allExpectedOutputs = M.fromList
  [ (ListCmd       , expectedOutputsListCmd)
  , (FamilyInitials, expectedOutputsFamilyInitials)
  , (InitialsFamily, expectedOutputsInitialsFamily)
  , (BibLaTeX      , expectedOutputsBibLaTeX)
  ]

-- Helper function to make tests from the expected outputs
mkTestCase :: AuthorFormatting -> Author -> TestTree
mkTestCase fmt auth = testCase label (actual @?= expected)
 where
  actual   = formatAuthor fmt auth
  expected = allExpectedOutputs M.! fmt M.! auth
  label    = show fmt ++ "_" ++ T.unpack (_family auth)

tests :: TestTree
tests = testGroup
  "Abbot.Reference.formatAuthor"
  [ mkTestCase fmt auth | fmt <- allFormats, auth <- allAuthors ]
 where
  allFormats = [ListCmd, FamilyInitials, InitialsFamily, BibLaTeX]
  allAuthors = [authJon, authTim, authAli, authJB]
