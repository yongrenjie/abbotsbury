import           Abbot.Reference

import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Test.Tasty
import           Test.Tasty.HUnit
import           Data.Text                      ( Text )
import qualified Data.Text                     as T


-- Define all possible inputs
authJon, authTim, authAli, authJB :: Author
authJon = Author (Just "Jonathan Ren Jie") "Yong"
authTim = Author (Just "Tim D. W.") "Claridge"
authAli = Author (Just "Mohammadali") "Foroozandeh"
authJB = Author (Just "Jean-Baptiste") "Verstraete"   -- useful for hyphens. Thanks JB

-- Define all possible (input, output) pairs
expectedOutputsListCmd :: [(Author, Text)]
expectedOutputsListCmd =
  [ (authJon, "JRJ Yong"),
    (authTim, "TDW Claridge"),
    (authAli, "M Foroozandeh"),
    (authJB, "JB Verstraete")
  ]

expectedOutputsFamilyInitials :: [(Author, Text)]
expectedOutputsFamilyInitials =
  [ (authJon, "Yong, J. R. J."),
    (authTim, "Claridge, T. D. W."),
    (authAli, "Foroozandeh, M."),
    (authJB, "Verstraete, J.-B.")
  ]

expectedOutputsInitialsFamily :: [(Author, Text)]
expectedOutputsInitialsFamily =
  [ (authJon, "J. R. J. Yong"),
    (authTim, "T. D. W. Claridge"),
    (authAli, "M. Foroozandeh"),
    (authJB, "J.-B. Verstraete")
  ]

expectedOutputsBibLaTeX :: [(Author, Text)]
expectedOutputsBibLaTeX =
  [ (authJon, "Yong, Jonathan Ren Jie"),
    (authTim, "Claridge, Tim D.\\ W."),
    (authAli, "Foroozandeh, Mohammadali"),
    (authJB, "Verstraete, Jean-Baptiste")
  ]

-- Helper function to make tests from the (input, output) pairs
mkTestCase :: AuthorFormatting -> (Author, Text) -> TestTree
mkTestCase fmt (auth, expected)
  = testCase (T.unpack (_family auth)) (formatAuthor fmt auth @?= expected)


tests :: TestTree
tests = testGroup "Abbot.Reference.formatAuthor"
  [ testGroup "ListCmd" $ map (mkTestCase ListCmd) expectedOutputsListCmd
  , testGroup "FamilyInitials" $ map (mkTestCase FamilyInitials) expectedOutputsFamilyInitials
  , testGroup "InitialsFamily" $ map (mkTestCase InitialsFamily) expectedOutputsInitialsFamily
  , testGroup "BibLaTeX" $ map (mkTestCase BibLaTeX) expectedOutputsBibLaTeX
  ]

main :: IO ()
main = defaultMain tests
