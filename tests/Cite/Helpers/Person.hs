{-# LANGUAGE QuasiQuotes #-}

module Cite.Helpers.Person
  ( tests
  ) where

import           Abbotsbury
import           Abbotsbury.Cite
import           Abbotsbury.Cite.Helpers.Person
import           Abbotsbury.Work
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
jon, tim, ali, jb, eriks :: Person
jon = mkPerson "Jonathan Ren Jie" "Yong"
tim = mkPerson "Tim D. W." "Claridge"
ali = mkPerson "Mohammadali" "Foroozandeh"
jb = mkPerson "Jean-Baptiste" "Verstraete" -- useful for hyphens. Thanks JB
eriks = mkPerson "Ēriks" "Kupče" -- test Unicode

allPersons :: [Person]
allPersons = [jon, tim, ali, jb, eriks]

allFormats :: [PersonStyle]
allFormats = [minBound .. maxBound]

expectedOutputsFamilyInitials :: Map Person Text
expectedOutputsFamilyInitials = M.fromList
  [ (jon  , "Yong, J. R. J.")
  , (tim  , "Claridge, T. D. W.")
  , (ali  , "Foroozandeh, M.")
  , (jb   , "Verstraete, J.-B.")
  , (eriks, "Kupče, Ē.")
  ]

expectedOutputsInitialsFamily :: Map Person Text
expectedOutputsInitialsFamily = M.fromList
  [ (jon  , "J. R. J. Yong")
  , (tim  , "T. D. W. Claridge")
  , (ali  , "M. Foroozandeh")
  , (jb   , "J.-B. Verstraete")
  , (eriks, "Ē. Kupče")
  ]

expectedOutputsBibLaTeX :: Map Person Text
expectedOutputsBibLaTeX = M.fromList
  [ (jon  , "Yong, Jonathan Ren Jie")
  , (tim  , "Claridge, Tim D.\\ W.")
  , (ali  , "Foroozandeh, Mohammadali")
  , (jb   , "Verstraete, Jean-Baptiste")
  , (eriks, [r|Kup{\v{c}}e, {\=E}riks|])
  ]

allExpectedOutputs :: Map PersonStyle (Map Person Text)
allExpectedOutputs = M.fromList
  [ (FamilyInitials, expectedOutputsFamilyInitials)
  , (InitialsFamily, expectedOutputsInitialsFamily)
  , (BibLaTeX      , expectedOutputsBibLaTeX)
  ]

-- Helper function to make tests from the expected outputs
mkTestCase :: PersonStyle -> Person -> TestTree
mkTestCase fmt auth = testCase label (actual @?= expected)
 where
  actual   = formatPerson fmt auth
  expected = allExpectedOutputs M.! fmt M.! auth
  label    = show fmt ++ "_" ++ T.unpack (_family auth)

tests :: TestTree
tests = testGroup
  "Person"
  [ mkTestCase fmt auth | fmt <- allFormats, auth <- allPersons ]
