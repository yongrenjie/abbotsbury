module Cite
  ( tests
  ) where

import           Abbotsbury
import qualified Data.Text                     as T
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified TestWorks                     as TW

-- | This is the only test for which we will exhaustively check all formats. For
-- other tests we can just check Markdown alone.
testOLCite :: TestTree
testOLCite = testGroup "cite - 2019 OL - all styles"
                       [acsText, acsMarkdown, acsRestructured, acsHtml, bib]
 where
  orgLett :: Work
  orgLett = TW.testWorks IM.! 1
  acsText :: TestTree
  acsText    = testCase "ACS/Text" $ textActual @?= textExpected
  textActual = cite acsStyle textFormat (TW.testWorks IM.! 1)
  textExpected
    = "Mansfield, S. J.; Smith, R. C.; Yong, J. R. J.; Garry, O. L.; Anderson, E. A. A General Copper-Catalyzed Synthesis of Ynamides from 1,2-Dichloroenamides. Org. Lett. 2019, 21 (8), 2918-2922. DOI: 10.1021/acs.orglett.9b00971."
  acsMarkdown :: TestTree
  acsMarkdown =
    testCase "ACS/Markdown" $ markdownActual @?= markdownExpected
  markdownActual = cite acsStyle markdownFormat orgLett
  markdownExpected
    = "Mansfield, S. J.; Smith, R. C.; Yong, J. R. J.; Garry, O. L.; Anderson, E. A. A General Copper-Catalyzed Synthesis of Ynamides from 1,2-Dichloroenamides. *Org. Lett.* **2019,** *21* (8), 2918-2922. DOI: [10.1021/acs.orglett.9b00971](https://doi.org/10.1021/acs.orglett.9b00971)."
  acsRestructured :: TestTree
  acsRestructured =
    testCase "ACS/reStructuredText"
      $   restructuredActual
      @?= restructuredExpected
  restructuredActual = cite acsStyle restructuredFormat orgLett
  restructuredExpected
    = "Mansfield, S. J.; Smith, R. C.; Yong, J. R. J.; Garry, O. L.; Anderson, E. A. A General Copper-Catalyzed Synthesis of Ynamides from 1,2-Dichloroenamides. *Org. Lett.* **2019,** *21* (8), 2918-2922. DOI: `10.1021/acs.orglett.9b00971 <https://doi.org/10.1021/acs.orglett.9b00971>`_."
  acsHtml :: TestTree
  acsHtml    = testCase "ACS/HTML" $ htmlActual @?= htmlExpected
  htmlActual = cite acsStyle htmlFormat orgLett
  htmlExpected
    = "Mansfield, S. J.; Smith, R. C.; Yong, J. R. J.; Garry, O. L.; Anderson, E. A. A General Copper-Catalyzed Synthesis of Ynamides from 1,2-Dichloroenamides. <i>Org. Lett.</i> <b>2019,</b> <i>21</i> (8), 2918-2922. DOI: <a href=\"https://doi.org/10.1021/acs.orglett.9b00971\">10.1021/acs.orglett.9b00971</a>."
  bib :: TestTree
  bib = testCase "BibLaTeX" $ bibActual @?= bibExpected
  bibActual = cite bibStyle textFormat orgLett
  bibExpected
    = T.intercalate "\n" [ "@article{Mansfield2019OL,"
                         , "    doi = {10.1021/acs.orglett.9b00971},"
                         , "    author = {Mansfield, Steven J. and Smith, Russell C. and Yong, Jonathan R.\\ J. and Garry, Olivia L. and Anderson, Edward A.},"
                         , "    title = {A General Copper-Catalyzed Synthesis of Ynamides from 1,2-Dichloroenamides},"
                         , "    journaltitle = {Org.\\ Lett.},"
                         , "    year = {2019},"
                         , "    volume = {21},"
                         , "    number = {8},"
                         , "    pages = {2918--2922},"
                         , "}" ]

tests :: TestTree
tests = testGroup "Cite" [testOLCite]
