module Cite (tests) where

import Abbotsbury
import Test.Tasty
import Test.Tasty.HUnit
import qualified TestWorks as TW

-- | This is the only test for which we will exhaustively check all formats. For other tests we can
-- just check Markdown alone.
testOLCite :: TestTree
testOLCite = testGroup "cite - 2019 OL - ACS style" [text, markdown, restructured, html]
  where
    text :: TestTree
    text = testCase "text format" $ textActual @?= textExpected
    textActual = cite acsStyle textFormat TW.orgLett
    textExpected =
      "Mansfield, S. J.; Smith, R. C.; Yong, J. R. J.; Garry, O. L.; Anderson, E. A. A General Copper-Catalyzed Synthesis of Ynamides from 1,2-Dichloroenamides. Org. Lett. 2019, 21 (8), 2918-2922. DOI: 10.1021/acs.orglett.9b00971."
    markdown :: TestTree
    markdown = testCase "Markdown format" $ markdownActual @?= markdownExpected
    markdownActual = cite acsStyle markdownFormat TW.orgLett
    markdownExpected =
      "Mansfield, S. J.; Smith, R. C.; Yong, J. R. J.; Garry, O. L.; Anderson, E. A. A General Copper-Catalyzed Synthesis of Ynamides from 1,2-Dichloroenamides. *Org. Lett.* **2019,** *21* (8), 2918-2922. DOI: [10.1021/acs.orglett.9b00971](https://doi.org/10.1021/acs.orglett.9b00971)."
    restructured :: TestTree
    restructured = testCase "ReStructuredText format" $ restructuredActual @?= restructuredExpected
    restructuredActual = cite acsStyle restructuredFormat TW.orgLett
    restructuredExpected =
      "Mansfield, S. J.; Smith, R. C.; Yong, J. R. J.; Garry, O. L.; Anderson, E. A. A General Copper-Catalyzed Synthesis of Ynamides from 1,2-Dichloroenamides. *Org. Lett.* **2019,** *21* (8), 2918-2922. DOI: `10.1021/acs.orglett.9b00971 <https://doi.org/10.1021/acs.orglett.9b00971>`_."
    html :: TestTree
    html = testCase "HTML format" $ htmlActual @?= htmlExpected
    htmlActual = cite acsStyle htmlFormat TW.orgLett
    htmlExpected =
      "Mansfield, S. J.; Smith, R. C.; Yong, J. R. J.; Garry, O. L.; Anderson, E. A. A General Copper-Catalyzed Synthesis of Ynamides from 1,2-Dichloroenamides. <i>Org. Lett.</i> <b>2019,</b> <i>21</i> (8), 2918-2922. DOI: <a href=\"https://doi.org/10.1021/acs.orglett.9b00971\">10.1021/acs.orglett.9b00971</a>."

tests :: TestTree
tests = testGroup "Cite" [testOLCite]
