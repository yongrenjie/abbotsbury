module TestWorks where


import           Abbotsbury.Work
import qualified Data.List.NonEmpty            as NE
import           Data.Text                      ( Text )
-- import qualified Data.Text                     as T



auth :: (Text, Text) -> Author
auth (gvn, fmy) = Author (Just gvn) fmy


orgLett :: Work
orgLett = Work wt t a jL jS y v i p d aN
 where
  wt = JournalArticle
  t =
    "A General Copper-Catalyzed Synthesis of Ynamides from 1,2-Dichloroenamides"
  a  = NE.fromList [mansfield, smith, yong, garry, anderson]
  jL = "Organic Letters"
  jS = "Org. Lett."
  y  = 2019
  v  = "21"
  i  = "8"
  p  = "2918-2922"
  d  = "10.1021/acs.orglett.9b00971"
  aN = ""
  mansfield, smith, yong, garry, anderson :: Author
  mansfield = auth ("Steven J.", "Mansfield")
  smith     = auth ("Russell C.", "Smith")
  yong      = auth ("Jonathan R. J.", "Yong")
  garry     = auth ("Olivia L.", "Garry")
  anderson  = auth ("Edward A.", "Anderson")


nrmpCorrected :: Work
nrmpCorrected = Work wt t a jL jS y v i p d aN
 where
  wt = JournalArticle
  t  = "Parallel nuclear magnetic resonance spectroscopy"
  a  = NE.fromList [kupce, frydman, webb, yong, claridge]
  jL = "Nature Reviews Methods Primers"
  jS = "Nat. Rev. Methods Primers"
  y  = 2021
  v  = "1"
  i  = "1"
  p  = ""
  d  = "10.1038/s43586-021-00024-3"
  aN = "27"
  kupce, frydman, webb, yong, claridge :: Author
  kupce    = auth ("Ēriks", "Kupče")
  frydman  = auth ("Lucio", "Frydman")
  webb     = auth ("Andrew G.", "Webb")
  yong     = auth ("Jonathan R. J.", "Yong")
  claridge = auth ("Tim D. W.", "Claridge")


noFirstNameN2020 :: Work
noFirstNameN2020 = Work wt t a jL jS y v i p d aN
 where
  wt = JournalArticle
  t  = "Attention science: some people have only one name"
  a  = NE.fromList [sheherazade, ardiantiono]
  jL = "Nature"
  jS = "Nature"
  y  = 2020
  v  = ""
  i  = ""
  p  = ""
  d  = "10.1038/d41586-020-02761-z"
  aN = ""
  sheherazade, ardiantiono :: Author
  sheherazade = Author Nothing "Sheherazade"
  ardiantiono = Author Nothing "Ardiantiono"


glaserS1998 :: Work
glaserS1998 = Work wt t a jL jS y v i p d aN
 where
  wt = JournalArticle
  t
    = "Unitary Control in Quantum Ensembles: Maximizing Signal Intensity in Coherent Spectroscopy"
  -- Crossref gives entirely wrong data for this. It has 7 authors.
  a = NE.fromList [auth ("S. J.", "Glaser")]
  jL = "Science"
  jS = "Science"   -- not inside the Crossref JSON, so defaults to long name
  y  = 1998
  v  = "280"
  i  = "5362"
  p  = "421-424"
  d  = "10.1126/science.280.5362.421"
  aN = ""
