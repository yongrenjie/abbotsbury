module TestWorks where

import           Abbotsbury
import qualified Data.List.NonEmpty            as NE
import           Data.Text                      ( Text )

auth :: (Text, Text) -> Author
auth (gvn, fmy) = Author (Just gvn) fmy

orgLett :: Work
orgLett = ArticleWork $ Article { _articleTitle        = t
                                , _articleAuthors      = a
                                , _articleJournalLong  = jL
                                , _articleJournalShort = jS
                                , _articleYear         = y
                                , _articleVolume       = v
                                , _articleIssue        = i
                                , _articlePages        = p
                                , _articleDoi          = doi
                                }
 where
  t =
    "A General Copper-Catalyzed Synthesis of Ynamides from 1,2-Dichloroenamides"
  a   = NE.fromList [mansfield, smith, yong, garry, anderson]
  jL  = "Organic Letters"
  jS  = "Org. Lett."
  y   = 2019
  v   = "21"
  i   = "8"
  p   = PageRange "2918-2922"
  doi = "10.1021/acs.orglett.9b00971"
  aN  = ""
  mansfield, smith, yong, garry, anderson :: Author
  mansfield = auth ("Steven J.", "Mansfield")
  smith     = auth ("Russell C.", "Smith")
  yong      = auth ("Jonathan R. J.", "Yong")
  garry     = auth ("Olivia L.", "Garry")
  anderson  = auth ("Edward A.", "Anderson")

nrmpCorrected :: Work
nrmpCorrected = ArticleWork $ Article { _articleTitle        = t
                                      , _articleAuthors      = a
                                      , _articleJournalLong  = jL
                                      , _articleJournalShort = jS
                                      , _articleYear         = y
                                      , _articleVolume       = v
                                      , _articleIssue        = i
                                      , _articlePages        = p
                                      , _articleDoi          = doi
                                      }
 where
  t   = "Parallel nuclear magnetic resonance spectroscopy"
  a   = NE.fromList [kupce, frydman, webb, yong, claridge]
  jL  = "Nature Reviews Methods Primers"
  jS  = "Nat. Rev. Methods Primers"
  y   = 2021
  v   = "1"
  i   = "1"
  p   = ArticleNumber "27"
  doi = "10.1038/s43586-021-00024-3"
  kupce, frydman, webb, yong, claridge :: Author
  kupce    = auth ("Ēriks", "Kupče")
  frydman  = auth ("Lucio", "Frydman")
  webb     = auth ("Andrew G.", "Webb")
  yong     = auth ("Jonathan R. J.", "Yong")
  claridge = auth ("Tim D. W.", "Claridge")

noFirstNameN2020 :: Work
noFirstNameN2020 = ArticleWork $ Article { _articleTitle        = t
                                         , _articleAuthors      = a
                                         , _articleJournalLong  = jL
                                         , _articleJournalShort = jS
                                         , _articleYear         = y
                                         , _articleVolume       = v
                                         , _articleIssue        = i
                                         , _articlePages        = p
                                         , _articleDoi          = doi
                                         }
 where
  t   = "Attention science: some people have only one name"
  a   = NE.fromList [sheherazade, ardiantiono]
  jL  = "Nature"
  jS  = "Nature"
  y   = 2020
  v   = ""
  i   = ""
  p   = PageRange ""
  doi = "10.1038/d41586-020-02761-z"
  sheherazade, ardiantiono :: Author
  sheherazade = Author Nothing "Sheherazade"
  ardiantiono = Author Nothing "Ardiantiono"

glaserS1998 :: Work
glaserS1998 = ArticleWork $ Article { _articleTitle        = t
                                    , _articleAuthors      = a
                                    , _articleJournalLong  = jL
                                    , _articleJournalShort = jS
                                    , _articleYear         = y
                                    , _articleVolume       = v
                                    , _articleIssue        = i
                                    , _articlePages        = p
                                    , _articleDoi          = doi
                                    }
 where
  t
    = "Unitary Control in Quantum Ensembles: Maximizing Signal Intensity in Coherent Spectroscopy"
  -- Crossref gives entirely wrong data for this. It has 7 authors.
  a   = NE.fromList [auth ("S. J.", "Glaser")]
  jL  = "Science"
  jS  = "Science" -- not inside the Crossref JSON, so defaults to long name
  y   = 1998
  v   = "280"
  i   = "5362"
  p   = PageRange "421-424"
  doi = "10.1126/science.280.5362.421"
