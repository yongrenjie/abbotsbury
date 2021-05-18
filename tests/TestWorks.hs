module TestWorks where

import           Abbotsbury
import qualified Data.List.NonEmpty            as NE
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.Text                      ( Text )

testWorks :: IntMap Work
testWorks = IM.fromList $ zip [1..] [ testWork1
                                    , testWork2
                                    , testWork3
                                    , testWork4
                                    , testWork5
                                    , testWork6
                                    ]

testWork1 :: Work
testWork1 = ArticleWork $ Article { _articleTitle        = t
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
  mansfield, smith, yong, garry, anderson :: Person
  mansfield = mkPerson "Steven J." "Mansfield"
  smith     = mkPerson "Russell C." "Smith"
  yong      = mkPerson "Jonathan R. J." "Yong"
  garry     = mkPerson "Olivia L." "Garry"
  anderson  = mkPerson "Edward A." "Anderson"

testWork2 :: Work
testWork2 = ArticleWork $ Article { _articleTitle        = t
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
  kupce, frydman, webb, yong, claridge :: Person
  kupce    = mkPerson "Ēriks" "Kupče"
  frydman  = mkPerson "Lucio" "Frydman"
  webb     = mkPerson "Andrew G." "Webb"
  yong     = mkPerson "Jonathan R. J." "Yong"
  claridge = mkPerson "Tim D. W." "Claridge"

testWork3 :: Work
testWork3 = ArticleWork $ Article { _articleTitle        = t
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
  sheherazade, ardiantiono :: Person
  sheherazade = Person Nothing "Sheherazade" Nothing
  ardiantiono = Person Nothing "Ardiantiono" Nothing

testWork4 :: Work
testWork4 = ArticleWork $ Article { _articleTitle        = t
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
  a   = NE.fromList [mkPerson "S. J." "Glaser"]
  jL  = "Science"
  jS  = "Science" -- not inside the Crossref JSON, so defaults to long name
  y   = 1998
  v   = "280"
  i   = "5362"
  p   = PageRange "421-424"
  doi = "10.1126/science.280.5362.421"

testWork5 :: Work
testWork5 = ArticleWork $ Article { _articleTitle        = t
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
    "Twenty Years of Organic Letters, A Look Back...and Forward"
  a   = NE.fromList [Person (Just "Amos B.") "Smith" (Just "III")]
  jL  = "Organic Letters"
  jS  = "Org. Lett."
  y   = 2018
  v   = "20"
  i   = "1"
  p   = PageRange "1-3"
  doi = "10.1021/acs.orglett.7b03845"

testWork6 :: Work
testWork6 = BookWork $ Book { _bookTitle        = t 
                            , _bookPublisher    = pub
                            , _bookPublisherLoc = loc
                            , _bookAuthors      = auths
                            , _bookEditors      = []
                            , _bookYear         = year
                            , _bookEdition      = ""
                            , _bookIsbn         = isbn
                            , _bookSeries       = ser
                            , _bookNumber       = ""
                            }
  where
    t = "Applications of Quantum Dynamics in Chemistry"
    pub = "Springer International Publishing"
    loc = "Cham"   -- In Switzerland, but Crossref doesn't tell us anything more.
    auths = [ mkPerson "Fabien" "Gatti"
            , mkPerson "Benjamin" "Lasorne"
            , mkPerson "Hans-Dieter" "Meyer"
            , mkPerson "André" "Nauts"
            ]
    year = 2017
    isbn = "9783319539218"
    ser = "Lecture Notes in Chemistry"
