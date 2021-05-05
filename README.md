## abbotsbury

Minimalistic command-line citation manager. A Haskell port of https://github.com/yongrenjie/cygnet.

It is eventually intended that this will be the main version, after all the current features in `cygnet` are implemented.
To see a list of features that have been / need to be implemented, see https://github.com/yongrenjie/abbotsbury/issues/1.

Basic usage in `ghci`:

```
λ> import Abbotsbury
λ> import qualified Data.Text.IO as TIO
λ> Right orgLett <- fetchWork "your@email.com" "10.1021/acs.orglett.9b00971"
λ> orgLett
Work {_workType = JournalArticle, _title = "A General Copper-Catalyzed Synthesis of Ynamides from 1,2-Dichloroenamides", _authors = Author {_given = Just "Steven J.", _family = "Mansfield"} :| [Author {_given = Just "Russell C.", _family = "Smith"},Author {_given = Just "Jonathan R. J.", _family = "Yong"},Author {_given = Just "Olivia L.", _family = "Garry"},Author {_given = Just "Edward A.", _family = "Anderson"}], _journalLong = "Organic Letters", _journalShort = "Org. Lett.", _year = 2019, _volume = "21", _issue = "8", _pages = "2918-2922", _doi = "10.1021/acs.orglett.9b00971"}
λ> TIO.putStrLn $ cite (Rules acsStyle markdownFormat) orgLett
Mansfield, S. J.; Smith, R. C.; Yong, J. R. J.; Garry, O. L.; Anderson, E. A. A General Copper-Catalyzed Synthesis of Ynamides from 1,2-Dichloroenamides. *Org. Lett.* **2019,** *21* (8), 2918-2922. DOI: [10.1021/acs.orglett.9b00971](https://doi.org/10.1021/acs.orglett.9b00971).
```

The name is taken from the [Abbotsbury Swannery](https://en.wikipedia.org/wiki/Abbotsbury_Swannery) in Dorset, UK, which I had the luck to visit in 2018:

![Swans at Abbotsbury](https://i.imgur.com/vFwSFY7.jpg)
