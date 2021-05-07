## abbotsbury

Minimalistic library for citation management, plus an executable which acts as a command-line citation manager.

`abbotsbury` began as a Haskell port of [`cygnet`](https://github.com/yongrenjie/cygnet), but I intend to make it more general and customisable.
To see a list of features that have been / need to be implemented, see https://github.com/yongrenjie/abbotsbury/issues/1.

Basic usage in `ghci`:

```
λ> import Abbotsbury
λ> import qualified Data.Text.IO as TIO
λ> Right orgLett <- fetchWork "your@email.com" "10.1021/acs.orglett.9b00971"
λ> orgLett
Work {_workType = JournalArticle, _title = "A General Copper-Catalyzed Synthesis of Ynamides from 1,2-Dichloroenamides", _authors = Author {_given = Just "Steven J.", _family = "Mansfield"} :| [Author {_given = Just "Russell C.", _family = "Smith"},Author {_given = Just "Jonathan R. J.", _family = "Yong"},Author {_given = Just "Olivia L.", _family = "Garry"},Author {_given = Just "Edward A.", _family = "Anderson"}], _journalLong = "Organic Letters", _journalShort = "Org. Lett.", _year = 2019, _volume = "21", _issue = "8", _pages = "2918-2922", _doi = "10.1021/acs.orglett.9b00971", _articleNumber = ""}
λ> TIO.putStrLn $ cite acsStyle markdownFormat orgLett
Mansfield, S. J.; Smith, R. C.; Yong, J. R. J.; Garry, O. L.; Anderson, E. A. A General Copper-Catalyzed Synthesis of Ynamides from 1,2-Dichloroenamides. *Org. Lett.* **2019,** *21* (8), 2918-2922. DOI: [10.1021/acs.orglett.9b00971](https://doi.org/10.1021/acs.orglett.9b00971).
```

The name is taken from the [Abbotsbury Swannery](https://en.wikipedia.org/wiki/Abbotsbury_Swannery) in Dorset, UK, which I visited in 2018:

![Swans at Abbotsbury](https://i.imgur.com/vFwSFY7.jpg)
