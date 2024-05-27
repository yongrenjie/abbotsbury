# abbotsbury

Minimalistic library for citation management, plus an executable which acts as a command-line citation manager.

I really enjoyed writing and using this, but I don't maintain it anymore.
You're welcome to look at the code, but I'm no longer in academia, so I don't actually get to use it.


### Executable usage

The interactive reference manager is named `abbot` and can be installed via

```
git clone https://github.com/yongrenjie/abbotsbury
cd abbotsbury
cabal install exe:abbot
```

Apart from the reference manager itself, `abbot` also provides an `abbot cite` subcommand which generates citations for a set of DOIs passed as arguments.
Type `abbot cite -h` for more information.


### Naming

The name is taken from the [Abbotsbury Swannery](https://en.wikipedia.org/wiki/Abbotsbury_Swannery) in Dorset, UK, which I visited in 2018:

<img src="https://i.imgur.com/vFwSFY7.jpg" width="400" alt="Swans at Abbotsbury Swannery">
