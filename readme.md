[box](https://tonyday567.github.io/box/index.html) [![Build Status](https://travis-ci.org/tonyday567/box.svg)](https://travis-ci.org/tonyday567/box)
===

A concurrent, effectful, composable box

compilation recipe
---

```
stack build --test --fast --haddock --file-watch
stack hoogle -- generate --local
stack hoogle -- server --local --port=8080
```
