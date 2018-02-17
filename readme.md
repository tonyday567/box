[etc](https://tonyday567.github.io/etc/index.html) [![Build Status](https://travis-ci.org/tonyday567/etc.svg)](https://travis-ci.org/tonyday567/etc)
===

emit - transduce - commit

compilation recipe
---

```
stack build --test --fast --haddock --file-watch
stack hoogle -- generate --local
stack hoogle -- server --local --port=8080
```
