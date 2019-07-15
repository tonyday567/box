[box](https://tonyday567.github.io/box/index.html) [![Build Status](https://travis-ci.org/tonyday567/box.svg)](https://travis-ci.org/tonyday567/box)
===

A concurrent, effectful, composable box to put stuff in and take stuff out.

compilation recipe
---

`box-test` takes about 5 minutes to run.

```
stack build --test --exec "$(stack path --local-install-root)/bin/box-test" --file-watch
```
