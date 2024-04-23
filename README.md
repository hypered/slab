Playing implementing Pug in Haskell. The idea is to convert Pug to `blaze-html`
expressions. Another possibility would be to generate Haskell code (which use
`blaze-html`).

```
$ scripts/ghci.sh
ghci> :main render examples/a.pug
ghci> :main render --pretty examples/a.pug
```

The test cases in `examples/cases/` come from the [original test
suite](https://github.com/pugjs/pug/tree/master/packages/pug/test/cases).  The
expected HTML is not exactly the same: we pretty print it differently, and some
tags are explicitely closed in Pug.js (e.g. `<source>` vs. `<source/>`.
