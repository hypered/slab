Playing implementing Pug in Haskell. The idea is to convert Pug to `blaze-html`
expressions. Another possibility would be to generate Haskell code (which use
`blaze-html`).

```
$ scripts/ghci.sh
ghci> :main render examples/a.pug
ghci> :main render --pretty examples/a.pug
```
