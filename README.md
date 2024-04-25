Implementing the [Pug.js](https://pugjs.org) templating language in Haskell.

```
$ scripts/ghci.sh
ghci> :main --help
pughs - parses the Pug syntax

Usage: pughs COMMAND
  pughs tries to implement the Pug syntax.

Available options:
  -h,--help                Show this help text

Available commands:
  render                   Render a Pug template to HTML
  parse                    Parse a Pug template to AST
  classes                  Parse a Pug template and report its CSS classes
```

```
ghci> :main render examples/a.pug
ghci> :main render --pretty examples/a.pug
ghci> :main classes examples/a.pug
```

The test cases in `examples/cases/` come from the [original test
suite](https://github.com/pugjs/pug/tree/master/packages/pug/test/cases). The
expected HTML is not exactly the same: we pretty print it differently, or some
tags are explicitely closed in Pug.js (e.g. `<source>` vs. `<source/>`.
