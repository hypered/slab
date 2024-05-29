A programmable markup language to generate HTML.

This is initially inspired by [Pug.js](https://pugjs.org) but deviates from it.

```
frag page
  h1 A title
  content

page
  p Another paragraph.
```

```
$ scripts/ghci.sh
ghci> :main --help
pughs - parses the Pug syntax

Usage: pughs COMMAND

  pughs tries to implement the Pug syntax.

Available options:
  -h,--help                Show this help text

Available commands:
  build                    Build a library of Pug templates
  render                   Render a Pug template to HTML
  eval                     Parse a Pug template to AST and evaluate it
  parse                    Parse a Pug template to AST
  classes                  Parse a Pug template and report its CSS classes
  mixins                   Parse a Pug template and report its mixins
```

```
ghci> :main parse examples/a.pug
ghci> :main parse --shallow examples/a.pug
ghci> :main eval examples/a.pug
ghci> :main render --pretty examples/a.pug
ghci> :main render examples/a.pug
ghci> :main classes examples/a.pug
```

The test cases in `examples/cases/` come from the [original test
suite](https://github.com/pugjs/pug/tree/master/packages/pug/test/cases). The
expected HTML is not exactly the same: we pretty print it differently, or some
tags are explicitely closed in Pug.js (e.g. `<source>` vs. `<source/>`.
