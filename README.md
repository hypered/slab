# Slab

Slab is a programmable markup language to generate HTML.

```
$ cat data/values.json
[
  {
    "username": "Alice",
    "email": "alice@example.com"
  },
  {
    "username": "Bob",
    "email": "bob@example.com"
  }
]

$ cat example.slab
let values = data/values.json
ul
  for value in values
    li Hello, #{value['username']}.

$ slab render --pretty example.slab
<ul>
    <li>
        Hello, Alice.
    </li>
    <li>
        Hello, Bob.
    </li>
</ul>
```

# Devlopment

```
$ scripts/ghci.sh
ghci> :main --help
slab - A programmable markup language to generate HTML

Usage: slab COMMAND

  Slab is a programmable markup language to generate HTML.

Available options:
  -h,--help                Show this help text

Available commands:
  build                    Build a library of Slab templates to HTML
  watch                    Watch and build a library of Slab templates to HTML
  serve                    Watch and serve a library of Slab templates to HTML
  render                   Render a Slab template to HTML
  eval                     Parse a Slab template to AST and evaluate it
  parse                    Parse a Slab template to AST
  classes                  Parse a Slab template and report its CSS classes
  fragments                Parse a Slab template and report its fragments
```

```
ghci> :main parse examples/a.slab
ghci> :main parse --shallow examples/a.slab
ghci> :main eval examples/a.slab
ghci> :main render --pretty examples/a.slab
ghci> :main render examples/a.slab
ghci> :main classes examples/a.slab
```

The test cases in `examples/cases/` come from the [original Pug test
suite](https://github.com/pugjs/pug/tree/master/packages/pug/test/cases). The
expected HTML is not exactly the same: we pretty print it differently, or some
tags are explicitely closed in Pug.js (e.g. `<source>` vs. `<source/>`.

# Code generation

In addition of generating static HTML files from Slab templates, we explore
generating code.

```
ghci> :main generate a.slab
```

Currently, a very small subset of the Slab language is supported, and only
Haskell code can be generated.

# Why the name

In the context of [memory
management](https://en.wikipedia.org/wiki/Slab_allocation), slab allocation is
a mechanism designed to reduce fragmentation caused by allocations and
deallocations.

Slab the language is designed to define, combine and, eventually evaluate
fragments to HTML.

# Acknowledgement

Slab was initially a port to Haskell of the [Pug.js](https://pugjs.org)
templating language.
