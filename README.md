# ocaml-close

A tool to detect uses of `open Module` in an OCaml source that are not
used much, and automatically suggest modifications to make the source more
legible.

**DISCLAIMER: this tool is in active development and currently not mature nor
stable. No guarantee is made about the quality of its results; use it at your
own risk.**

## Installation

Note that this tool will currently only build with OCaml 4.11.2 due to our usage
of `compiler-libs`.

```bash
dune build @install
dune install
```

## CLI usage

Run an analysis by running `ocamlclose ML_FILE_1 ML_FILE_2 ...`
from inside a directory owned by dune.

*For a list of options, see `ocamlclose --help`*

`ocaml-close` will print a list of suggested actions based on the configuration
stored in `.ocamlclose` files. There may be multiple `.ocamlclose` file in the
project tree. Their behavior is described in the next section.

## Configuration

### Grammar

The configuration has the following grammar, where `root` and `precedence`
fields are optional:

```scheme
((root)
 (rules (RULE+))
 (precedence (KIND+)))

RULE := (KIND PRED)
KIND := keep | remove | to_local | move
PRED :=
    | true
    | false
    | (not PRED)
    | (or (PRED+))
    | (and (PRED+))
    | exports-syntax | exports-modules | exports-modules-only
    | in-list (<string>+)
    | (>= EXPR EXPR)
    | (<= EXPR EXPR)
    | (= EXPR EXPR)
EXPR := <number> | symbols | uses | scope-lines | file-lines
    | (+ EXPR EXPR)
    | (- EXPR EXPR)
    | (* EXPR EXPR)
    | (/ EXPR EXPR)
```

### Meaning

The minimal configuration is
```scheme
((rules ()) (precedence ()))
```

If there is no rule or no rule is matched by a file, the default behavior is to
keep the `open`.

An example configuration is
```scheme
((rules
   ((keep
      (or ((in-list ("Base" "Core"))
           exports-syntax
           exports-modules-only)))
    (remove
      (or ((<= symbols 4)
           (<= uses 10))))))
 (precedence (keep remove)))
```

The first rule states that an `open` must be kept if it exports infix operators,
only modules (it is a simple layer) or is either the `Base` or `Core` module
from Jane Street.

The second states that an `open` should be removed if it used in less than 10
places in the file or if the total number of used symbols from that module is
less then 4.

Finally, the configuration states that `keep` takes the priority on `remove`,
meaning that an `open` that matches the `keep` rule will be kept even if it also
matches the `remove` rule.

### Multiple files

`ocamlclose` will try to find `.ocamlclose` files by searching parent
directories of the files under analysis. It gathers all found files, stopping if
a file contains the `root` field. It then merges the rules of the found
configuration files with a disjunction.

Hence, your project should always contain a main `.ocamlclose` file at its root with
the `root` field. This configuration can then be refined by other files in
subdirectories.

For example, given the following directory tree:
```
├── a.ml
├── foo
│   ├── b.ml
│   └── .ocamlclose
└── .ocamlclose
```

If `foo/.ocamlclose` contains `((rules ((keep (in-list ("Bar"))))))` then an
`open Bar` will always be kept for all files in `foo/` and its subdirectories.

## Known limitations

- Uses ocaml-libs, hence for now it only works with OCaml 4.11.2.
- Does not support MLI files for now
- Relies on `dune` to find .cmt files or build them
- The tool currently over-approximates the number of use-sites for opens
