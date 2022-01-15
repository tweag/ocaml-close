# ocaml-close

A tool to detect uses of `open Module` in an OCaml source that are not
used much, and automatically suggest modifications to make the source more
legible.

Will also perform several other analyses.

## Installation

```bash
dune build @install
dune install
```

## CLI usage

Run an analysis by running `ocamlclose ML_FILE_1 ML_FILE_2 ...`
from inside a directory owned by dune.

*For a list of options, see `ocamlclose --help`*

`ocaml-close` will print a list of suggested actions based on the configuration
stored in a `.ocamlclose` file which must be at the root of the project.

## Configuration

### Grammar

The configuration has the following grammar:
```scheme
((rules (RULE+))
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
EXPR := <number> | symbols | uses
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

## Status

### Known limitations

- Uses ocaml-libs, hence for now it only works with OCaml 4.11.2.
- Does not support MLI files for now
- Relies on `dune` to find .cmt files or build them
- The tool currently over-approximates the number of use-sites for opens
