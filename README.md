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
(root)?
(placement PL_MODE)?
(precedence (KIND*))?
(rules RULE*)

PL_MODE := scope | pos
RULE := (KIND PRED)
KIND := keep | remove | local | move | structure
PRED :=
    | true
    | false
    | (not PRED)
    | (or (PRED+))
    | (and (PRED+))
    | exports-syntax | exports-modules | exports-modules-only
    | exports-subvalues | exports-types | ghost-use
    | in-list (<string>+)
    | (>= EXPR EXPR)
    | (<= EXPR EXPR)
    | (= EXPR EXPR)
EXPR := <number> | symbols | uses | dist-to-optimal
    | scope-lines | file-lines | functions | name-length
    | (+ EXPR EXPR)
    | (- EXPR EXPR)
    | (* EXPR EXPR)
    | (/ EXPR EXPR)
```

### Meaning

The minimal configuration is
```scheme
(rules)
```

If there is no rule or no rule is matched by a file, the default behavior is to
keep the `open`.

Below is the configuration file used for this repo, with comments explaining the
purpose of each line.

```scheme
(root) ; ocaml-close will not look for .ocamlclose files in parent directories

; Determines what is the considered the optimal placement of a global open.
; Either:
;   - 'pos':   the position before the first actual use of the open is optimal
;   - 'scope': the beginning of the smallest enclosing module of all the uses is
;              optimal
(placement pos)

; The order in which rules are matched
; (e.g., we keep opens matched by the 'keep' rule no matter the other rules)
(precedence (keep remove local structure move))

; An 'open <X>' statement is...
(rules

  ; - left untouched if...
  (keep
    ; ...either it is whitelisted, ...
    (or (in-list ("Base" "Core" "Core_kernel"))
        ; ...it is used for infix operators, ...
        exports-syntax
        ; ...it is only for its exposed submodules, ...
        exports-modules-only
        ; ...or its scope is roughly a screen.
        (<= scope-lines 40)))

  ; - removed, and its uses re-qualified, if...
  ;   ... it is not used much and X is not too long and can be qualified easily.
  (remove (and (<= uses 5) (<= name-length 15) (not ghost-use)))

  ; - replaced by an explicit structured open, if...
  (structure
    ;   ... it exports few different identifiers, and...
    (and (<= symbols 5)
         ; ... it is used enough times, and...
         (>= uses 15)
         ; ... it only exports direct symbols, not from submodules, and...
         (not exports-subvalues)
         ; ... it does not exports types (avoid using ppx_import).
         (not exports-types)))

  ; - removed and replaced by local 'let open <X> in's if...
  ;   ... it is used only by only a few functions.
  (local (<= functions 4))

  ; - moved closer to its optimal position (see 'placement' parameter), if...
  ;   ... it is too far from that optimal placement.
  (move (>= dist-to-optimal 40)))

; vim: filetype=scheme
```

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

If `foo/.ocamlclose` contains `(rules (keep (in-list ("Bar"))))` then an
`open Bar` will always be kept for all files in `foo/` and its subdirectories.

## Known limitations

- Uses ocaml-libs, hence for now it only works with OCaml 4.11.2.
- Does not support MLI files for now
- Relies on `dune` to find .cmt files or build them
- The tool currently over-approximates the number of use-sites for opens
