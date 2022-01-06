# ocaml-close

A hacky tool to detect uses of `open Module` in an OCaml source that are not
used much, and automatically suggest modifications to make the source more
legible.

## Installation

```bash
dune build @install
dune install
```

## Usage

```bash
ocamlclose ML_FILE
```

## Status

It is extremely experimental and hacky, relying on Merlin and some internal
Ppxlib tools.

It does not support MLI files for now.
