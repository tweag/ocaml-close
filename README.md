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

## Usage

```bash
ocamlclose ML_FILE
```

## Status

It is currently experimental and hacky, relying on Merlin and some internalPpxlib tools.

It does not support MLI files for now.
