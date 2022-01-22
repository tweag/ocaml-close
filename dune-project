(lang dune 2.8)
(name ocamlclose)

(generate_opam_files true)

(license LGPL-2.1-or-later)
(authors "Virgile Robles")
(maintainers "virgile.robles@pm.me")
(source (github Firobe/ocaml-close))

(package
  (name ocamlclose)
  (synopsis "Kill 'em open")
  (description "Kill 'm open")
  (depends
    (ocaml (= 4.11.2))
    core
    (feather (>= 0.3.0))
    sexplib
    ppx_deriving
    ppx_sexp_conv
    csexp
    stdio
    cmdliner
    fpath
    progress
  )
)
