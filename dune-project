(lang dune 2.8)
(name ocamlclose)

(generate_opam_files true)

(license MIT)
(authors "Virgile Robles")
(maintainers "virgile.robles@pm.me")
(source (github Firobe/ocaml-close))

(package
  (name ocamlclose)
  (synopsis "Kill 'em open")
  (description "Kill 'm open")
  (depends
    core
    feather
    ppxlib
    yojson
    sexplib
    ppx_deriving
    ppx_sexp_conv
    ppx_deriving_yojson
    stdio
    cmdliner
    fpath
    progress
  )
)
