(lang dune 2.9)
(name ocamlclose)

(generate_opam_files true)

(package
  (name ocamlclose)
  (synopsis "Kill 'em open")
  (description "Kill 'm open")
  (depends
    base
    ppxlib
    yojson
    ppx_deriving_yojson
    stdio
  )
)
