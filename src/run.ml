open Cmdliner

let filename =
  let doc = "Path to the OCaml source to read. Only .ml files." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)

let info =
  let doc = "analyse a program to detect opens that make it less legible" in
  let man = [
    `S Manpage.s_bugs;
    `P "Email bug reports to virgile.robles@tweag.io." ]
  in
  Term.info "ocamlclose" ~version:"0.1" ~doc ~exits:Term.default_exits ~man

let close_t = Term.(const (Close.filtered_analyse) $ filename) |> Term.term_result

let () = Term.eval (close_t, info) |> Term.exit
