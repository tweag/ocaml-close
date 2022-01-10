open Cmdliner

let filename =
  let doc = "Path to the OCaml source to read. Only .ml files." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)

let verbose =
  let doc = "Report progress during processing." in
  Arg.(value & flag & info ["v"; "verbose"] ~doc)

let conf_file =
  let doc = "Force the usage of a configuration file." in
  let env =
    let doc = "Override the configuration to use." in
    Arg.env_var ~doc "OCLOSE_CONF_FILE"
  in
  Arg.(value & opt (some file) None & info ~env ["c"; "conf"] ~doc)

let info =
  let doc = "analyse a program to detect opens that make it less legible" in
  let man = [
    `S Manpage.s_bugs;
    `P "Email bug reports to virgile.robles@tweag.io." ]
  in
  Term.info "ocamlclose" ~version:"0.1" ~doc ~exits:Term.default_exits ~man

let close_t =
  let open Term in
  const (Closelib.Close.filtered_analyse) $ filename $ verbose $ conf_file
  |> term_result

let () = Term.eval (close_t, info) |> Term.exit
