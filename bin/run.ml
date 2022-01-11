open Cmdliner

let filenames =
  let doc = "Paths to the OCaml sources to read. Only .ml files." in
  Arg.(non_empty & pos_all file [] & info [] ~docv:"FILES" ~doc)

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
  let open Closelib.Close in
  let pack_args verbose conf_file = {verbose; conf_file} in
  let args = const pack_args $ verbose $ conf_file in
  let applied = const execute $ args $ filenames in
  term_result applied

let () = Term.eval (close_t, info) |> Term.exit
