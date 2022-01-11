open Cmdliner

let filenames =
  let doc = "Paths to the OCaml sources to read. Only .ml files." in
  Arg.(non_empty & pos_all file [] & info [] ~docv:"FILES" ~doc)

let report =
  let parse =
    let open Base in function
    | "bar" -> Result.return `Bar
    | "text" -> Result.return `Text
    | "none" -> Result.return `None
    | _ -> Result.fail (`Msg "must be one of 'bar', 'text' or 'none'.")
  in
  let print fmt x = let text = match x with
      | `Bar -> "bar"
      | `Text -> "text"
      | `None -> "none"
    in Format.pp_print_string fmt text
  in
  let doc = "Report progress during processing.\
             The value $(docv) must be one of `bar', `text' or `none'" in
  Arg.(value & opt (conv (parse, print)) `Bar & info ["r"; "report"] ~doc)

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
  let pack_args report conf_file = {report; conf_file} in
  let args = const pack_args $ report $ conf_file in
  let applied = const execute $ args $ filenames in
  term_result applied

let () = Term.eval (close_t, info) |> Term.exit
