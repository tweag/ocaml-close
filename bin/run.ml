open Cmdliner

let filenames =
  let doc = "Paths to the OCaml sources to read. Either .ml or .cmt files. If
  .ml files are given, dune will be used to locate their .cmt counterpart and
  build it if it doesn't exist." in
  Arg.(non_empty & pos_all file [] & info [] ~docv:"FILES" ~doc)

let report =
  let parse =
    let open Base in function
    | "bar" -> Result.return `Bar | "text" -> Result.return `Text
    | "none" -> Result.return `None
    | _ -> Result.fail (`Msg "must be one of 'bar', 'text' or 'none'.")
  in
  let print fmt x = let text = match x with
      | `Bar -> "bar" | `Text -> "text" | `None -> "none"
    in Format.pp_print_string fmt text
  in
  let doc = "Report progress during processing.\
             The value $(docv) must be one of `bar', `text' or `none'" in
  Arg.(value & opt (conv (parse, print)) `Bar & info ["r"; "report"] ~doc)

let behavior =
  let parse =
    let open Base in function
      | "suggest" -> Result.return `Suggest
      | "list-only" -> Result.return `List_only
      | _ -> Result.fail (`Msg "must be one of 'suggest' or 'list-only'.")
  in
  let print fmt x = let text = match x with
      | `Suggest -> "suggest" | `List_only -> "list-only"
    in Format.pp_print_string fmt text
  in
  let doc = "Defines behavior of the program. 'suggest' lists modification
  suggestions, while 'list-only' makes the program print a summary of every
  found open, mainly for debugging purposes."
  in
  Arg.(value & opt (conv (parse, print)) `Suggest & info ["b"; "behavior"] ~doc)

let conf_file =
  let doc = "Force the usage of a configuration file." in
  let env =
    let doc = "Override the configuration to use." in
    Arg.env_var ~doc "OCLOSE_CONF_FILE"
  in
  Arg.(value & opt (some file) None & info ~env ["c"; "conf"] ~doc)

let skip_absent =
  let doc = "Do not try to build a .cmt file from a .ml file if it does not
  exist, and skip the file instead." in
  Arg.(value & flag & info ~doc ["skip-absent"])

let silence_errors =
  let doc = "Silence any error encountered during the analysis." in
  Arg.(value & flag & info ~doc ["silence-errors"])

let info =
  let doc = "analyse a program to detect opens that make it less legible" in
  let man = [
    `S Manpage.s_bugs;
    `P "Email bug reports to virgile.robles@tweag.io." ]
  in
  Term.info "ocamlclose" ~version:"0.1" ~doc ~exits:Term.default_exits ~man


let close_t =
  let open Term in
  let open Closelib in
  let pack_args
      report conf_file skip_absent
      silence_errors behavior =
    Utils.{report; conf_file; skip_absent; silence_errors; behavior} in
  let args = const pack_args $ report $ conf_file $ skip_absent
             $ silence_errors $ behavior in
  let applied = const Close.execute $ args $ filenames in
  term_result applied

let () = Term.eval (close_t, info) |> Term.exit
