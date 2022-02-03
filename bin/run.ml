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

let conf_file =
  let doc = "Force the usage of a configuration file." in
  let env =
    let doc = "Override the configuration to use." in
    Arg.env_var ~doc "OCLOSE_CONF_FILE"
  in
  Arg.(value & opt (some file) None & info ~env ["c"; "conf"] ~doc)

let patch_file =
  let doc =
    "Filename used as either input or output for the patch file."
  in
  Arg.(value & opt (some string) None & info ["p"; "patch"] ~doc)

let skip_absent =
  let doc = "Do not try to build a .cmt file from a .ml file if it does not
  exist, and skip the file instead." in
  Arg.(value & flag & info ~doc ["skip-absent"])

let silence_errors =
  let doc = "Silence any error encountered during the analysis." in
  Arg.(value & flag & info ~doc ["silence-errors"])

let verbose =
  let doc = "Display debug messages (only when reporting mode is 'text')." in
  Arg.(value & flag & info ~doc ["v"; "verbose"])

let info =
  let doc = "analyse a program to detect opens that make it less legible" in
  let man = [
    `S Manpage.s_bugs;
    `P "Email bug reports to virgile.robles@tweag.io or open
        an issue at https://github.com/Firobe/ocaml-close/issues" ]
  in
  Term.info "ocamlclose" ~version:"0.1" ~doc ~exits:Term.default_exits ~man

let pack_args command report conf_file
    skip_absent silence_errors verbose patch_file =
  let open Closelib in
  Utils.{report; conf_file; skip_absent; silence_errors;
         command; verbose; patch_file}

let common_options behavior =
  let open Term in
  let open Closelib in
  let args =
    const pack_args $ (const behavior) $ report $ conf_file $ skip_absent
    $ silence_errors $ verbose $ patch_file
  in
  let applied = const Close.execute $ args $ filenames in
  term_result applied

let lint =
  let doc = "Lint files to suggest a list of modification recommendations."
  in (common_options `Lint, Term.info "lint" ~doc)

let dump =
  let doc =
    "Print a summary of every found open, mainly for debugging purposes."
  in
  (common_options `Dump, Term.info "dump" ~doc)

let patch =
  let doc = "Try to patch files according to the computed recommendations." in
  let filename =
    let doc = "File of the saved patches." in
    Arg.(required & pos 0 (some file) None & info [] ~docv:"PATCH_FILE" ~doc)
  in
  let term = Term.(const Closelib.Patch.apply_saved $ filename)
             |> Term.term_result in
  (term, Term.info "patch" ~doc)

let clean =
  let doc = "Clean all .suggested files." in
  let term = Term.(const Closelib.Patch.clean $ const ())
             |> Term.term_result in
  (term, Term.info "clean" ~doc)

let default_t = fst lint

let () =
  Term.eval_choice (default_t, info) [lint; dump; patch; clean] |> Term.exit
