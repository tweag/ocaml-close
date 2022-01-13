open Core
open Utils
open Sexplib

type conf = {
  whitelist : string list;
  keep_rule : keep_rule;
}[@@deriving sexp]

let default_rule = Whitelisted
let default = {whitelist = []; keep_rule = default_rule}

let conf_file_name = ".ocamlclose"

let parse_conf filename =
  try
    let raw = Sexp.load_sexp filename in
    Result.return (conf_of_sexp raw)
  with
  | Sexp.Parse_error {err_msg; _} -> Result.failf "Parse error '%s'" err_msg
  | Failure _ -> Result.failf "File ended too soon"
  | e -> Result.failf "Conversion error '%s'" (Exn.to_string e)

let read_conf ?conf_file () =
  let do_try () =
    let* filename =
      match conf_file with
      | Some x -> Result.return x
      | None ->
        let src = Sys.getcwd () in
        find_file conf_file_name src
    in
    parse_conf filename
  in match do_try () with
  | Ok c -> c
  | Error m ->
    Stdio.printf
      "Could not load configuration: %s.\n\
       Falling back to default config.\n"
      m;
    default
