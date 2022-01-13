open Core
open Utils
open Sexplib

type rule =
  | And of rule list
  | Or of rule list
  | Not of rule
  | True
  | False
  | Min_use of int
  | Min_exported of int
  | In_list of string list
  | Exports_syntax
  | Exports_modules
  | Exports_modules_only
[@@deriving sexp]

type rule_kind = Keep | Remove | Convert_to_local | Move
[@@deriving sexp]

type conf = {
  rules : (rule_kind * rule) list;
  precedence : rule_kind list;
}[@@deriving sexp]

let default = {rules = []; precedence = []}

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
        let* found = find_file_s conf_file_name src in
        Result.return found
    in
    parse_conf filename
  in match do_try () with
  | Ok c -> c
  | Error m ->
    Stdio.printf
      "Could not load configuration: %s.\n\
       Falling back to default config: %s.\n"
      m (sexp_of_conf default |> Sexp.to_string_hum);
    default
