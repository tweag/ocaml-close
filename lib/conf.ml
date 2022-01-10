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

let find_conf_file src =
  let rec search cur =
    let cur = Fpath.normalize cur in
    if Fpath.is_root cur then
      Result.failf "Could not find a %s file anywhere" conf_file_name
    else
      let f = Fpath.add_seg cur conf_file_name |> Fpath.to_string in
      match Sys.file_exists f with
      | `Yes -> Result.return f
      | _ -> search (Fpath.parent cur)
  in search src

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
        let* src = Sys.getcwd () |> Fpath.of_string
                   |> Result.map_error ~f:(fun (`Msg g) -> g)
        in find_conf_file src
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
