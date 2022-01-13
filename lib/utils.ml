open Ppxlib
open Core

type pos = {line : int ; col : int}[@@deriving yojson]

type qualify = {
  start : pos;
  finish : pos [@key "end"];
  content : string
}[@@deriving yojson]

type keep_rule =
  | And of keep_rule list
  | Or of keep_rule list
  | Not of keep_rule
  | True
  | False
  | Min_use of int
  | Min_exported of int
  | Whitelisted
  | Exports_syntax
  | Exports_modules
  | Exports_modules_only
[@@deriving sexp]

let (let*) = Stdlib.Result.bind
let filter_errors = Result.map_error ~f:(fun m -> `Msg m)
let norm_error = Result.map_error ~f:(fun (`Msg m) -> m)

let find_file ?(containing_folder=false) name src =
  let* srcf = Fpath.of_string src |> norm_error in
  let rec search cur =
    let cur = Fpath.normalize cur in
    if Fpath.is_root cur then
      Result.failf "Could not find a %s file anywhere" name
    else
      let f = Fpath.add_seg cur name |> Fpath.to_string in
      match Sys.file_exists f with
      | `Yes when containing_folder -> Result.return (Fpath.to_string cur)
      | `Yes -> Result.return f
      | _ -> search (Fpath.parent cur)
  in search (Fpath.to_dir_path srcf)

let pos_of_position posi =
  {line = posi.pos_lnum; col = posi.pos_cnum - posi.pos_bol}

let string_of_pos pos = Printf.sprintf "%d:%d" pos.line pos.col

let map_result ~f l = 
  List.map ~f l |> Result.combine_errors |> Result.map_error ~f:(List.hd_exn)
