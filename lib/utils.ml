open Ppxlib
open Base

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

let pos_of_position posi =
  {line = posi.pos_lnum; col = posi.pos_cnum - posi.pos_bol}

let string_of_pos pos = Printf.sprintf "%d:%d" pos.line pos.col

let (let*) = Stdlib.Result.bind

let filter_errors = Result.map_error ~f:(fun m -> `Msg m)
let map_result ~f l = 
  List.map ~f l |> Result.combine_errors |> Result.map_error ~f:(List.hd_exn)
