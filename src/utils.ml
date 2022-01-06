open Ppxlib

type pos = {line : int ; col : int}[@@deriving yojson {exn=true}]

type qualify = {
  start : pos;
  finish : pos [@key "end"];
  content : string
}[@@deriving yojson {exn=true}]

let string_of_position pos =
  Printf.sprintf "%d:%d" pos.pos_lnum pos.pos_cnum

let string_of_location loc = string_of_position loc.loc_start
