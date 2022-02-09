(** Misc types and stuff *)

open Core

type 'a res = ('a, string) Result.t

let (let*) = Stdlib.Result.bind
let filter_errors = Result.map_error ~f:(fun m -> `Msg m)
let norm_error = Result.map_error ~f:(fun (`Msg m) -> m)

let find_file ?(containing_folder=false) name src =
  let rec search cur =
    let cur = Fpath.normalize cur in
    if Fpath.is_root cur then
      Result.failf "Could not find a %s file anywhere" name
    else
      let f = Fpath.add_seg cur name in
      match Sys.file_exists (Fpath.to_string f) with
      | `Yes when containing_folder -> Result.return cur 
      | `Yes -> Result.return f
      | _ -> search (Fpath.parent cur)
  in search (Fpath.to_dir_path src)

let find_file_s ?containing_folder name src =
  let* srcf = Fpath.of_string src |> norm_error in
  let* found = find_file ?containing_folder name srcf in
  Result.return (Fpath.to_string found)

let pp_of_show show fmt x = Caml.Format.fprintf fmt "%s" (show x)

type pos = {line : int ; col : int}[@@deriving sexp, ord]
let show_pos p = Printf.sprintf "%d:%d" p.line p.col
let pp_pos = pp_of_show show_pos
type chunk = {ch_begin : pos; ch_end : pos}[@@deriving sexp]
let show_chunk p = Printf.sprintf "%s -> %s"
    (show_pos p.ch_begin) (show_pos p.ch_end)
let pp_chunk = pp_of_show show_chunk

let map_result ~f l = 
  List.map ~f l |> Result.combine_errors |> Result.map_error ~f:(List.hd_exn)

(* Supports wildcard in module name *)
let module_name_equal a b =
  let la = String.split ~on:'.' a in
  let lb = String.split ~on:'.' b in
  let check_prefix la lb =
    List.for_all2_exn la lb ~f:(fun a b ->
        String.(a = "*" || b = "*" || a = b)
      )
  in
  let la_l = List.length la in
  let lb_l = List.length lb in
  if la_l = lb_l then
    check_prefix la lb
  else
    let pat, v = if la_l > lb_l then lb, la else la, lb in
    let pat_l = List.length pat in
    let prefix = List.take v pat_l in
    check_prefix prefix pat &&
    String.(List.last_exn pat = "*")
