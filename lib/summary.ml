open Utils
open Base

type t = {
  module_name : string;
  short_name : string;
  chunk : chunk;
  ghost_use : bool;
  total : int;
  symbols : (string * Typed.Open_uses.use_kind) list;
  scope_lines : int;
  optimal_pos : pos;
  functions : pos list option;
  use_sites : pos list;
}[@@deriving show]

(* Fully analyse an open t and its use sites, systematically *)
let compute tree conf (t, uses) =
  let open Typed in
  let chunk = Open_info.get_chunk t in
  let scope_lines = Find.scope_lines tree t in
  let* name = Open_info.get_name t in
  let* short_name = Open_info.get_short_name t in
  let total = List.length uses in
  let h = Hashtbl.Poly.create () in
  List.iter uses ~f:(fun x -> Hashtbl.Poly.incr h Open_uses.(x.name, x.kind));
  let ghost_use = Open_uses.has_ghost_uses uses in
  let optimal_pos =
    match conf.Conf.placement with
    | Pos -> Open_uses.optimal_global_position tree uses
    | Scope -> Open_uses.optimal_scoped_position tree uses
  in
  let functions =
    Option.map (Open_uses.by_function tree uses) ~f:Hashtbl.keys in
  let use_sites = List.map uses ~f:(fun {loc; _} ->
      (chunk_of_loc loc).ch_begin
    )
  in
  let symbols = Hashtbl.keys h in
  Result.return
    {module_name = name; total; scope_lines; symbols;
     chunk; short_name; optimal_pos; functions; use_sites; ghost_use}

let is_standard_open conf t =
  let* name = Typed.Open_info.get_name t in
  Result.return (List.mem conf.Conf.standard name ~equal:module_name_equal)

(* Detect all non-standard opens in the tree and analyse them *)
let compute_all tree conf params =
  let open Params in
  let opens = Typed.Open_info.gather tree in
  let non_standard = List.filter opens ~f:(fun o ->
      match is_standard_open conf o with
      | Ok b -> not b
      | Error _ -> true
    )
  in
  params.log.change "Analyzing";
  List.map non_standard ~f:(fun x ->
      let* use_sites = Typed.Open_uses.compute tree x in
      compute tree conf (x, use_sites)
    )
  (* Filter out failing opens *)
  |> List.filter_map ~f:(function Ok o -> Some o | _ -> None)
  |> Result.return
