open Base
open Utils

(* Possible TODO: use typerex-lint ? But it's outdated *)

(* Note : usage of ocp-index in one file
ocp-index print -F Dune__exe__Close
   --context=src/close.ml:4,5
   --build=_build/default/src/.close.eobjs/byte 
   whitelist 

The build dir can be fetched by parsing dune describe --format=csexp
*)

(* TODO: support for wildcard *)

(* let whitelist = ["Base"; "Core"; "Core_kernel"] *)
let whitelist = []

let infer_prefix file qualify =
  let* unqualified = Syntactic.get_source_fragment file qualify.start qualify.finish in
  Option.bind
    (String.chop_suffix qualify.content ~suffix:unqualified)
    ~f:(String.chop_suffix ~suffix:".")
  |> Result.of_option ~error:"Couldn't chop prefix when inferring module name"

type open_summary = {
  module_name : string;
  total : int;
  groups : int;
  layer_only : bool;
  imports_syntax : bool;
}[@@deriving show]

let is_module_id id =
  String.split ~on:'.' id
  |> List.for_all ~f:(fun m -> Char.is_uppercase m.[0])

let is_operator_id id =
  String.split ~on:'.' id
  |> List.last_exn
  |> String.exists ~f:(Fn.non Char.(fun c ->
      is_alphanum c || c = '_'
    ))

let compute_summary file uses =
  if List.is_empty uses then Result.return None
  else
    let* prefix, uses =
      match infer_prefix file (List.hd_exn uses) with
      | Ok prefix ->
        let uses =
          List.map ~f:(fun s -> s.content) uses
          |> List.map ~f:(String.chop_prefix ~prefix:(prefix ^ "."))
        in
        let* uses =
          if List.exists ~f:Option.is_none uses then
            Result.failf "Couldn't chop inferred prefix on use"
          else Result.return (List.filter_opt uses)
        in
        Result.return (prefix, uses)
      | Error _ ->
        let uses = List.map ~f:(fun s -> s.content) uses in
        Result.return ("NOTINFERRED", uses)
    in
    let total = List.length uses in
    let h = Hashtbl.create (module String) in
    List.iter ~f:(Hashtbl.incr h) uses;
    let groups = Hashtbl.length h in
    let layer_only =
      Hashtbl.keys h
      |> List.for_all ~f:is_module_id
    in
    let imports_syntax = 
      Hashtbl.keys h
      |> List.exists ~f:is_operator_id
    in
    Result.return @@
    Some {module_name = prefix; total;
          groups; layer_only; imports_syntax}

let print_summary_maybe = function
  | Some sum -> Stdio.printf "%s\n" (show_open_summary sum)
  | None -> Stdio.printf "One open with no use...\n"

let analyse filename verbose =
  let file = Stdio.In_channel.read_lines filename in
  let* () = Merlin.check_errors filename in
  if verbose then Stdio.printf "Merlin loaded!\n%!";
  let* opens = Syntactic.get_opens filename in
  if verbose then Stdio.printf "Number of opens: %d\n%!" (List.length opens);
  let* uses =
    List.filter ~f:(Fn.non (Syntactic.is_whitelisted whitelist)) opens
    |> List.mapi ~f:(fun i x -> (i, x))
    |> map_result ~f:(fun (i, x) ->
        if verbose then Stdio.printf "Processing %d\n%!" i;
        Merlin.uses_of_open filename x
      ) in
  let* summaries = map_result ~f:(compute_summary file) uses in
  List.iter ~f:print_summary_maybe summaries;
  Result.return ()

let filtered_analyse f b = analyse f b |> filter_errors
