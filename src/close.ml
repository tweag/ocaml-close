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
let whitelist = ["Base"]

let infer_prefix filename qualify =
  let unqualified = Syntactic.get_source_fragment filename qualify.start qualify.finish in
  String.chop_suffix_exn qualify.content ~suffix:unqualified
  |> String.chop_suffix_exn ~suffix:"."

type open_summary = {
  module_name : string;
  use_table : (string, int) Base.Hashtbl.t;
}

let compute_summary filename uses =
  if List.is_empty uses then None
  else (
    let prefix = infer_prefix filename (List.hd_exn uses) in
    let uses =
      List.map ~f:(fun s -> s.content) uses
      |> List.map ~f:(String.chop_prefix_exn ~prefix:(prefix ^ "."))
    in
    let h = Hashtbl.create (module String) in
    List.iter ~f:(Hashtbl.incr h) uses;
    Some {module_name = prefix ; use_table = h}
  )

let print_summary sum =
  Stdio.printf "Module %s : " sum.module_name ;
  let s = Hashtbl.sexp_of_t String.sexp_of_t Int.sexp_of_t sum.use_table in
  Stdio.printf "%s\n" (Sexp.to_string_hum s)

let print_summary_maybe = function
  | Some sum -> print_summary sum
  | None -> Stdio.printf "One open with no use..."

let main =
  let filename = (Sys.get_argv ()).(1) in
  let ast = Syntactic.get_ast_ml filename in
  let opens = Syntactic.opens_of ast in
  Stdio.printf "Number of opens: %d\n" (List.length opens);
  Merlin.check_errors filename ;
  List.filter ~f:(Fn.non (Syntactic.is_whitelisted whitelist)) opens
  |> List.map ~f:(Merlin.uses_of_open filename)
  |> List.map ~f:(compute_summary filename)
  |> List.iter ~f:print_summary_maybe
