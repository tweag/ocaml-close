open Base
open Utils

type open_summary = {
  module_name : string;
  short_name : string;
  pos : pos;
  total : int;
  symbols : string list;
  layer_only : bool;
  imports_syntax : bool;
  scope_lines : int;
  optimal_pos : pos;
  dist_to_optimal : int;
  functions : pos list option;
}[@@deriving show]

let is_module_id id = Char.is_uppercase id.[0]

let is_operator_id id =
  String.split ~on:'.' id
  |> List.last_exn
  |> String.exists ~f:(Fn.non Char.(fun c ->
      is_alphanum c || c = '_'
    ))

let compute_summary tree (t, use_sites) =
  let pos = Typed.Open_info.get_position t in
  let scope_lines = Typed.Find.scope_lines tree t in
  let* name = Typed.Open_info.get_name t in
  let* short_name = Typed.Open_info.get_short_name t in
  let total = List.length use_sites in
  let h = Hashtbl.create (module String) in
  List.map ~f:fst use_sites
  |> List.iter ~f:(Hashtbl.incr h);
  let optimal_pos = Typed.Open_uses.optimal_global_position tree use_sites in
  let dist_to_optimal =
    let oloc = Typed.Open_info.get_position t in
    Int.abs (oloc.line - optimal_pos.line)
  in
  let functions = Option.map (Typed.Open_uses.by_function tree use_sites) ~f:Hashtbl.keys in
  let symbols = Hashtbl.keys h in
  let layer_only =
    Hashtbl.keys h
    |> List.for_all ~f:is_module_id
  in
  let imports_syntax = 
    Hashtbl.keys h
    |> List.exists ~f:is_operator_id
  in
  Result.return {module_name = name; total; scope_lines; symbols; layer_only;
                 imports_syntax; pos; short_name; optimal_pos; dist_to_optimal;
                 functions}

(* TODO:
 * command to automatically perform the modification
 * potentially look into :
 *  - https://hannesm.github.io/patch/patch/Patch/index.html
 *  - https://ocaml.janestreet.com/ocaml-core/latest/doc/patience_diff/Patience_diff_lib__Patience_diff/Hunk/index.html#type-t
 *  
 *  to build the diff and perform a patch
 *)
let enact_decision filename sum =
  let open Conf in
  let line = sum.pos.line in
  let mod_name = Printf.sprintf "\027[91m%s\027[0m" sum.short_name in
  let print_file () = Stdio.printf "\027[90m%s:\027[0m " filename in
  Progress.interject_with (fun () -> function
      | Keep -> ()
      | Remove ->
        print_file ();
        Stdio.printf "remove open %s (line %d)\n" mod_name line
      | Move ->
        print_file ();
        Stdio.printf "move open %s from line %d to line %d\n"
          mod_name line sum.optimal_pos.line
      | Structure ->
        let symbols = "[" ^ (String.concat ~sep:", " sum.symbols) ^ "]" in
        print_file ();
        Stdio.printf "explicitly open values %s from %s at line %d\n"
          symbols mod_name line
      | Local ->
        print_file ();
        let lines =
          Option.value_exn sum.functions
          |> List.map ~f:(fun f -> f.line)
          |> List.sort ~compare
          |> List.map ~f:Int.to_string_hum
          |> String.concat ~sep:","
        in
        let lines = "[" ^ lines ^ "]" in
        Stdio.printf
          "transform open %s (line %d) to local opens at lines %s\n"
          mod_name line lines
    )

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

(* TODO: add --explain flag to explain why a rule was applied *)

let apply_rule tree rule sum =
  let open Conf in
  let rec eval = function
    | Const n -> n
    | Uses -> sum.total
    | Symbols -> List.length sum.symbols
    | File_lines -> Typed.Extraction.source_lines tree
    | Scope_lines -> sum.scope_lines
    | Dist_to_optimal -> sum.dist_to_optimal
    | Functions ->
      begin match sum.functions with
        | None -> Int.max_value
        | Some l -> List.length l
      end
    | Name_length -> String.length sum.module_name
    | Plus (e1, e2) -> eval e1 + eval e2
    | Mult (e1, e2) -> eval e1 * eval e2
    | Minus (e1, e2) -> eval e1 - eval e2
    | Div (e1, e2) -> eval e1 / eval e2
  in
  let rec apply = function
    | And l -> List.map ~f:apply l |> List.for_all ~f:Fn.id
    | Or l -> List.map ~f:apply l |> List.exists ~f:Fn.id
    | Not b -> not (apply b)
    | Eq (e1, e2) -> (eval e1) = (eval e2)
    | Leq (e1, e2) -> (eval e1) <= (eval e2)
    | Geq (e1, e2) -> (eval e1) >= (eval e2)
    | True -> true
    | False -> false
    | In_list l -> List.mem l sum.module_name ~equal:module_name_equal
    | Exports_syntax -> sum.imports_syntax
    | Exports_modules -> failwith "Exports_modules not yet implemented"
    | Exports_modules_only -> sum.layer_only
  in apply rule

let make_decision filename tree conf sum =
  let open Conf in
  let answers =
    List.map ~f:(fun (x, rule) -> (x, apply_rule tree rule sum)) conf.rules
  in
  let decision =
    List.fold (List.rev conf.precedence) ~init:Keep ~f:(fun acc kind ->
        match List.Assoc.find ~equal:Poly.equal answers kind with
        | None | Some false -> acc
        | Some true -> kind
      ) in
  enact_decision filename sum decision

module Progress_bar = struct
  open Progress
  let config = Config.(v ~persistent:false ())
  let bar1 ~total = Line.(list [ 
      const "Files";
      bar ~style:`UTF8 ~width:(`Fixed 60) ~color:Terminal.Color.(hex "#FA0") total;
      count_to total
    ])
  let bar2 = Line.(list [ 
      spinner ();
      string
    ])
  let b total = Multi.(line bar2 ++ line (bar1 ~total))
end

let get_summaries tree params =
  let open Params in
  let opens = Typed.Open_info.gather tree in
  params.log.change "Analyzing";
  List.map opens ~f:(fun x ->
      let* use_sites = Typed.Open_uses.compute tree x in
      compute_summary tree (x, use_sites)
    )
  (* Filter out failing opens *)
  |> List.filter_map ~f:(function Ok o -> Some o | _ -> None)
  |> Result.return

let analyse params filename =
  let open Params in
  begin
    params.log.change "Fetching";
    let* tree = Typed.Extraction.get_typed_tree ~params filename in
    let* summaries = get_summaries tree params in
    let conf = params.conf filename in
    let f = match params.behavior with
      | `Suggest -> make_decision filename tree conf
      | `List_only -> fun x -> Stdio.printf "%s\n" (show_open_summary x)
    in
    List.iter summaries ~f;
    Result.return ()
  end |> Result.map_error ~f:(fun s -> Printf.sprintf "-> %s: %s" filename s)

let execute args filenames =
  let open Params in
  let total = List.length filenames in
  let bar = Progress_bar.b total in
  let go oreport freport =
    let params = Params.of_args args ((oreport, freport), total) in
    params.log.change "Starting";
    List.map filenames ~f:(fun filename ->
        params.log.new_file filename;
        analyse params filename
      )
    |> Result.combine_errors
    |> Result.map_error ~f:(fun err_list ->
        let errors = String.concat ~sep:"\n" err_list in
        Printf.sprintf "There were errors during processing:\n%s" errors
      )
    |> Result.map ~f:ignore
    |> filter_errors
  in
  match begin
    if Poly.(args.report = `Bar) then
      Progress.with_reporters ~config:Progress_bar.config bar go
    else go ignore ignore
  end with
  | Error _ when args.silence_errors -> Ok ()
  | s -> s
