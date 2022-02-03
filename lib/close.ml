open Base
open Utils

type open_summary = {
  module_name : string;
  short_name : string;
  chunk : chunk;
  ghost_use : bool;
  total : int;
  symbols : string list;
  layer_only : bool;
  imports_syntax : bool;
  scope_lines : int;
  optimal_pos : pos;
  dist_to_optimal : int;
  functions : pos list option;
  use_sites : pos list;
}[@@deriving show]

(* Ugly heuristic for detecting that a name is from a module *)
let is_module_id id = Char.is_uppercase id.[0]

(* Ugly heuristic for detecting infix and prefix operators *)
let is_operator_id id =
  String.split ~on:'.' id
  |> List.last_exn
  |> String.exists ~f:(Fn.non Char.(fun c ->
      is_alphanum c || c = '_'
    ))

(* Fully analyse an open t and its use sites, systematically *)
let compute_summary tree (t, use_sites) =
  let chunk = Typed.Open_info.get_chunk t in
  let scope_lines = Typed.Find.scope_lines tree t in
  let* name = Typed.Open_info.get_name t in
  let* short_name = Typed.Open_info.get_short_name t in
  let total = List.length use_sites in
  let h = Hashtbl.create (module String) in
  List.map ~f:fst use_sites
  |> List.iter ~f:(Hashtbl.incr h);
  let ghost_use = Typed.Open_uses.has_ghost_uses use_sites in
  let optimal_pos = Typed.Open_uses.optimal_global_position tree use_sites in
  let dist_to_optimal =
    let opos = chunk.ch_begin in
    Int.abs (opos.line - optimal_pos.line)
  in
  let functions = Option.map (Typed.Open_uses.by_function tree use_sites) ~f:Hashtbl.keys in
  let use_sites = List.map use_sites ~f:(fun loc ->
      (Typed.chunk_of_loc (snd loc)).ch_begin
    )
  in
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
                 imports_syntax; chunk; short_name; optimal_pos; dist_to_optimal;
                 functions; use_sites; ghost_use}

let print_decision filename sum =
  let open Conf in
  let line = sum.chunk.ch_begin.line in
  (* Color the output *)
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

let patch_of_decision filename sum decision =
  let open Conf in
  let patch = Patch.empty filename in
  match decision with
  | Keep -> patch
  | Move ->
    let to_insert = "open " ^ sum.short_name in
    Patch.insert ~newline:true to_insert ~at:sum.optimal_pos patch
    |> Patch.delete ~chunk:sum.chunk
  | Local -> 
    let patch = Patch.delete ~chunk:sum.chunk patch in
    let locs = Option.value_exn sum.functions in
    let to_insert = Printf.sprintf "let open %s in" sum.short_name in
    List.fold locs ~init:patch ~f:(fun patch pos ->
        Patch.insert ~newline:true to_insert ~at:pos patch
      )
  | Remove ->
    let patch = Patch.delete ~chunk:sum.chunk patch in
    let to_insert = sum.short_name ^ "." in
    List.fold sum.use_sites ~init:patch ~f:(fun patch pos ->
        Patch.insert to_insert ~at:pos patch
      )
  | Structure -> 
    Patch.invalid "transformation into explicit structures\
    cannot yet be automatically applied"
    (* TODO, must know if uses are values, modules, types *)

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

(* Evaluate the configuration language *)
let apply_rule tree rule sum =
  let open Conf in
  (* expressions *)
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
    | Name_length -> String.length sum.short_name
    | Plus (e1, e2) -> eval e1 + eval e2
    | Mult (e1, e2) -> eval e1 * eval e2
    | Minus (e1, e2) -> eval e1 - eval e2
    | Div (e1, e2) -> eval e1 / eval e2
  in
  (* predicates *)
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
    | Ghost_use -> sum.ghost_use
  in apply rule

let make_decision tree conf sum =
  let open Conf in
  let answers =
    List.map ~f:(fun (x, rule) -> (x, apply_rule tree rule sum)) conf.rules
  in
  (* Find the first matching rule, with the given precedence. Keep is nothing
     matches *)
  List.fold (List.rev conf.precedence) ~init:Keep ~f:(fun acc kind ->
      match List.Assoc.find ~equal:Poly.equal answers kind with
      | None | Some false -> acc
      | Some true -> kind
    )

(* Purely decorative *)
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

(* Detect all opens in the tree and analyse them *)
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

type 'a com =
  | Cmd_lint : Patch.t com
  | Cmd_dump : unit com

let analyse (type ty) params (com : ty com) filename : ty list res =
  let open Params in
  let error s = 
    Printf.sprintf "-> %s: %s" filename s
  in
  begin
    params.log.change "Fetching";
    let* tree = Typed.Extraction.get_typed_tree ~params filename in
    let* summaries = get_summaries tree params in
    let conf = params.conf filename in
    let f sum : ty res = match com with
      | Cmd_lint ->
        let decision = make_decision tree conf sum in
        print_decision filename sum decision;
        (* Compute and save patch for decision made *)
        let patch = patch_of_decision filename sum decision in
        Result.return patch
      | Cmd_dump ->
        Stdio.printf "%s\n" (show_open_summary sum);
        Result.return ()
    in
    map_result ~f summaries
  end |> Result.map_error ~f:error

(* Process a single file, returning the computed patche or nothing *)
let one_file (type ty) params (com : ty com) filename : ty res = 
  let open Params in
  params.log.new_file filename;
  match com with
  | Cmd_lint ->
    let* patches = analyse params Cmd_lint filename in
    let patch =
      (* Merge all open patches into a single one *)
      if List.is_empty patches then Patch.empty filename
      else List.reduce_exn patches ~f:Patch.merge
    in
    Result.return patch
  | Cmd_dump ->
    analyse params Cmd_dump filename |> ignore;
    Result.return ()

let execute args filenames =
  let open Params in
  let total = List.length filenames in
  let bar = Progress_bar.b total in
  let go oreport freport =
    (* Prepare arguments *)
    let params = Params.of_args args ((oreport, freport), total) in
    params.log.change "Starting";
    (* What to do with all files, depending on the command *)
    let gather =
      match params.command with
      | `Lint ->
        let patches = List.map filenames ~f:(one_file params Cmd_lint) in
        let* patches =
          if args.silence_errors then
            List.filter_map patches ~f:(function
                | Ok x -> Some x
                | Error _ -> None
              ) |> Result.return
          else
            Result.combine_errors patches
        in
        Patch.exports patches params.patch_file;
        Stdio.printf "Run 'ocamlclose patch %s' to apply modifications\n"
          params.patch_file;
        Result.return ()
      | `Dump ->
        List.map filenames ~f:(one_file params Cmd_dump)
        |> Result.combine_errors |> Result.map ~f:ignore
    in
    (* Process errors, correctly formatting them *)
    let* errors = Result.map_error gather ~f:(fun err_list ->
        let errors = String.concat ~sep:"\n" err_list in
        Printf.sprintf "There were errors during processing:\n%s" errors
      )
    |> Result.map ~f:ignore
    |> filter_errors
    in 
    Result.return errors
  in
  (* Launch the general program with reporting functions *)
  match begin
    if Poly.(args.report = `Bar) then
      Progress.with_reporters ~config:Progress_bar.config bar go
    else go ignore ignore
  end with
  | Error _ when args.silence_errors -> Ok ()
  | s -> s
