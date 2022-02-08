open Base
open Utils

type open_summary = {
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

(* Ugly heuristic for detecting infix and prefix operators *)
let is_operator_id id =
  String.split ~on:'.' id
  |> List.last_exn
  |> String.exists ~f:(Fn.non Char.(fun c ->
      is_alphanum c || c = '_'
    ))

let is_submodule_id id = String.exists id ~f:(fun c -> Char.(c = '.'))

(* Fully analyse an open t and its use sites, systematically *)
let compute_summary tree conf (t, uses) =
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
        let names = List.map ~f:fst sum.symbols in
        let symbols = "[" ^ (String.concat ~sep:", " names) ^ "]" in
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
    (* Remove duplicate use sites *)
    let dedup_sites = List.dedup_and_sort sum.use_sites ~compare:compare_pos in
    List.fold dedup_sites ~init:patch ~f:(fun patch pos ->
        Patch.insert to_insert ~at:pos patch
      )
  | Structure -> 
    let patch = Patch.delete ~chunk:sum.chunk patch in
    let open Caml.Format in
    let pp_value fmt (name, kind) =
      let open Typed.Open_uses in
      match kind with
      | Uk_Type 0 ->
        (* type t = t is cyclic, even with an open *)
        fprintf fmt "type %s = [%%import: %s.%s]" name sum.short_name name
      | Uk_Type n ->
        (* For arity > 0, write "type ('t1, ...) t = ('t1, ...) M.t" *)
        let arity_args =
          List.range 1 (n + 1)
          |> List.map ~f:(fun x -> Printf.sprintf "'t%d" x)
        in
        let arity_str = asprintf "(%a)"
            (pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt ", ")
               pp_print_string) arity_args
        in
        fprintf fmt "type %s %s = [%%import: %s %s.%s]"
          arity_str name arity_str sum.short_name name
      | _ ->
        let kind = match kind with
          | Uk_Module -> "module"
          | Uk_Value -> "let"
          | Uk_Module_Type -> "module type"
          | Uk_Type _ -> assert false
        in fprintf fmt "%s %s = %s" kind name name
    in
    (* here, just leverage the open *)
    let to_insert = asprintf
        "@[<v 2>open struct@,open %s@,%a@]@,end" sum.short_name
        (pp_print_list ~pp_sep:pp_print_cut pp_value) sum.symbols
    in
    Patch.insert ~newline:true to_insert ~at:sum.chunk.ch_begin patch

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
    | Dist_to_optimal ->
      let opos = sum.chunk.ch_begin in
      Int.abs (opos.line - sum.optimal_pos.line)
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
    | Exports_syntax ->
      List.exists sum.symbols ~f:(fun (name, _) -> is_operator_id name)
    | Exports_subvalues_only ->
      List.for_all sum.symbols ~f:(fun (name, _) -> is_submodule_id name)
    | Exports_subvalues ->
      List.exists sum.symbols ~f:(fun (name, _) ->
          String.exists name ~f:(fun c -> Char.(c = '.'))
        )
    | Exports_types ->
      List.exists sum.symbols ~f:(function
          | _, Uk_Type _ -> true
          | _ -> false
        )
    | Ghost_use -> sum.ghost_use
    | Optimal_is_before ->
      (* TODO check eligibility of move before, rather than this hack
       * This can be done by checking that the optimal pos actually have the
       * short_name in its environment (or even computing the optimal pos this
       * way to begin with *)
      let opos = sum.chunk.ch_begin in
      sum.optimal_pos.line - opos.line < 0
  in apply rule

let make_decision tree conf sum =
  let open Conf in
  let answers =
    List.map ~f:(fun (x, rule) -> (x, apply_rule tree rule sum))
      conf.rules
  in
  (* Find the first matching rule, with the given precedence. Keep if nothing
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
      bar ~style:`UTF8 ~width:(`Fixed 60)
        ~color:Terminal.Color.(hex "#FA0") total;
      count_to total
    ])
  let bar2 = Line.(list [ 
      spinner ();
      string
    ])
  let b total = Multi.(line bar2 ++ line (bar1 ~total))
end

(* Detect all opens in the tree and analyse them *)
let get_summaries tree conf params =
  let open Params in
  let opens = Typed.Open_info.gather tree in
  params.log.change "Analyzing";
  List.map opens ~f:(fun x ->
      let* use_sites = Typed.Open_uses.compute tree x in
      compute_summary tree conf (x, use_sites)
    )
  (* Filter out failing opens *)
  |> List.filter_map ~f:(function Ok o -> Some o | _ -> None)
  |> Result.return

(* GADT version of the command type, to allow functions to return different
 * objects depending on the command *)
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
    if params.print_tree then Typed.Extraction.print tree;
    let conf = params.conf filename in
    let* summaries = get_summaries tree conf params in
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

type outcome = Nothing_to_do | Patches_needed | Failed

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
                | Ok x when Patch.is_empty x -> None
                | Ok x -> Some x
                | Error _ -> None
              ) |> Result.return
          else
            Result.combine_errors patches
        in
        if List.is_empty patches then (
          Stdio.printf "No modification suggested. All good!\n";
          Result.return Nothing_to_do
        )
        else (
          Patch.exports patches params.patch_file;
          let prog_name = (Sys.get_argv ()).(0) in
          Stdio.printf
            "Modification are needed. Run '%s patch %s' to apply them.\n"
            prog_name params.patch_file;
          Result.return Patches_needed
        )
      | `Dump ->
        let* () = List.map filenames ~f:(one_file params Cmd_dump)
                  |> Result.combine_errors |> Result.map ~f:ignore
        in Result.return Nothing_to_do
    in
    (* Process errors, correctly formatting them *)
    let* with_combined_errors = Result.map_error gather ~f:(fun err_list ->
        let errors = String.concat ~sep:"\n" err_list in
        Printf.sprintf "There were errors during processing:\n%s" errors
      )
    |> filter_errors
    in 
    Result.return with_combined_errors
  in
  (* Launch the general program with reporting functions *)
  match begin
    if Poly.(args.report = `Bar) then
      Progress.with_reporters ~config:Progress_bar.config bar go
    else go ignore ignore
  end with
  | Error _ when args.silence_errors -> Result.return Failed
  | s -> s
