open Base
open Utils

type t = Conf.rule_kind

(* Ugly heuristic for detecting infix and prefix operators *)
let is_operator_id id =
  String.split ~on:'.' id
  |> List.last_exn
  |> String.exists ~f:(Fn.non Char.(fun c ->
      is_alphanum c || c = '_'
    ))

let is_submodule_id id = String.exists id ~f:(fun c -> Char.(c = '.'))

let print filename sum =
  let open Conf in
  let open Summary in
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

let to_patch filename sum decision =
  let open Conf in
  let open Summary in
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

(* TODO: add --explain flag to explain why a rule was applied *)

(* Evaluate the configuration language *)
let apply_rule tree rule sum =
  let open Conf in
  let open Summary in
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

let compute tree conf sum =
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
