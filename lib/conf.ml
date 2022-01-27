open Core
open Utils
open Sexplib

type expr =
  | Const of int
  | Plus of expr * expr
  | Minus of expr * expr
  | Mult of expr * expr
  | Div of expr * expr
  | Uses
  | Symbols
  | Scope_lines
  | File_lines
  | Dist_to_optimal
[@@deriving sexp_of]

let is_int s = try ignore @@ Int.of_string s; true with _ -> false

let rec expr_of_sexp =
  let open Sexp in function
    | Atom x when is_int x -> Const (Int.of_string x)
    | Atom "uses" -> Uses
    | Atom "symbols" -> Symbols
    | Atom "scope-lines" -> Scope_lines
    | Atom "file-lines" -> File_lines
    | Atom "dist-to-optimal" -> Dist_to_optimal
    | List [Atom "+"; e1; e2] -> Plus (expr_of_sexp e1, expr_of_sexp e2)
    | List [Atom "-"; e1; e2] -> Minus (expr_of_sexp e1, expr_of_sexp e2)
    | List [Atom "*"; e1; e2] -> Mult (expr_of_sexp e1, expr_of_sexp e2)
    | List [Atom "/"; e1; e2] -> Div (expr_of_sexp e1, expr_of_sexp e2)
    | _ -> failwith "Not an exp"

type rule =
  | And of rule list
  | Or of rule list
  | Not of rule
  | Eq of expr * expr
  | Leq of expr * expr
  | Geq of expr * expr
  | True
  | False
  | In_list of string list
  | Exports_syntax
  | Exports_modules
  | Exports_modules_only
[@@deriving sexp_of]

let rec rule_of_sexp =
  let open Sexp in function
    | Atom "true" -> True
    | Atom "false" -> False
    | List [Atom "and"; r] -> And (List.t_of_sexp rule_of_sexp r)
    | List [Atom "or"; r] -> Or (List.t_of_sexp rule_of_sexp r)
    | List [Atom "not"; r] -> Not (rule_of_sexp r)
    | List [Atom "="; a; b] -> Eq (expr_of_sexp a, expr_of_sexp b)
    | List [Atom "<="; a; b] -> Leq (expr_of_sexp a, expr_of_sexp b)
    | List [Atom ">="; a; b] -> Geq (expr_of_sexp a, expr_of_sexp b)
    | List [Atom "in-list"; l] -> In_list (List.t_of_sexp Sexp.to_string l)
    | Atom "exports-syntax" -> Exports_syntax
    | Atom "exports-modules-only" -> Exports_modules_only
    | Atom "exports-modules" -> Exports_modules
    | s -> Stdio.printf "Unexpected token: %s\n" (Sexp.to_string s); failwith "Not a rule"

type rule_kind = Keep | Remove | To_local | Move
[@@deriving sexp]

type conf = {
  root : bool [@sexp.bool]; 
  rules : (rule_kind * rule) list;
  precedence : rule_kind list [@sexp.list];
}[@@deriving sexp]

let default = {root = true; rules = []; precedence = []}

let conf_file_name = ".ocamlclose"

let parse_conf filename =
  try
    let raw = Sexp.load_sexp filename in
    Result.return (conf_of_sexp raw)
  with
  | Sexp.Parse_error {err_msg; _} -> Result.failf "Parse error '%s'" err_msg
  | Failure _ -> Result.failf "File ended too soon"
  | e -> Result.failf "Conversion error '%s'" (Exn.to_string e)

let rec find_all_conf_files src =
  match find_file_s conf_file_name src with
  | Error _ -> Result.return []
  | Ok f ->
    let* conf = parse_conf f in
    if conf.root then Result.return [conf]
    else
      let* path = Fpath.of_string f |> norm_error in
      let src' = Fpath.parent path |> Fpath.parent |> Fpath.to_string in
      let* acc = find_all_conf_files src' in
      Result.return (acc @ [conf])

let merge_confs l =
  match l with
  | [] -> Result.failf "No configuration file found"
  | h :: t ->
    let merge acc c =
      begin if not @@ List.is_empty c.precedence then
          Stdio.printf "Warning: a non-root configuration file has a precedence field, ignored.\n"
      end;
      let to_merge, to_add = List.partition_tf c.rules
          ~f:(fun (kind, _) ->
              List.Assoc.mem acc.rules ~equal:Poly.equal kind) in
      let merged = List.map acc.rules ~f:(fun ((akind, arule) as r) ->
          match List.Assoc.find to_merge ~equal:Poly.equal akind with
          | None -> r
          | Some brule -> (akind, Or [arule; brule])
        )
      in
      let added = merged @ to_add in
      {acc with rules = added}
    in
    List.fold t ~init:h ~f:merge |> Result.return

let conf_memo = Hashtbl.create (module String)
let read_conf ?conf_file filename =
  let do_try () =
      match conf_file with
      | Some x -> parse_conf x
      | None ->
        let* path = Fpath.of_string filename |> norm_error in
        let* pwd = Sys.getcwd () |> Fpath.of_string |> norm_error in
        let src = Fpath.(append pwd (parent path) |> normalize |> to_string) in
        (* TODO: memoize all upper directories *)
        begin match Hashtbl.find conf_memo src with
          | Some c -> Result.return c
          | None ->
            let* found = find_all_conf_files src in
            let* data = merge_confs found in
            Hashtbl.add_exn conf_memo ~key:src ~data;
            Result.return data
        end
  in match do_try () with
  | Ok c -> c
  | Error m ->
    Stdio.printf
      "Could not load configuration: %s.\n\
       Falling back to default config: %s.\n"
      m (sexp_of_conf default |> Sexp.to_string_hum);
    default
