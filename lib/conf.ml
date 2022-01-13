open Core
open Utils
open Sexplib

type expr =
  | Const of int
  | Uses
  | Symbols
[@@deriving sexp_of]

let is_int s = try ignore @@ Int.of_string s; true with _ -> false

let expr_of_sexp =
  let open Sexp in function
    | Atom x when is_int x -> Const (Int.of_string x)
    | Atom "uses" -> Uses
    | Atom "symbols" -> Symbols
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
  rules : (rule_kind * rule) list;
  precedence : rule_kind list;
}[@@deriving sexp]

let default = {rules = []; precedence = []}

let conf_file_name = ".ocamlclose"

let parse_conf filename =
  try
    let raw = Sexp.load_sexp filename in
    Result.return (conf_of_sexp raw)
  with
  | Sexp.Parse_error {err_msg; _} -> Result.failf "Parse error '%s'" err_msg
  | Failure _ -> Result.failf "File ended too soon"
  | e -> Result.failf "Conversion error '%s'" (Exn.to_string e)

let read_conf ?conf_file () =
  let do_try () =
    let* filename =
      match conf_file with
      | Some x -> Result.return x
      | None ->
        let src = Sys.getcwd () in
        let* found = find_file_s conf_file_name src in
        Result.return found
    in
    parse_conf filename
  in match do_try () with
  | Ok c -> c
  | Error m ->
    Stdio.printf
      "Could not load configuration: %s.\n\
       Falling back to default config: %s.\n"
      m (sexp_of_conf default |> Sexp.to_string_hum);
    default
