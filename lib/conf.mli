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
  | Functions
  | Name_length

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

type rule_kind = Keep | Remove | Local | Move | Structure

type conf = {
  root : bool;
  rules : (rule_kind * rule) list;
  precedence : rule_kind list;
}

val read_conf : ?conf_file:string -> string -> conf
