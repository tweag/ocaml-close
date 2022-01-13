type expr =
  | Const of int
  | Uses
  | Symbols

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

type rule_kind = Keep | Remove | To_local | Move

type conf = {
  rules : (rule_kind * rule) list;
  precedence : rule_kind list;
}

val read_conf : ?conf_file:string -> unit -> conf
