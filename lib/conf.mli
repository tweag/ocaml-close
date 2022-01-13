type rule =
  | And of rule list
  | Or of rule list
  | Not of rule
  | True
  | False
  | Min_use of int
  | Min_exported of int
  | In_list of string list
  | Exports_syntax
  | Exports_modules
  | Exports_modules_only

type rule_kind = Keep | Remove | Convert_to_local | Move

type conf = {
  rules : (rule_kind * rule) list;
  precedence : rule_kind list;
}

val read_conf : ?conf_file:string -> unit -> conf
