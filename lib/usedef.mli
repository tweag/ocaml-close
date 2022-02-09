(** Generic data structure to serve as an analysis basis *)

type path = string
type short_path = string

type scope_kind = Module | Value

type node_desc =
  | Open of path
  | Use of path * short_path
  | Def of path
  | Scope of scope_kind * t list

and t = {
  node_loc : Location.t;
  node_desc : node_desc;
}
[@@deriving show]

val make : Analysis.AST.t -> t Utils.res
