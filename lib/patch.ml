open Utils

type single =
  | Delete of chunk
  | Insert of {what : string; where : pos; newline : bool}
[@@deriving show]

type t = {actions : single list; filename : string}
[@@deriving show]

let delete ~chunk t =
  {t with actions = (Delete chunk) :: t.actions}

let insert ?(newline=false) ~at what t =
  {t with actions = (Insert {what; where=at; newline}) :: t.actions}

let empty filename = {actions = []; filename}[@@deriving show]

let is_empty t = Base.List.is_empty t.actions

(* TODO: when applying insert, DO NOT apply if string already exist at this
 * place *)
