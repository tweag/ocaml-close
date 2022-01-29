open Utils

type single =
  | Delete of chunk
  | Insert of {what : string; where : pos; newline : bool}
[@@deriving show]

type t =
  | Valid of {actions : single list; filename : string}
  | Invalid of string
[@@deriving show]

let delete ~chunk = function
  | Invalid s -> Invalid s
  | Valid t ->
    if chunk.ch_begin.line = chunk.ch_end.line then
      Valid {t with actions = (Delete chunk) :: t.actions}
    else Invalid "cannot delete chunk of multiple lines"

let insert ?(newline=false) ~at what = function
  | Invalid s -> Invalid s
  | Valid t ->
    Valid {t with actions = (Insert {what; where=at; newline}) :: t.actions}

let empty filename = Valid {actions = []; filename}

let invalid why = Invalid why

let is_empty = function Valid t -> Base.List.is_empty t.actions | _ -> true

(* TODO: when applying insert, DO NOT apply if string already exist at this
 * place *)
