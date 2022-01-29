type t
[@@deriving show]

val empty : string -> t

val invalid : string -> t

val is_empty : t -> bool

val delete : chunk:Utils.chunk -> t -> t

val insert : ?newline:bool -> at:Utils.pos -> string -> t -> t

val apply : t -> (unit, string) result
