type t
[@@deriving show]

val empty : string -> t

val invalid : string -> t

val is_empty : t -> bool

val delete : chunk:Utils.chunk -> t -> t

val insert : ?newline:bool -> at:Utils.pos -> string -> t -> t

val apply : t -> unit Utils.res

val clean : unit -> (unit, [`Msg of string]) result

val exports : t list -> string -> unit

val imports : string -> t list

val merge : t -> t -> t

val apply_saved : string -> (unit, [`Msg of string]) result
