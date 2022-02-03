type t

val create : string -> t

val delete : ?orig_pos:bool -> pos:int -> t -> unit Utils.res

val insert : ?orig_pos:bool -> pos:int -> string -> t -> unit Utils.res

val delete_many : pos:int -> length:int -> t -> unit

val delete_many_orig : pos:int -> length:int -> t -> unit

val contents : t -> string

val is_original : t -> bool
