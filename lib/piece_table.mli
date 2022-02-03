(** Piece table data structure, for efficient text edition *)

type t

val create : string -> t
(** Create an empty piece table with an initial text *)

val delete : ?orig_pos:bool -> pos:int -> t -> unit Utils.res
(** Delete a single character at column [pos].
    If [orig_pos] is set, assume that [pos] is a column number in the original
    text rather than the current one. *)

val insert : ?orig_pos:bool -> pos:int -> string -> t -> unit Utils.res
(** Add a string at column [pos].
    If [orig_pos] is set, assume that [pos] is a column number in the original
    text rather than the current one. *)

val delete_many : pos:int -> length:int -> t -> unit
(** [delete_many pos length t] deletes [length] characters at position [pos] *)

val delete_many_orig : pos:int -> length:int -> t -> unit
(** [delete_many pos length t] deletes [length] **original** characters at
    position [pos], assuming [pos] is a column number in the original text. *)

val contents : t -> string
(** Compute the current edited string *)

val is_original : t -> bool
(** Check if the original string remains unedited *)
