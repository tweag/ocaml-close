
val analyse : string -> bool -> (unit, string) result
(** Analyse opens of a file and prints suggestions for change. *)

val filtered_analyse : string -> bool -> (unit, [`Msg of string]) result
(** Same as {!analyse} but errors have a more standard format. *)
