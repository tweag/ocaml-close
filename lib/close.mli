
val analyse : string -> (unit, string) result
(** Analyse opens of a file and prints suggestions for change. *)

val filtered_analyse : string -> (unit, [`Msg of string]) result
(** Same as {!analyse} but errors have a more standard format. *)
