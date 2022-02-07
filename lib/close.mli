(** Main entry point for the application *)

type outcome = Nothing_to_do | Patches_needed | Failed

val execute : Utils.args -> string list -> (outcome, [`Msg of string]) result
(** Analyse opens of a file and prints suggestions for change. *)
