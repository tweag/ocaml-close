type args = {
  report : [`Bar | `Text | `None];
  conf_file : string option;
}

val execute : args -> string list -> (unit, [`Msg of string]) result
(** Analyse opens of a file and prints suggestions for change. *)
