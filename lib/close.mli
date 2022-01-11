type args = {
  report : [`Bar | `Text | `None];
  conf_file : string option;
}

val execute : args -> string list -> (unit, [`Msg of string]) result
(** Analyse opens of a file and prints suggestions for change. *)

(* TODO : have different commands
    lint to recommend modifications (and save this in a temp file)
    fix to apply the listed modifications
*)
