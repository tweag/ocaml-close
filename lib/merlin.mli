val check_errors : string -> (unit, string) result
(** [check_errors filename] checks if merlin returns errors on that file. *)

val uses_of_open : string -> Ppxlib.module_expr -> (Utils.qualify list, string) result
(** [uses_of_open filename mod] calls merlin to gather every point in filename
    where [mod] is used. *)
