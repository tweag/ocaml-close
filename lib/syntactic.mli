val get_source_fragment : string list -> Utils.pos -> Utils.pos -> (string, string) result
(** [get_source_fragment filename start finish] returns the string in the file
    delimited by the two positions. *)
