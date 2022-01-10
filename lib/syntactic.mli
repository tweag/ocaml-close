val get_opens : string -> (Ppxlib.module_expr list, string) result
(** [get_opens filename] returns a list of module expressions that are open in
    that file. *)

val get_source_fragment : string list -> Utils.pos -> Utils.pos -> (string, string) result
(** [get_source_fragment filename start finish] returns the string in the file
    delimited by the two positions. *)

val is_whitelisted : string list -> Ppxlib.module_expr -> bool
(** [is_whitelisted whitelist module] checks if the resolved name of [module] is
    in the whitelist. {b Not really sound.} *)
