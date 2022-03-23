(** Interaction with dune *)

type ext_kind = [`Cmt | `Cmi]

val all_ml_files : unit -> string list Utils.res
(** Return the file names of every implementation file known by dune. *)

val build_obj : Fpath.t -> unit Utils.res
(** Try to build the given file with dune. The file path must be relative to the
    dune root.*)

val find_obj_location : kind:ext_kind -> Fpath.t -> (Fpath.t * Fpath.t) Utils.res
(** Try to find the theoretical location of the built .cmt file from a given
    filename, by calling dune describe. Returns relative and absolute paths *)

val find_or_build_obj : kind:ext_kind -> params:Params.t -> Fpath.t -> string Utils.res
(** Returns the path of either a pre-existing or built cmt file *)
