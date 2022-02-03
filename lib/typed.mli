
(** Retrieval and simple manipulation of the OCaml typed tree. *)
module Extraction : sig

  type t
  (** The type of a typed tree. Left abstract to isolate compiler-libs usage *)

  val get_typed_tree : params:Params.t -> string -> t Utils.res
  (** [get_typed_tree ~report filename] locates the .cmt file corresponding to
      the given .ml by using the `dune describe` command, building the .cmt
      file in the process if it does not exist.
      From that .cmt, it extracts the typed tree of the implementation and
      returns it. *)

  val source_lines : t -> int
  (** Returns the total number of lines in the source file described by [t] *)
end

(** Retrieval and simple manipulation of [open Module] information in the
    typed tree. *)
module Open_info : sig
  type t
  (** The type of an open. *)

  val gather : Extraction.t -> t list
  (** Return the list of global opens in a tree *)

  val get_chunk : t -> Utils.chunk
  (** Get the position of an open in the source file *)

  val get_name : t -> string Utils.res
  (** Try to retrieve the name of an opened module. Will fail if it is not a
      simple identifier (for example a functor application). *)

  val get_short_name : t -> string Utils.res
end

(** For gathering the use-sites of an [open Module]. *)
module Open_uses : sig
  type use_loc
  type use = (string * use_loc)

  val compute : Extraction.t -> Open_info.t
    -> use list Utils.res
  (** [compute tree op] returns a list of use-sites of [op] in [tree]. Each
      use-site is described by its name (the name of the value/type/... used
      from the opened module) and its location in the source file *)

  val optimal_global_position : Extraction.t -> use list -> Utils.pos
  (** [optimal_global_position tree uses] returns the position of the last
      structure item that captures all uses of an open (hence its optimal global
      position *)

  val by_function : Extraction.t -> use list -> (Utils.pos, int) Base.Hashtbl.t option
  (** [by_function tree uses] returns a categorization of the [uses] of an open
      in [tree] by their containing function. *)

  val has_ghost_uses : use list -> bool
  (** Checks if one of the use sites is a ghost location (usually because of a PPX
      pass *)
end

val chunk_of_loc : Open_uses.use_loc -> Utils.chunk
(** [chunk_of_loc loc] returns the equivalent chunk of a compiler loc *)

(** For scope-related utils. *)
module Find : sig
  val scope_lines : Extraction.t -> Open_info.t -> int
  (** [scope_lines tree op] returns the number of line in the source file
      described by [tree] that the scope of [op] covers. *)
end
