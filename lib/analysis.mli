(** Typed AST analysis facilities, tied to OCaml 4.11.2 *)

(** Retrieval and simple manipulation of the OCaml typed tree. *)
module AST : sig

  type t
  (** The type of a typed tree. Left abstract to isolate compiler-libs usage *)

  val get : params:Params.t -> string -> t Utils.res
  (** [get_typed_tree ~report filename] locates the .cmt file corresponding to
      the given .ml by using the `dune describe` command, building the .cmt
      file in the process if it does not exist.
      From that .cmt, it extracts the typed tree of the implementation and
      returns it. *)

  val source_lines : t -> int
  (** Returns the total number of lines in the source file described by [t] *)

  val print : t -> unit
  (** Pretty-print the typed AST *)
end

(** Retrieval and simple manipulation of [open Module] information in the
    typed tree. *)
module Info : sig
  type t
  (** The type of an open. *)

  val gather : AST.t -> t list
  (** Return the list of global opens in a tree *)

  val get_chunk : t -> Utils.chunk
  (** Get the position of an open in the source file *)

  val get_name : t -> string Utils.res
  (** Try to retrieve the name of an opened module. Will fail if it is not a
      simple identifier (for example a functor application). *)

  val get_short_name : t -> string Utils.res
end

(** For gathering the use-sites of an [open Module]. *)
module Uses : sig
  type loc
  type kind =
    | Uk_Module
    | Uk_Module_Type
    | Uk_Value
    | Uk_Type of int (* arity of type *)
  [@@deriving show]
  type t = {name : string; loc : loc; kind : kind}

  val compute : AST.t -> Info.t -> t list Utils.res
  (** [compute tree op] returns a list of use-sites of [op] in [tree]. Each
      use-site is described by its name (the name of the value/type/... used
      from the opened module) and its location in the source file *)

  val optimal_global_position : AST.t -> t list -> Utils.pos
  (** [optimal_global_position tree uses] returns the position of the last
      structure item that captures all uses of an open (hence its optimal global
      position *)

  val optimal_scoped_position : AST.t -> t list -> Utils.pos
  (** Same as {!optimal_global_position}, except it will only return positions
      that are at the beginning of a scope : it will not place an open in the
      middle of a module. *)

  val by_function : AST.t -> t list -> (Utils.pos, int) Base.Hashtbl.t option
  (** [by_function tree uses] returns a categorization of the [uses] of an open
      in [tree] by their containing function. *)

  val has_ghost_uses : t list -> bool
  (** Checks if one of the use sites is a ghost location (usually because of a PPX
      pass *)
end

val chunk_of_loc : Uses.loc -> Utils.chunk
(** [chunk_of_loc loc] returns the equivalent chunk of a compiler loc *)

(** For scope-related utils. *)
module Find : sig
  val scope_lines : AST.t -> Info.t -> int
  (** [scope_lines tree op] returns the number of line in the source file
      described by [tree] that the scope of [op] covers. *)
end
