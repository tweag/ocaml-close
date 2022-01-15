module Extraction : sig

  type t

  val get_typed_tree : report:([`Text | `Bar | `None]* (string * int -> unit))
    -> string -> (t, string) result
  (** [get_typed_tree ~report filename] locates the .cmt file corresponding to
      the given .ml by using the `dune describe` command, building the .cmt
      file in the process if it does not exist.
      From that .cmt, it extracts the typed tree of the implementation and
      returns it. *)
end

module Open_info : sig
  type t
  val gather : Extraction.t -> t list
  val get_position : t -> Utils.pos
  val get_name : t -> (string, string) result
  val strip_from_name : t -> string -> (string, string) result
end

module Open_uses : sig
  val compute : Extraction.t -> Open_info.t -> (string list, string) result
end
