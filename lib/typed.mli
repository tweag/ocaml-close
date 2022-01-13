module Extraction : sig

  val get_typed_tree : report:([`Text | `Bar | `None]* (string * int -> unit))
    -> string -> (Typedtree.structure, string) result
  (** [get_typed_tree ~report filename] locates the .cmt file corresponding to
      the given .ml by using the `dune describe` command, building the .cmt
      file in the process if it does not exist.
      From that .cmt, it extracts the typed tree of the implementation and
      returns it. *)

end
