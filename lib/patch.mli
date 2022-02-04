(** Aggregation of insertions and deletions in a file, saveable and loadable *)

type t

val empty : string -> t
(** [empty f] is the empty patch for filename [f] *)

val invalid : string -> t
(** [invalid r] is an invalid patch for reason [r] *)

val is_empty : t -> bool
(** Check if a patch contains no edits *)

val delete : chunk:Utils.chunk -> t -> t
(** Add to a patch the action of deleting a chunk of text. The chunk must be on
    a single line. *)

val insert : ?newline:bool -> at:Utils.pos -> string -> t -> t
(** [insert ~at s p] adds to a patch [p] the action of inserting [s] at position
    [pos]. If [newline] is [true] (defaults to [false]), a new line is inserted
    after the string and the original line is correctly indented. *)

val merge : t -> t -> t
(** Merge two patches. The two patches must apply to the same file and be both
    valid. *)

val apply : ?inplace:bool -> ?check:bool -> t -> unit Utils.res
(** Apply a patch by creating an edited copy of the original file next to it,
    suffixed with .close.ml *)

val clean : unit -> (unit, [`Msg of string]) result
(** Recursively remove all .close.ml files in the directory *)

val exports : t list -> string -> unit
(** Export a list of patches as a file *)

val imports : string -> t list
(** Import previously exported patches *)

val apply_saved : ?inplace:bool -> ?check:bool
  -> string -> (unit, [`Msg of string]) result
(** Import and apply patches saved in a file *)
