(** Decision making based on analysis and configuration *)

type t

val compute : Typed.Extraction.t -> Conf.t -> Summary.t -> t
(** [compute tree conf sum] returns the decision taken by applying rule matching
    of the rules in the configuration to the analysis summary of an open *)

val to_patch : string -> Summary.t -> t -> Patch.t
(** [to_patch filename sum decision] transforms the decision into a patch that
    can be used to automatically fix the given file *)

val print : string -> Summary.t -> t -> unit
(** [print filename sum decision] prints on standard output a textual
    explanation of how the file should be modified to respect the decision *)
