(** Entry-point for open-analysis *)

type t = {
  module_name : string;
  short_name : string;
  chunk : Utils.chunk;
  ghost_use : bool;
  total : int;
  symbols : (string * Typed.Open_uses.use_kind) list;
  scope_lines : int;
  optimal_pos : Utils.pos;
  functions : Utils.pos list option;
  use_sites : Utils.pos list;
}[@@deriving show]

val compute_all : Typed.Extraction.t -> Conf.t -> Params.t -> t list Utils.res
(** [compute_all tree conf params] gather all non-standard opens and computes an
    analysis summary for each of them *)
