(** Params and functions that are carried around in the library *)

module Log : sig
  type t = {
    change : string -> unit;
    new_file : string -> unit;
    debug : string -> unit;
  }
end

type t = {
  command : Utils.command;
  conf : string -> Conf.t;
  patch_file : string;
  skip_absent : bool;
  print_tree : bool;
  silence_errors : bool;
  parallel : bool;
  log : Log.t;
}

val of_args : Utils.args -> ((string -> unit) * (int -> unit)) * int -> t
