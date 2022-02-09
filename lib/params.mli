(** Params and functions that are carried around in the library *)

module Log : sig
  type t = {
    change : string -> unit;
    new_file : string -> unit;
    debug : string -> unit;
  }
end

type command = [`Lint | `Dump]

type common_args = {
  command : command;
  print_tree : bool;
  skip_absent : bool;
  silence_errors : bool;
  parallel : bool;
}

type cli = {
  report : [`Bar | `Text | `None];
  conf_file : string option;
  verbose : bool;
  patch_file : string option;
  common : common_args;
}

type t = {
  conf : string -> Conf.t;
  log : Log.t;
  patch_file : string;
  common : common_args;
}

val of_cli : cli -> ((string -> unit) * (int -> unit)) * int -> t
