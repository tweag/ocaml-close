module Log : sig
  type t = {
    change : string -> unit;
    new_file : string -> unit;
    debug : string -> unit;
  }
end

type t = {
  conf : Conf.conf;
  skip_absent : bool;
  silence_errors : bool;
  log : Log.t;
}

val of_args : Utils.args -> ((string -> unit) * (int -> unit)) * int -> t
