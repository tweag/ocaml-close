open Utils

type conf = {
  keep_rule : keep_rule;
}

val read_conf : ?conf_file:string -> unit -> conf
