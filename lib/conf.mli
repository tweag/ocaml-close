open Utils

type conf = {
  whitelist : string list;
  keep_rule : keep_rule;
}

val read_conf : ?conf_file:string -> unit -> conf
