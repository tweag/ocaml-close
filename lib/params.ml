module Log = struct
  type t = {
    change : string -> unit;
    new_file : string -> unit;
    debug : string -> unit;
  }

  let change kind txt = match kind with
    | `Text -> Stdio.printf "%s...\n%!" txt
    | `None -> ()
    | `Bar (f, _) -> f txt

  let new_file total kind =
    let n = ref 0 in
    fun name -> incr n;
      match kind with
      | `Text -> Stdio.printf "Processing %s (%d/%d)\n%!" name !n total
      | `None -> ()
      | `Bar (_, f) -> f 1

  let debug verbose kind txt = match kind with
    | `Text when verbose -> Stdio.printf "%s\n%!" txt
    | _ -> ()

  let make kind (fs, total) verbose =
    let kind = match kind with
      | `Text -> `Text
      | `None -> `None
      | `Bar -> `Bar fs
    in {change = change kind; new_file = new_file total kind;
        debug = debug verbose kind}
end


type t = {
  conf : string -> Conf.conf;
  skip_absent : bool;
  silence_errors : bool;
  log : Log.t;
  behavior : [`Suggest | `List_only];
}

let of_args (t : Utils.args) info =
  let conf = Conf.read_conf ?conf_file:t.conf_file in
  {conf; skip_absent = t.skip_absent; silence_errors = t.silence_errors;
   behavior = t.behavior; log = Log.make t.report info t.verbose}
