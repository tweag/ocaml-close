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
  command : Utils.command;
  conf : string -> Conf.t;
  patch_file : string;
  skip_absent : bool;
  print_tree : bool;
  silence_errors : bool;
  parallel : bool;
  log : Log.t;
}

let of_args (t : Utils.args) info =
  let conf = Conf.read_conf ?conf_file:t.conf_file in
  let patch_file = match t.patch_file with
    | None -> Filename.temp_file "patch" ".close"
    | Some x -> x
  in
  let report = if t.parallel then `None else t.report in
  let log = Log.make report info t.verbose in
  {
    conf; skip_absent = t.skip_absent; silence_errors = t.silence_errors;
    command = t.command; log; patch_file; print_tree = t.print_tree;
    parallel = t.parallel
  }
