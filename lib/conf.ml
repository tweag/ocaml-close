open Core
open Utils

type conf = {
  whitelist : string list;
}[@@deriving yojson]

let default = {whitelist = []}

let conf_file_name = ".ocamlclose.json"

let find_conf_file src =
  let rec search cur =
    let cur = Fpath.normalize cur in
    if Fpath.is_root cur then
      Result.failf "Could not find a %s file anywhere" conf_file_name
    else
      let f = Fpath.add_seg cur conf_file_name |> Fpath.to_string in
      match Sys.file_exists f with
      | `Yes -> Result.return f
      | _ -> search (Fpath.parent cur)
  in search src

let read_conf ?conf_file () =
  let do_try () =
    let* filename =
      match conf_file with
      | Some x -> Result.return x
      | None ->
        let* src = Sys.getcwd () |> Fpath.of_string
                   |> Result.map_error ~f:(fun (`Msg g) -> g)
        in find_conf_file src
    in
    let channel = Stdio.In_channel.create filename in
    let raw = Yojson.Safe.from_channel channel in
    conf_of_yojson raw
  in match do_try () with
  | Ok c -> c
  | Error m ->
    Stdio.printf
      "Could not load configuration: %s.\n\
       Falling back to default config.\n"
      m;
    default
