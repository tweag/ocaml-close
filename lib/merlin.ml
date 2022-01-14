open Base
open Utils

let merlin ~filename args =
  let open Feather in
  let args = ["server"] @ args @ ["-filename"; filename] in
  cat filename |. process "ocamlmerlin" args

let call_merlin ~filename ~command =
  let open Yojson.Safe in
  let args = match command with
    | `Open pos -> ["refactor-open"; "-position";
                    string_of_pos pos; "-action"; "qualify"]
    | `Errors -> ["errors"]
  in
  try
  let raw = 
    merlin ~filename args
    |> Feather.(collect stdout)
    |> from_string
  in
  assert String.(Util.(member "class" raw |> to_string = "return"));
  Result.return (Util.member "value" raw)
  with e ->
    Result.failf "Error while calling/parsing merlin: %s" (Exn.to_string e)

let check_errors filename =
  let* errors = call_merlin ~filename ~command:`Errors in
  let errors = Yojson.Safe.Util.to_list errors in
  if List.is_empty errors then Result.return ()
  else
    let errors = 
      List.take errors 1
      |> List.map ~f:Yojson.Safe.Util.(fun t -> member "message" t |> to_string)
      |> String.concat ~sep:"\n"
    in
    Result.failf "Merlin has errors. Here is the first one:\n%s\n" errors

let uses_of_open filename t =
  let pos = Typed.Open_explore.get_position t in
  let command = `Open pos in
  let* answer = call_merlin ~filename ~command in
  Yojson.Safe.Util.to_list answer
  |> map_result ~f:qualify_of_yojson
