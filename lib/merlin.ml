open Ppxlib
open Base
open Utils

let merlin ~filename args =
  let open Feather in
  let args = ["server"] @ args @ ["-filename"; filename] in
  cat filename |. process "ocamlmerlin" args

let call_merlin ~filename ~command =
  let open Yojson.Safe in
  let args = match command with
    | `Open pos -> ["refactor-open"; "-position"; pos; "-action"; "qualify"]
    | `Errors -> ["errors"]
  in
  let raw = 
    merlin ~filename args
    |> Feather.(collect stdout)
    |> from_string
  in
  assert String.(Util.(member "class" raw |> to_string = "return"));
  Util.member "value" raw

let check_errors filename =
  let errors =
    call_merlin ~filename ~command:`Errors
    |> Yojson.Safe.Util.to_list
  in
  if List.is_empty errors then Result.return ()
  else
    let errors = 
      List.take errors 1
      |> List.map ~f:Yojson.Safe.Util.(fun t -> member "message" t |> to_string)
      |> String.concat ~sep:"\n"
    in
    Result.failf "Merlin has errors. Here is the first one:\n%s\n" errors

let uses_of_open filename module_expr =
  let pos = string_of_location module_expr.pmod_loc  in
  let command = `Open pos in
  call_merlin ~filename ~command
  |> Yojson.Safe.Util.to_list
  |> map_result ~f:qualify_of_yojson
