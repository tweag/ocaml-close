open Utils
open Ppxlib
open Base

let opens_of t =
  let o = object
    inherit [Ast.module_expr list] Ast_traverse.fold as super

    method! structure_item s acc =
      let acc = super#structure_item s acc in
      match s.pstr_desc with
      | Pstr_open {popen_expr; _} -> popen_expr :: acc
      | _ -> acc
  end
  in o#structure t []

let rec str_of_longident = function
  | Lident s -> s
  | Ldot (li, s) -> (str_of_longident li) ^ "." ^ s
  | Lapply (li1, li2) ->
    Printf.sprintf "%s(%s)" (str_of_longident li1) (str_of_longident li2)

(* TODO: really resolve module expr before checking against whitelist *)
let is_whitelisted whitelist module_expr =
  match module_expr.pmod_desc with
  | Pmod_ident {txt; _} ->
    let str = str_of_longident txt in
    List.exists ~f:(String.equal str) whitelist
  | _ -> false

let get_source_fragment filename start finish =
  let lines = Stdio.In_channel.read_lines filename in
  if start.line <> finish.line then
    Result.fail "Source fragment over multiple lines not implemented"
  else
    let delta = finish.col - start.col in
    match List.nth lines (start.line - 1) with
    | None -> Result.failf "No line %d in %s" start.line filename
    | Some line -> Result.return @@ String.sub line ~pos:start.col ~len:delta

(* TODO proper ast parsing with error management *)


let parse_source_code ~kind ~input_name ~prefix_read_from_source ic
  =
  let open Stdppx in
  try
    let lexbuf = Lexing.from_channel ic in
    let len = String.length prefix_read_from_source in
    Bytes.blit_string ~src:prefix_read_from_source ~src_pos:0
      ~dst:lexbuf.lex_buffer ~dst_pos:0 ~len;
    lexbuf.lex_buffer_len <- len;
    lexbuf.lex_curr_p <-
      { pos_fname = input_name; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
    (*Skip_hash_bang.skip_hash_bang lexbuf;*)
    let ast =
      match kind with
      | `Intf -> `Intf (Parse.interface lexbuf)
      | `Impl -> `Impl (Parse.implementation lexbuf)
    in
    Result.return ast
  with exn -> (
      match Location.Error.of_exn exn with
      | None -> raise exn
      | Some error -> Result.fail error
    )

let get_ast_ml filename =
  let chan = Stdio.In_channel.create filename in
  let ast =
    parse_source_code ~kind:`Impl ~input_name:filename
      ~prefix_read_from_source:"" chan
  in
  match ast with
  | Ok (`Impl s) -> Result.return s
  | _ -> Result.failf "Couldn't parse %s as a ML file" filename

let get_opens filename =
  let* ast = get_ast_ml filename in
  Result.return (opens_of ast)
