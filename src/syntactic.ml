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
module Utils = Ppxlib__Utils
module IO = Utils.Ast_io 

let get_ast_ml filename =
  match IO.read (IO.File filename)
          ~input_kind:(Possibly_source (Utils.Kind.Impl, filename))
  with
  | Ok {ast = Impl s ; _ } -> s
  | _ -> failwith "Parsing failed"

