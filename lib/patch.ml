open Utils

type single =
  | Delete of chunk
  | Insert of {what : string; where : pos; newline : bool}
[@@deriving show]

type t =
  | Valid of {actions : single list; filename : string}
  | Invalid of string
[@@deriving show]

let delete ~chunk = function
  | Invalid s -> Invalid s
  | Valid t ->
    if chunk.ch_begin.line = chunk.ch_end.line then
      Valid {t with actions = (Delete chunk) :: t.actions}
    else Invalid "cannot delete chunk of multiple lines"

let insert ?(newline=false) ~at what = function
  | Invalid s -> Invalid s
  | Valid t ->
    Valid {t with actions = (Insert {what; where=at; newline}) :: t.actions}

let empty filename = Valid {actions = []; filename}

let invalid why = Invalid why

let is_empty = function Valid t -> Base.List.is_empty t.actions | _ -> true

(* TODO: when applying insert, DO NOT apply if string already exist at this
 * place *)

(* TODO: allow to merge patches when there are multiple actions on same file *)

(* TODO: manage multiple edits on same line. with trie ? *)

(* TODO: delete lines that end up empty after delete *)

open Base

let apply_single lines = function
  | Delete chunk ->
    let line_no = chunk.ch_begin.line in
    let line = lines.(line_no - 1) in
    let before = String.prefix line chunk.ch_begin.col in
    let after = String.drop_prefix line chunk.ch_end.col in
    lines.(line_no - 1) <- before ^ after
  | Insert {what; where; newline} ->
    let line_no = where.line in
    let line = lines.(line_no - 1) in
    let before = String.prefix line where.col in
    let after = String.drop_prefix line where.col in
    let final = if newline then
        before ^ what ^ "\n" ^ (String.make (String.length before) ' ') ^ after
      else before ^ what ^ after
    in
    lines.(line_no - 1) <- final

let apply = function
  | Invalid s -> Result.failf "Couldn't apply an invalid patch: %s" s
  | Valid {actions; filename} ->
    let out_filename = filename ^ ".suggested.ml" in
    let lines = Array.of_list @@ Stdio.In_channel.read_lines filename in
    List.iter ~f:(apply_single lines) actions;
    Stdio.Out_channel.write_lines out_filename (Array.to_list lines);
    Result.return ()
