open Utils
open Core

type single =
  | Delete of chunk
  | Insert of {what : string; where : pos; newline : bool}
[@@deriving show, sexp]

type t =
  | Valid of {actions : single list; filename : string}
  | Invalid of string
[@@deriving show, sexp]

let merge t1 t2 = match t1, t2 with
  | Valid t1, Valid t2 ->
    if String.(t1.filename <> t2.filename) then
      Invalid "Merging patches for two different files"
    else
      Valid {t1 with actions = t1.actions @ t2.actions}
  | _ -> Invalid "Merging invalid patches"

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

let apply_single (lines : Piece_table.t Array.t) = function
  | Delete chunk ->
    let line_no = chunk.ch_begin.line in
    let line = lines.(line_no - 1) in
    let length = chunk.ch_end.col - chunk.ch_begin.col in
    Piece_table.delete_many_orig  ~pos:chunk.ch_begin.col ~length line
  | Insert {what; where; newline} ->
    let line_no = where.line in
    let line = lines.(line_no - 1) in
    let to_insert = if newline then
        what ^ "\n" ^ (String.make where.col ' ')
      else what
    in
    Piece_table.insert ~orig_pos:true ~pos:where.col to_insert line
    |> ignore

let suggested_suffix = ".close.ml"

let apply =
  function
  | Invalid s -> Result.failf "Couldn't apply an invalid patch: %s" s
  | Valid {actions; filename} ->
    try
      if List.is_empty actions then Result.return ()
      else (
        Stdio.printf "- Patching %s...\n%!" filename;
        let out_filename = filename ^ suggested_suffix in
        let is_empty_str = String.for_all ~f:Char.is_whitespace in
        let lines =
          Stdio.In_channel.read_lines filename
          |> Array.of_list
          |> Array.map ~f:Piece_table.create
        in
        List.iter ~f:(apply_single lines) actions;
        Array.to_list lines
        |> List.filter_map ~f:(fun l ->
            let s = Piece_table.contents l in
            if is_empty_str s && not @@ Piece_table.is_original l then
              None
            else Some s
          )
        |> Stdio.Out_channel.write_lines out_filename;
        Result.return ()
      )
    with Sys_error s -> Result.fail s

let clean () =
  Stdio.printf "Deleting...\n%!";
  let p = Feather.process "find"
      ["-name"; "*.ml" ^ suggested_suffix; "-print"; "-delete"] in
  let stderr, status = Feather.(collect stderr_and_status p) in
  begin if status = 0 then
      Stdio.printf "Done.\n" |> Result.return
    else Result.failf "Clean failed: %s" stderr
  end |> filter_errors

let exports ts filename =
  let sexps =
    List.filter ts ~f:(Fn.non is_empty)
    |>List.map ~f:sexp_of_t in
  Sexp.save_sexps filename sexps

let imports filename =
  let sexps = Sexp.load_sexps filename in
  List.map sexps ~f:t_of_sexp

let apply_saved filename =
  Stdio.printf
    "Applying recommendations; modified files will be suffixed with %s...\n%!"
    suggested_suffix;
  let patches = imports filename in
  map_result patches ~f:apply |> Result.map ~f:ignore |> filter_errors
