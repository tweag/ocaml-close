open Utils
open Base

let get_source_fragment file start finish =
  if start.line <> finish.line then
    Result.fail "Source fragment over multiple lines not implemented"
  else
    let delta = finish.col - start.col in
    match List.nth file (start.line - 1) with
    | None -> Result.failf "No line %d in file" start.line
    | Some line -> Result.return @@ String.sub line ~pos:start.col ~len:delta
