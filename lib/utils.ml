open Core

type command = [`Lint | `Dump]

type args = {
  command : command;
  report : [`Bar | `Text | `None];
  conf_file : string option;
  patch_file : string option;
  skip_absent : bool;
  silence_errors : bool;
  verbose : bool;
}

type 'a res = ('a, string) Result.t

let (let*) = Stdlib.Result.bind
let filter_errors = Result.map_error ~f:(fun m -> `Msg m)
let norm_error = Result.map_error ~f:(fun (`Msg m) -> m)

let find_file ?(containing_folder=false) name src =
  let rec search cur =
    let cur = Fpath.normalize cur in
    if Fpath.is_root cur then
      Result.failf "Could not find a %s file anywhere" name
    else
      let f = Fpath.add_seg cur name in
      match Sys.file_exists (Fpath.to_string f) with
      | `Yes when containing_folder -> Result.return cur 
      | `Yes -> Result.return f
      | _ -> search (Fpath.parent cur)
  in search (Fpath.to_dir_path src)

let find_file_s ?containing_folder name src =
  let* srcf = Fpath.of_string src |> norm_error in
  let* found = find_file ?containing_folder name srcf in
  Result.return (Fpath.to_string found)

type pos = {line : int ; col : int}[@@deriving show, sexp]
type chunk = {ch_begin : pos; ch_end : pos}[@@deriving show, sexp]

let map_result ~f l = 
  List.map ~f l |> Result.combine_errors |> Result.map_error ~f:(List.hd_exn)
