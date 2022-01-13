open Core
open Utils

let find_dune_root () = 
  let* src = Sys.getcwd () |> Fpath.of_string |> norm_error in
  let find_from src =
    let find x = Utils.find_file ~containing_folder:true x src in
    match find "dune-project", find "dune-workspace" with
    | Ok p, _ | _, Ok p -> Result.return p
    | error, _ -> error
  in
  let* first = find_from src in
  let rec find_farthest src =
    match find_from (Fpath.parent src) with
    | Ok src' -> find_farthest src'
    | Error _ -> src
  in Result.return (find_farthest first)

(* Cache it *)
let call_describe =
  let cache = ref None in fun () ->
    match !cache with
    | Some x -> Result.return x
    | None ->
      let open Feather in
      let {stdout; stderr; status} =
        process "dune" ["describe"; "--format=csexp"; "--lang=0.1"] |> collect everything
      in
      if status = 0 then
        let module Csexp = Csexp.Make(Sexp) in
        let* sexp =
          Csexp.parse_string stdout
          |> Result.map_error ~f:(fun (_, s) ->
              Printf.sprintf "dune describe's output could not be parsed: %s" s
            )
        in cache := Some sexp ; Result.return sexp
      else Result.fail stderr

let parse_describe path t =
  let open Sexp in
  let segs = Fpath.segs path in
  let nb = List.length segs in
  let matches f =
    let* fpath = Fpath.of_string f |> norm_error in
    let fsegs = Fpath.normalize fpath |> Fpath.segs in
    let n = List.length fsegs in
    let last = List.drop fsegs (n - nb) in
    Result.return (List.equal String.equal last segs)
  in
  let rec search x =
    let error = Result.failf "File not found in dune workspace" in
    let default () = match x with
      | Atom _ -> error
      | List l ->
        begin match List.map ~f:search l |> List.find ~f:Result.is_ok with
          | Some x -> x
          | None -> error
        end
    in
    match x with
    | List [
        List [Atom "name"; Atom _         ];
        List [Atom "impl"; List [Atom f]  ];
        List [Atom "intf"; List _         ];
        List [Atom "cmt" ; List [Atom cmt]];
        _] ->
      let* m = matches f in
      if m then Result.return cmt
      else default ()
    | _ -> default ()
  in search t

let find_cmt_location ~report filename =
  let of_string x = Fpath.of_string x |> norm_error in
  let* filename = of_string filename in
  let* dune_root = find_dune_root () in
  let* cwd = Sys.getcwd () |> Fpath.of_string |> norm_error in
  let cwd = Fpath.to_dir_path cwd in
  let* relative_cwd =
    if Poly.(report = `Text) then Stdio.printf "Dune root: %s\n" (Fpath.to_string dune_root);
    match Fpath.relativize ~root:dune_root cwd with
    | Some p -> Result.return p
    | None -> Result.failf "Invalid dune root prefix"
  in
  let filename_from_root =
    Fpath.append relative_cwd filename |> Fpath.normalize in
  let* description = call_describe () in
  let* found_cmt = parse_describe filename_from_root description in
  let* found_cmt = of_string found_cmt in
  let final = Fpath.(append dune_root found_cmt |> normalize) in
  let finals = Fpath.to_string final in
  let* () =
    if Poly.(Sys.file_exists finals = `Yes) then  Result.return ()
    else
      let open Feather in
      let* relative_root = match Fpath.relativize ~root:cwd dune_root with
        | Some p -> Result.return p
        | None -> Result.failf "Invalid dune root prefix"
      in
      let full_path = Fpath.(append relative_root found_cmt |> normalize) in
      let err, status = process "dune" ["build"; Fpath.to_string full_path] 
                        |> collect stderr_and_status in
      if status = 0 then Result.return ()
      else Result.failf "Could not build cmt: %s\n" err
  in
  Result.return finals

(** TODO: Call describe only once when handling multiple files *)

let get_typed_tree ~report filename =
  let* path = find_cmt_location ~report filename in
  let open Cmt_format in
  try
    match (read_cmt path).cmt_annots with
    | Implementation s -> Result.return s
    | _ -> Result.failf ".cmt file does not contain an implementation"
  with Error (Not_a_typedtree err) | Failure err ->
    Result.failf "Parsing of the .cmt file failed: %s" err
