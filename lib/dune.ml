open Core
open Utils

let cwd_path () =
  let* cwd = Sys.getcwd () |> Fpath.of_string |> norm_error in
  Result.return (Fpath.to_dir_path cwd)

(* Memoize it *)
(* This returns an absolute path *)
let find_dune_root = 
  let cache = ref None in fun () ->
    match !cache with
    | Some x -> Result.return x
    | None ->
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
      in
      Result.return (find_farthest first)

let relative_dune_root () =
  let* cwd = cwd_path () in
  let* dune_root = find_dune_root () in
  match Fpath.relativize ~root:cwd dune_root with
  | Some p -> Result.return p
  | None -> Result.failf "Invalid dune root prefix"

(* Memoize it *)
let call_describe =
  let cache = ref None in fun () ->
    match !cache with
    | Some x -> Result.return x
    | None ->
      let open Feather in
      let {stdout; stderr; status} =
        process "dune" ["describe"; "--format=csexp"; "--lang=0.1"]
        |> collect everything
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

let path_of_string x = Fpath.of_string x |> norm_error

let all_ml_files () =
  let open Sexp in
  let* description = call_describe () in
  let rec search x =
    let default () = match x with
      | Atom _ -> []
      | List l -> List.map ~f:search l |> List.concat
    in
    match x with
    | List [
        List [Atom "name"; Atom _         ];
        List [Atom "impl"; List [Atom f]  ];
        List [Atom "intf"; List _         ];
        List [Atom "cmt" ; List _];
        _] -> [f]
    | _ -> default ()
  in
  let all_build_ml = search description in
  (* This is an assumption *)
  let build_dir = "_build/default/" in
  let remove_prefix s =
    match String.chop_prefix s ~prefix:build_dir with
    | Some x -> x
    | None -> s
  in
  let abs_paths = List.map ~f:remove_prefix all_build_ml in
  let* dune_root = relative_dune_root () in
  let* paths = map_result ~f:path_of_string abs_paths in
  let only_ml = List.filter paths ~f:(fun p ->
      Poly.(Fpath.get_ext p = ".ml")
    )
  in
  let fix p =
    Result.return Fpath.(append dune_root p |> normalize |> to_string)
  in
  map_result ~f:fix only_ml

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
    let error = Result.failf "This is not a .ml file known by dune" in
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

let build_cmt cmt_path =
  let open Feather in
  let* relative_root = relative_dune_root () in
  let full_path = Fpath.(append relative_root cmt_path |> normalize) in
  let {status; _} =
    process "dune"
      ["build"; "--cache=enabled"; Fpath.to_string full_path] 
    |> collect everything in
  if status = 0 then Result.return ()
  else Result.failf "Could not build the .cmt file!\n"

let find_cmt_location filename =
  let* dune_root = find_dune_root () in
  let* cwd = cwd_path () in
  let* relative_cwd =
    match Fpath.relativize ~root:dune_root cwd with
    | Some p -> Result.return p
    | None -> Result.failf "Invalid dune root prefix"
  in
  let filename_from_root =
    Fpath.append relative_cwd filename |> Fpath.normalize in
  let* description = call_describe () in
  let* found_cmt = parse_describe filename_from_root description in
  let* relative = path_of_string found_cmt in
  let absolute = Fpath.(append dune_root relative |> normalize) in
  Result.return (relative, absolute)

let find_or_build_cmt ~(params : Params.t) filename =
  let* relative, absolute = find_cmt_location filename in
  let absolute_str = Fpath.to_string absolute in
  let* () =
    if Poly.(Sys.file_exists absolute_str = `Yes) then Result.return ()
    else if params.skip_absent then Result.failf "Not built, skipping."
    else (params.log.change "Building"; build_cmt relative)
  in Result.return absolute_str
