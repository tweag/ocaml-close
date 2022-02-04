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

let of_string x = Fpath.of_string x |> norm_error

let cwd_path () =
  let* cwd = Sys.getcwd () |> Fpath.of_string |> norm_error in
  Result.return (Fpath.to_dir_path cwd)

let build_cmt cmt_path =
  let open Feather in
  let* cwd = cwd_path () in
  let* dune_root = find_dune_root () in
  let* relative_root = match Fpath.relativize ~root:cwd dune_root with
    | Some p -> Result.return p
    | None -> Result.failf "Invalid dune root prefix"
  in
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
  let* relative = of_string found_cmt in
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
