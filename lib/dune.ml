open Core
open Utils

type ext_kind = [`Cmt | `Cmi]

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

type dune_module = {
  name : string;
  impl : string option;
  intf : string option;
  cmt : string option;
  cmti : string option;
}[@@deriving of_sexp]

let describe_match_module
    ~(f : impl:string -> cmt:string -> 'a)
    ~(default : unit -> 'a) t =
  try
    let m = dune_module_of_sexp t in
    let impl = Option.value_exn m.impl in
    let cmt = Option.value_exn m.cmt in
    f ~impl ~cmt
  with _ -> default ()

(* Paths relative to CWD *)
let all_ml_files () =
  let open Sexp in
  let* description = call_describe () in
  let rec search x =
    let default () = match x with
      | Atom _ -> []
      | List l -> List.map ~f:search l |> List.concat
    in
    describe_match_module x ~default ~f:(fun ~impl ~cmt:_ -> [impl])
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
  let* cwd = cwd_path () in
  let fix p =
    let normalized = Fpath.(append cwd (append dune_root p) |> normalize) in
    (* Try to relativize *)
    match Fpath.rem_prefix cwd normalized with
    | Some p -> Fpath.to_string p
    | None -> Fpath.(append dune_root p |> normalize |> to_string)
  in
  List.map ~f:fix only_ml |> Result.return

let parse_describe ~kind path t =
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
    describe_match_module x ~default ~f:(fun ~impl ~cmt ->
        let* m = matches impl in
        if m then
          let* path = path_of_string cmt in
          match kind with
          | `Cmt -> Result.return path
          | `Cmi -> Result.return (Fpath.set_ext "cmi" path)
        else default ()
      )
  in search t

let build_obj obj_path =
  let open Feather in
  let* relative_root = relative_dune_root () in
  let full_path = Fpath.(append relative_root obj_path |> normalize) in
  let {status; _} =
    process "dune"
      ["build"; "--cache=enabled"; Fpath.to_string full_path] 
    |> collect everything in
  if status = 0 then Result.return ()
  else Result.failf "Could not build the file %s!\n" (Fpath.to_string obj_path)

let find_obj_location ~kind filename =
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
  let* relative = parse_describe ~kind filename_from_root description in
  let absolute = Fpath.(append dune_root relative |> normalize) in
  Result.return (relative, absolute)

let find_or_build_obj ~kind ~(params : Params.t) filename =
  let* relative, absolute = find_obj_location ~kind filename in
  let absolute_str = Fpath.to_string absolute in
  let* () =
    if Poly.(Sys.file_exists absolute_str = `Yes) then Result.return ()
    else if params.common.skip_absent then Result.failf "Not built, skipping."
    else (params.log.change "Building"; build_obj relative)
  in Result.return absolute_str
