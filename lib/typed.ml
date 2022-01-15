open Core
open Params
open Utils

module Extraction = struct
  type t = Typedtree.structure

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

  let find_cmt_location ~params filename =
    let* dune_root = find_dune_root () in
    let* cwd = Sys.getcwd () |> Fpath.of_string |> norm_error in
    let cwd = Fpath.to_dir_path cwd in
    let* relative_cwd =
      if Poly.(params.rkind = `Text) then
        Stdio.printf "Dune root: %s\n" (Fpath.to_string dune_root);
      match Fpath.relativize ~root:dune_root cwd with
      | Some p -> Result.return p
      | None -> Result.failf "Invalid dune root prefix"
    in
    let filename_from_root =
      Fpath.append relative_cwd filename |> Fpath.normalize in
    params.oreport ("Describe", 0);
    let* description = call_describe () in
    let* found_cmt = parse_describe filename_from_root description in
    let* found_cmt = of_string found_cmt in
    let final = Fpath.(append dune_root found_cmt |> normalize) in
    let finals = Fpath.to_string final in
    let* () =
      if Poly.(Sys.file_exists finals = `Yes) then  Result.return ()
      else if params.skip_absent then Result.failf "Not built, skipping."
      else
        let open Feather in
        params.oreport ("Building", 0);
        let* relative_root = match Fpath.relativize ~root:cwd dune_root with
          | Some p -> Result.return p
          | None -> Result.failf "Invalid dune root prefix"
        in
        let full_path = Fpath.(append relative_root found_cmt |> normalize) in
        let {status; _} =
          process "dune"
            ["build"; "--cache=enabled"; Fpath.to_string full_path] 
          |> collect everything in
        if status = 0 then Result.return ()
        else Result.failf "Could not build the .cmt file!\n"
    in
    Result.return finals

  let get_typed_tree ~params filename =
    let* fpath = of_string filename in
    let* cmt_path = match Fpath.get_ext fpath with
      | ".ml" ->
        let with_cmt = Fpath.(set_ext ".cmt" fpath |> to_string) in
        begin match Sys.file_exists with_cmt with
          | `Yes -> Result.return with_cmt
          | _ -> find_cmt_location ~params fpath
        end
      | ".cmt" -> Result.return filename
      | _ -> Result.failf "%s is not a .ml or .cmt file" filename
    in
    let parse_error msg = Result.failf "Parsing of the .cmt failed: %s" msg in
    let open Cmt_format in
    try
      match (read_cmt cmt_path).cmt_annots with
      | Implementation s -> Result.return s
      | _ -> Result.failf ".cmt file does not contain an implementation"
    with
    | Cmi_format.Error err -> 
      parse_error @@ Format.asprintf "%a" Cmi_format.report_error err
    | Error (Not_a_typedtree err)
    | Failure err -> parse_error err

end

let segs_of_path path = 
  match Path.flatten path with
  | `Ok (i, l) -> Result.return (Ident.name i :: l)
  | `Contains_apply -> Result.fail "This module is not a simple identifier"

module Open_info = struct

  type t = Typedtree.open_declaration

  let iterator t f =
    let open Typedtree in
    let super = Tast_iterator.default_iterator in
    let structure_item i s =
      begin
        match s.str_desc with
        | Typedtree.Tstr_open o ->
          (* Do not consider structured opens *)
          if List.is_empty o.open_bound_items then f o
        | _ -> ()
      end; super.structure_item i s
    in
    let it = {super with structure_item} in
    it.structure it t

  let gather t =
    let l = ref [] in
    let f o = l := o :: !l in
    iterator t f; !l

  let get_position (t : t) =
    let Warnings.{loc_start = p; _} = t.open_expr.mod_loc in
    {line = p.pos_lnum; col = p.pos_cnum - p.pos_bol}

  let get_path (t : t) =
    match (t.open_expr).mod_desc with
    | Typedtree.Tmod_ident (path, _) -> Result.return path
    | _ -> Result.fail "This module is not a simple identifier"

  let get_name (t : t) =
    let* path = get_path t in
    Result.return (Path.name path)

  let strip_from_name (t : t) str =
    let vsegs = String.split ~on:'.' str in
    let* path = get_path t in
    let* msegs = segs_of_path path in
    let rec strip mp vp = match mp, vp with
      | m :: t1, v :: t2 when String.(m = v) -> strip t1 t2
      | _ :: t1, _ -> strip t1 vp
      | [], _ -> vp
    in
    strip msegs vsegs |> String.concat ~sep:"." |> Result.return
end

module Open_uses = struct
  open Typedtree
  open Types

  (* TODO actually only visit the scope of the open *)
  (* TODO missing module types, module names *)

  let f_if_constr f t = match t.desc with
    | Tconstr (path, _, _) -> f path
    | _ -> ()

  (* TODO replace last element of path by constructor name ? *)
  let check_constructor f desc = f_if_constr f desc.cstr_res

  (* TODO idem ? *)
  let check_label f desc = f_if_constr f desc.lbl_res

  let path_iterator t f =
    let super = Tast_iterator.default_iterator in
    let pat (type k) it (p : k general_pattern) =
      List.iter p.pat_extra ~f:(fun (pe, _, _) -> match pe with
          | Tpat_type (path, _)
          | Tpat_open (path, _, _) -> f path
          | _ -> ()
        );
      begin match p.pat_desc with
        | Tpat_construct (_, cons_desc, _) -> check_constructor f cons_desc
        | Tpat_record (fields, _) ->
          List.iter fields ~f:(fun (_, lab_desc, _) -> check_label f lab_desc)
        | _ -> ()
      end; super.pat it p
    in
    let expr it e =
      begin match e.exp_desc with
        | Texp_instvar (path1, path2, _) -> f path1; f path2
        | Texp_ident (path, _, _)
        | Texp_new (path, _, _) 
        | Texp_override (path, _)
        | Texp_extension_constructor (_, path) -> f path
        | Texp_letop {let_; _} -> f let_.bop_op_path
        | Texp_construct (_, cons_desc, _) -> check_constructor f cons_desc
        | Texp_field (_, _, lab_desc)
        | Texp_setfield (_, _, lab_desc, _) -> check_label f lab_desc
        | Texp_record {fields; _} ->
          Array.iter fields ~f:(fun (lab_desc, _) -> check_label f lab_desc)
        | _ -> ()
      end; super.expr it e
    in
    let it = {super with expr; pat} in
    it.structure it t

  let compute t o =
    let* opath = Open_info.get_path o in
    let rec matches os vs = match os, vs with
      | o :: t1, v :: t2 when String.(o = v) -> matches t1 t2
      | [], [] -> None
      | [], _ -> Some vs
      | _, _ -> None
    in
    let uses = ref [] in
    let f vpath = 
      begin
        let* osegs = segs_of_path opath in
        let* vsegs = segs_of_path vpath in
        match matches osegs vsegs with
        | Some suffix ->
          let suffix = String.concat ~sep:"." suffix in
          Result.return (uses := suffix :: !uses)
        | None -> Result.return ()
      end |> ignore (* Silence individual errors, no need to stop everything *)
    in
    try path_iterator t f; Result.return (!uses)
    with Failure e -> Result.fail e
end
