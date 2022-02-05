open Core
open Utils
open Typedtree

let lines_of_loc loc =
  Warnings.(loc.loc_end.pos_lnum - loc.loc_start.pos_lnum + 1)

(* Loc equality modulo ghost *)
let loc_equal l1 l2 =
  Warnings.(Poly.(l1.loc_start = l2.loc_start && l1.loc_end = l2.loc_end))

module Extraction = struct
  type t = structure

  let of_string x = Fpath.of_string x |> norm_error

  let get_typed_tree ~params filename =
    let* fpath = of_string filename in
    let* cmt_path = match Fpath.get_ext fpath with
      | ".ml" ->
        let with_cmt = Fpath.(set_ext ".cmt" fpath |> to_string) in
        begin match Sys.file_exists with_cmt with
          | `Yes -> Result.return with_cmt
          | _ -> Dune.find_or_build_cmt ~params fpath
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

  let loc t =
    (** TODO find another way to get structure module,
     * potentially by carrying the loc around in Find (found in
     * module_expressions *)
    if List.is_empty t.str_items then Location.none
    else
      let non_ghost =
        List.filter ~f:(fun x -> not x.str_loc.loc_ghost) t.str_items in
      match (List.hd non_ghost, List.last t.str_items) with
      | Some first, Some last ->
        let loc = first.str_loc in
        let loc_end = last.str_loc.loc_end in
        {loc with loc_end}
      | _ -> Location.none

  let source_lines t = lines_of_loc (loc t)

  let print t = Caml.Format.printf "@[<v 2>%a@]@," Printtyped.implementation t
end

let segs_of_path path = 
  match Path.flatten path with
  | `Ok (i, l) -> Result.return (Ident.name i :: l)
  | `Contains_apply -> Result.fail "This module is not a simple identifier"

let pos_of_lexpos lp =
  Lexing.{line = lp.pos_lnum; col = lp.pos_cnum - lp.pos_bol}

let chunk_of_loc loc = 
  let Warnings.{loc_start; loc_end; _} = loc in
  let ch_begin = pos_of_lexpos loc_start in
  let ch_end = pos_of_lexpos loc_end in
  {ch_begin; ch_end}

module Open_info = struct

  type t = open_declaration

  let iterator t f =
    let super = Tast_iterator.default_iterator in
    let structure_item i s =
      begin
        match s.str_desc with
        | Tstr_open o ->
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

  let get_chunk (t : t) = chunk_of_loc t.open_loc

  let get_path (t : t) =
    match (t.open_expr).mod_desc with
    | Tmod_ident (path, _) -> Result.return path
    | _ -> Result.fail "This module is not a simple identifier"

  let get_ident (t : t) =
    match (t.open_expr).mod_desc with
    | Tmod_ident (_, loc) -> Result.return loc.txt
    | _ -> Result.fail "This module is not a simple identifier"

  let get_name (t : t) =
    let* path = get_path t in
    Result.return (Path.name path)

  let get_short_name (t : t) =
    let* ident = get_ident t in
    Result.return (Longident.flatten ident |> String.concat ~sep:".")
    (*
    let env = t.open_env in
    let* path = get_path t in
    let* _segs = segs_of_path path in
    (
    if Env.bound_module "Dune__exe" env then
      Printf.printf "bound !!!!!!\n"
    else Printf.printf "Non\n"
  );
    (*
    let mo = Env.find_module path env in
    ignore mo;
    List.drop_while segs ~f:(fun x -> Env.bound_module x env)
    |> String.concat ~sep:"." |> Result.return
       *)
    Result.return "bonjour"*)
end

module Find = struct
  let position_between ~start ~finish p =
    Lexing.(start.pos_cnum <= p.pos_cnum && p.pos_cnum <= finish.pos_cnum)

  let position_in loc p =
    Location.(position_between ~start:loc.loc_start ~finish:loc.loc_end p)

  let location_enclosed ~outer l =
    Location.(position_in outer l.loc_start && position_in outer l.loc_end)

  (* Find smallest structure in t that contains all locations in vlocs *)
  let enclosing_structure vlocs t =
    let best_candidate = ref t in
    let super = Tast_iterator.default_iterator in
    let structure i s =
      let str_loc = Extraction.loc s in
      let all_enclosed = Core_kernel.List.for_all vlocs ~f:(
          location_enclosed ~outer:str_loc
        )
      in
      if all_enclosed then begin
        let old_s = !best_candidate in
        let loc, old_loc = Extraction.(loc s, loc old_s) in
        if location_enclosed ~outer:old_loc loc then
          best_candidate := s
      end;
      super.structure i s
    in
    let it = {super with structure} in
    it.structure it t; !best_candidate

  (* The scope of an open is its smallest encloding module (including toplevel),
   * restrained to values *after* the open statement *)
  let scope_of_open t op =
    let loc = op.open_loc in
    let surround_loc = enclosing_structure [loc] t |> Extraction.loc in
    {loc with loc_end = surround_loc.loc_end}

  let scope_lines t op = scope_of_open t op |> lines_of_loc

  (* For a list of locations in a tree, return the list of their corresponding
   * containing functions if it exists *)
  let enclosing_functions vlocs t =
    let candidates = List.map vlocs ~f:(fun _ -> ref None) in
    let super = Tast_iterator.default_iterator in
    let update_one vb current orig_loc =
      let fn_loc = vb.vb_loc in
      if location_enclosed ~outer:fn_loc orig_loc then begin
        match !current with
        | None -> current := Some vb
        | Some old_vb ->
          let old_loc = old_vb.vb_loc in
          if location_enclosed ~outer:old_loc fn_loc then
            current := Some vb
      end
    in
    let structure_item i si =
      match si.str_desc with
      | Tstr_value (_, vbs) ->
        List.iter vbs ~f:(fun vb ->
            List.iter2_exn ~f:(update_one vb) candidates vlocs;
          ); super.structure_item i si
      | _ -> super.structure_item i si
    in
    let it = {super with structure_item} in
    it.structure it t;
    List.map candidates ~f:(fun x -> !x)

  (* Find the first sub-expression that does not have a ghost location *)
  let first_real_expression exp =
    let found = ref None in
    let super = Tast_iterator.default_iterator in
    let expr it expr =
      if expr.exp_loc.loc_ghost then
        super.expr it expr
      else if not @@ List.is_empty expr.exp_extra then
        (* If the expression has additional (extra) type constraints, its
         * location is unreliable. Find the next one (hacky) *)
        super.expr it expr
      else found := Some expr
    in
    let it = {super with expr} in
    it.expr it exp; !found

  (* Gather the locs of all types that have deriving attributes,
     as a heuristic for detecting PPX deriving *)
  let attributed_type_locs t =
    let found = ref [] in
    let super = Tast_iterator.default_iterator in
    let structure_item it si =
      begin match si.str_desc with
        | Tstr_type (_, typs) ->
          List.iter typs ~f:(fun typ ->
              if not @@ List.is_empty typ.typ_attributes then
                found := typ.typ_loc :: !found
            )
        | _ -> ()
      end; super.structure_item it si
    in
    let it = {super with structure_item} in
    it.structure it t; !found
end

module Open_uses = struct
  type use_loc = Location.t

  type use_kind = Uk_Module | Uk_Module_Type | Uk_Value | Uk_Type of int
  [@@deriving show]

  type use = {name : string; loc : use_loc; kind : use_kind}

  (* TODO if we encounter a (local or global) open for the same module, warn
   * that it is subsumed *)

  let f_if_constr f t =
    let open Types in
    match t.desc with
    | Tconstr (path, args, _) -> f (Uk_Type (List.length args)) path
    | _ -> ()

  let check_constructor f desc = f_if_constr f desc.Types.cstr_res

  let check_label f desc = f_if_constr f desc.Types.lbl_res

  type f = use_loc -> ?txt:Longident.t -> use_kind -> Path.t -> unit
  let path_iterator t (f : f) =
    let super = Tast_iterator.default_iterator in
    let attributed_type_locs = Find.attributed_type_locs t in
    let f loc =
      (* Mark locations that are the same as attributed types as ghost, since
         they probably correspond to PPX locations *)
      if List.mem attributed_type_locs loc ~equal:loc_equal then
        f Warnings.{loc with loc_ghost = true}
      else f loc
    in
    (* explicit type annotation *)
    let typ it typ =
      begin match typ.ctyp_desc with
        | Ttyp_constr (path, {txt; loc}, args) ->
          f loc ~txt (Uk_Type (List.length args)) path
        | _ -> ()
      end; super.typ it typ
    in
    let pat (type k) it (p : k general_pattern) =
      let f = f p.pat_loc in
      List.iter p.pat_extra ~f:(fun (pe, _, _) -> match pe with
          | Tpat_type (path, {txt; _}) -> f ~txt (Uk_Type 0) path 
          | Tpat_open (path, {txt; _}, _) -> f ~txt Uk_Module path
          | _ -> ()
        );
      begin match p.pat_desc with
        | Tpat_construct ({txt; _}, cons_desc, _) ->
          check_constructor (f ~txt) cons_desc
        | Tpat_record (fields, _) ->
          List.iter fields ~f:(fun ({txt; _}, lab_desc, _) ->
              check_label (f ~txt) lab_desc
            )
        | _ -> ()
      end; super.pat it p
    in
    let expr it e =
      let f = f e.exp_loc in
      if List.is_empty e.exp_attributes then
        begin match e.exp_desc with
          | Texp_instvar (path1, path2, _) ->
            f Uk_Value path1; f Uk_Value path2
          | Texp_override (path, _) -> f Uk_Value path
          | Texp_ident (path, {txt; _}, _)
          | Texp_new (path, {txt; _}, _) 
          | Texp_extension_constructor ({txt; _}, path) -> f ~txt Uk_Value path
          | Texp_letop {let_; _} -> f Uk_Value let_.bop_op_path
          | Texp_construct ({txt; _}, cons_desc, _) ->
            check_constructor (f ~txt) cons_desc
          | Texp_field (_, {txt; _}, lab_desc)
          | Texp_setfield (_, {txt; _}, lab_desc, _) ->
            check_label (f ~txt) lab_desc
          | Texp_record {fields; _} ->
            Array.iter fields ~f:(fun (lab_desc, rec_lab) ->
                match rec_lab with
                | Kept _ -> check_label f lab_desc
                | Overridden ({txt; _}, _) -> check_label (f ~txt) lab_desc
              )
          | _ -> ()
        end;
      super.expr it e
    in
    let module_expr it m =
      begin match m.mod_desc with
        | Tmod_ident (path, {txt; _}) -> f ~txt m.mod_loc Uk_Module path
        | _ -> ()
      end; super.module_expr it m
    in
    let module_type it m =
      begin match m.mty_desc with
        | Tmty_ident (path, {txt; _}) -> f ~txt m.mty_loc Uk_Module_Type path
        | _ -> ()
      end; super.module_type it m
    in
    let it = {super with expr; pat; module_expr; typ; module_type}
    in it.structure it t

  let rec largest_subpaths p =
    if Poly.(Path.flatten p = `Contains_apply) then
      match p with
      | Pident _ -> assert false
      | Pdot (p, _) -> largest_subpaths p
      | Papply (p1, p2) -> largest_subpaths p1 @ largest_subpaths p2
    else [p]

  let compute t o =
    (** Function to check that candidate uses are really in the open scope *)
    let check_scope =
      let scope = Find.scope_of_open t o in
      fun loc -> Find.location_enclosed ~outer:scope loc
    in
    let* opath = Open_info.get_path o in
    let rec matches os vs = match os, vs with
      | o :: t1, v :: t2 when String.(o = v) -> matches t1 t2
      | [], [] -> None
      | [], _ -> Some vs
      | _, _ -> None
    in
    let uses = ref [] in
    (* Path of the open *)
    let* osegs = segs_of_path opath in
    (* Path of the open short name *)
    let* sosegs = Open_info.get_ident o in
    let sosegs = Longident.flatten sosegs in
    (* Function applied on each potential use *)
    let rec f loc ?(txt=Longident.Lident "") kind vpath = 
      (* if the path contains a functor application that cannot have been found
       * by the iterator (because it's a type), find the largest subpaths
       * in it *)
      let is_type = match kind with Uk_Type _ -> true | _ -> false in
      if is_type && Poly.(Path.flatten vpath = `Contains_apply) then
        let paths = largest_subpaths vpath in
        List.iter ~f:(f loc Uk_Module) paths
      else
        begin
          if check_scope loc then (
            (* Path of the fully-qualified use *)
            let* vsegs = segs_of_path vpath in
            (* Path of the actual use *)
            let isegs = Longident.flatten txt in
            if Option.is_some (matches sosegs isegs) then
              (* Use is already qualified, skip *)
              Result.return ()
            else if not @@ String.is_empty (Longident.last txt) &&
                    List.(length vsegs <> length osegs + length isegs)
            then
              (* Use is under-qualified: its the use of another sub-open *)
              Result.return ()
            else (
              (* Isolate the suffix *)
              match matches osegs vsegs with
              | Some suffix ->
                let name = String.concat ~sep:"." suffix in
                let use = {name; loc; kind} in
                uses := use :: !uses;
                Result.return ()
              | None ->
                Result.return ()
            )
        ) else Result.return ()
        end |> ignore (* Silence individual errors, no need to stop everything *)
    in
    try
      path_iterator t f; Result.return (!uses)
    with Failure e -> Result.fail e

  let optimal_scoped_position t uses =
    let use_locs = List.map ~f:(fun x -> x.loc) uses in
    let closest_structure = Find.enclosing_structure use_locs t in
    let loc = Extraction.loc closest_structure in
    pos_of_lexpos loc.loc_start

  let optimal_global_position t uses =
    let use_locs = List.map ~f:(fun x -> x.loc) uses in
    let closest_structure = Find.enclosing_structure use_locs t in
    let compare l1 l2 =
      let open Warnings in
      Int.compare l1.loc_start.pos_cnum l2.loc_start.pos_cnum
    in
    match List.min_elt use_locs ~compare with
    | None -> pos_of_lexpos (Extraction.loc t).Location.loc_start
    | Some earliest_use ->
      let best_res =
        List.map closest_structure.str_items
          ~f:(fun s -> s.str_loc)
        (* Compute distance to earliest use, and keep only items before use *)
        |> List.filter_map ~f:(fun l ->
            let open Warnings in
            let diff = l.loc_start.pos_cnum - earliest_use.loc_start.pos_cnum in
            if diff > 0 then None
            else Some (l, -diff)
          )
        (* Find element with minimum distance *)
        |> List.min_elt ~compare:(fun (_, a) (_, b) -> Int.compare a b)
      in
      let best_pos = (fst (Option.value_exn best_res)).Location.loc_start
      in pos_of_lexpos best_pos

  let by_function t uses =
    (* try to find first non-ghost expression to have the right pos *)
    let locs = List.map ~f:(fun x -> x.loc) uses in
    let functions = Find.enclosing_functions locs t in
    if List.exists functions ~f:Option.is_none then None
    else
      let functions = List.filter_opt functions in
      let h = Hashtbl.Poly.create () in
      let ok = ref true in
      let attrs = Find.attributed_type_locs t in
      List.iter2_exn locs functions ~f:(fun loc f ->
          (* Find first position after '=' *)
          match Find.first_real_expression f.vb_expr with
          | Some expr ->
            if not @@ Find.location_enclosed ~outer:expr.exp_loc loc then
              (* Bail out if that location does not contain our use anymore... *)
              ok := false
            else if List.mem ~equal:loc_equal attrs loc then 
              (* or if the final location seems to be in generated code *)
              ok := false
            else (* OK *)
              let pos = pos_of_lexpos expr.exp_loc.loc_start in
              Hashtbl.Poly.incr h pos
          | None ->
            (* Or if the location is completely ghost *)
            ok := false
        );
      if !ok then Some h else None

  let has_ghost_uses uses =
    List.exists uses ~f:(fun {loc; _} -> loc.Warnings.loc_ghost)
end
