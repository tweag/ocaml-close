open Base

type path = string [@@deriving show]
type short_path = string [@@deriving show]

type scope_kind = Module | Value
[@@deriving show]

type node_desc =
  | Open of path
  | Use of path * short_path
  | Def of short_path
  | Scope of scope_kind * t list

and t = {
  node_loc : Location.t; [@opaque]
  node_desc : node_desc;
}
[@@deriving show]

open Typedtree

module Make = struct
  open Tast_iterator

  let acc = ref []

  let mk_node node_loc node_desc = {node_loc; node_desc}

  let rec normalize = function
    | `Single x :: t -> x :: (normalize t)
    | `List l :: t -> (normalize l) @ (normalize t)
    | [] -> []

  let rec_run f x =
    let save = !acc in
    acc := []; f x;
    let r = !acc in
    acc := save; normalize r

  let return x = acc := !acc @ x

  let super = Tast_iterator.default_iterator

  let structure it x = 
    let child = rec_run (super.structure it) x in
    let node = mk_node Location.none (Scope (Module, child)) in
    return [`Single node]

  let value_binding it x =
    let child_right = rec_run (it.expr it) x.vb_expr in
    let node_right = mk_node x.vb_loc (Scope (Value, child_right)) in
    let child_left = rec_run (it.pat it) x.vb_pat in
    List.map (node_right :: child_left) ~f:(fun x -> `Single x) |> return

  let pat (type k) it (x : k general_pattern) =
    let loc = x.pat_loc in
    begin match x.pat_desc with
      | Tpat_var (_, {txt; _}) -> return [`Single (mk_node loc (Def txt))]
      | _ -> return []
    end; super.pat it x

  let open_declaration it x =
    begin match Analysis.Info.get_name x with
    | Ok name -> return [`Single (mk_node x.open_loc (Open name))]
    | Error _ -> return []
    end; super.open_declaration it x

  let iterator = {super with structure; value_binding; pat; open_declaration}

  let go t =
    acc := [];
    iterator.structure iterator t;
    match !acc with
    | [`Single x] -> Result.return x
    | _ -> Result.failf "Couldn't compute usedef tree"
end

let make = Make.go
