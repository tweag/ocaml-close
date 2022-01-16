open Base
open Utils
open Params

(* Possible TODO: use typerex-lint ? But it's outdated *)
(* TODO: support for wildcard *)

type open_summary = {
  module_name : string;
  total : int;
  groups : int;
  layer_only : bool;
  imports_syntax : bool;
}[@@deriving show]

let is_module_id id = Char.is_uppercase id.[0]

let is_operator_id id =
  String.split ~on:'.' id
  |> List.last_exn
  |> String.exists ~f:(Fn.non Char.(fun c ->
      is_alphanum c || c = '_'
    ))

let compute_summary (t, use_sites) =
  let* name = Typed.Open_info.get_name t in
  let total = List.length use_sites in
  let h = Hashtbl.create (module String) in
  List.map ~f:fst use_sites
  |> List.iter ~f:(Hashtbl.incr h);
  let groups = Hashtbl.length h in
  let layer_only =
    Hashtbl.keys h
    |> List.for_all ~f:is_module_id
  in
  let imports_syntax = 
    Hashtbl.keys h
    |> List.exists ~f:is_operator_id
  in
  Result.return {module_name = name; total;
                 groups; layer_only; imports_syntax}

let enact_decision filename sum =
  let open Conf in
  Progress.interject_with (fun () -> function
      | Keep -> ()
      | Remove -> Stdio.printf "%s: refactor open %s\n" filename sum.module_name
      | _ -> Stdio.printf "%s: unknown decision on open %s\n" filename sum.module_name
    )

let apply_rule rule sum =
  let open Conf in
  let eval = function
    | Const n -> n
    | Uses -> sum.total
    | Symbols -> sum.groups
  in
  let rec apply = function
    | And l -> List.map ~f:apply l |> List.for_all ~f:Fn.id
    | Or l -> List.map ~f:apply l |> List.exists ~f:Fn.id
    | Not b -> not (apply b)
    | Eq (e1, e2) -> (eval e1) = (eval e2)
    | Leq (e1, e2) -> (eval e1) <= (eval e2)
    | Geq (e1, e2) -> (eval e1) >= (eval e2)
    | True -> true
    | False -> false
    | In_list l -> List.mem l sum.module_name ~equal:String.equal
    | Exports_syntax -> sum.imports_syntax
    | Exports_modules -> failwith "Exports_modules not yet implemented"
    | Exports_modules_only -> sum.layer_only
  in apply rule

let make_decision filename conf sum =
  let open Conf in
  let answers =
    List.map ~f:(fun (x, rule) -> (x, apply_rule rule sum)) conf.rules
  in
  let decision =
    List.fold (List.rev conf.precedence) ~init:Keep ~f:(fun acc kind ->
        match List.Assoc.find ~equal:Poly.equal answers kind with
        | None | Some false -> acc
        | Some true -> kind
      ) in
  enact_decision filename sum decision

module Progress_bar = struct
  open Progress
  let config = Config.(v ~persistent:false ())
  let bar1 ~total = Line.(list [ 
      const "Files";
      bar ~style:`UTF8 ~width:(`Fixed 60) ~color:Terminal.Color.(hex "#FA0") total;
      count_to total
    ])
  let bar2 = Line.(list [ 
      spinner ();
      string
    ])
  let b total = Multi.(line bar2 ++ line (bar1 ~total))
end

let get_summaries filename params =
  let* t = Typed.Extraction.get_typed_tree ~params filename in
  let opens = Typed.Open_info.gather t in
  params.log.change "Analyzing";
  List.map opens ~f:(fun x ->
      let* use_sites = Typed.Open_uses.compute t x in
      compute_summary (x, use_sites)
    )
  (* Filter out failing opens *)
  |> List.filter_map ~f:(function Ok o -> Some o | _ -> None)
  |> Result.return

let analyse params filename =
  begin
    params.log.change "Fetching";
    let* summaries = get_summaries filename params in
    let f = match params.behavior with
      | `Suggest -> make_decision filename params.conf
      | `List_only -> fun x -> Stdio.printf "%s\n" (show_open_summary x)
    in
    List.iter summaries ~f;
    Result.return ()
  end |> Result.map_error ~f:(fun s -> Printf.sprintf "-> %s: %s" filename s)

let execute args filenames =
  let total = List.length filenames in
  let bar = Progress_bar.b total in
  let go oreport freport =
    let params = Params.of_args args ((oreport, freport), total) in
    params.log.change "Starting";
    List.map filenames ~f:(fun filename ->
        params.log.new_file filename;
        analyse params filename
      )
    |> Result.combine_errors
    |> Result.map_error ~f:(fun err_list ->
        let errors = String.concat ~sep:"\n" err_list in
        Printf.sprintf "There were errors during processing:\n%s" errors
      )
    |> Result.map ~f:ignore
    |> filter_errors
  in
  match begin
    if Poly.(args.report = `Bar) then
      Progress.with_reporters ~config:Progress_bar.config bar go
    else go ignore ignore
  end with
  | Error _ when args.silence_errors -> Ok ()
  | s -> s
