open Base
open Utils

(* Possible TODO: use typerex-lint ? But it's outdated *)
(* TODO: support for wildcard *)

type open_summary = {
  module_name : string;
  total : int;
  groups : int;
  layer_only : bool;
  imports_syntax : bool;
}(*[@@deriving show]*)

let is_module_id id = Char.is_uppercase id.[0]

let is_operator_id id =
  String.split ~on:'.' id
  |> List.last_exn
  |> String.exists ~f:(Fn.non Char.(fun c ->
      is_alphanum c || c = '_'
    ))

let compute_summary (t, use_names) =
  let* name = Typed.Open_info.get_name t in
  let total = List.length use_names in
  let h = Hashtbl.create (module String) in
  List.iter ~f:(Hashtbl.incr h) use_names;
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
  (*Stdio.printf "%s\n" (show_open_summary sum);*)
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
      const "Opens";
      using snd @@ bar ~style:`UTF8 ~width:(`Fixed 60) ~data:`Latest
        ~color:Terminal.Color.(hex "#5500FF") 100;
      using fst string
    ])
  let b total = Multi.(line bar2 ++ line (bar1 ~total))
end

let get_summaries filename skip_absent report oreport =
  let* t = Typed.Extraction.get_typed_tree
      ~skip_absent ~report:(report, oreport) filename in
  let opens = Typed.Open_info.gather t in
  let total = List.length opens in
  List.mapi ~f:(fun i x -> (i, x)) opens
  |> List.map ~f:(fun (i, x) ->
      (match report with
       | `Bar -> oreport ("Analyzing", (100 * (i + 1) / total))
       | `Text -> Stdio.printf "%d/%d\n%!" (i + 1) total
       | `None -> ());
      let* use_names = Typed.Open_uses.compute t x in
      compute_summary (x, use_names)
    )
  (* Filter out failing opens *)
  |> List.filter_map ~f:(function Ok o -> Some o | _ -> None)
  |> Result.return

type args = {
  report : [`Bar | `Text | `None];
  conf_file : string option;
  skip_absent : bool;
  silence_errors : bool
}

let analyse {conf_file; report; skip_absent; _} oreport filename =
  begin
    oreport ("Fetching", 0);
    let conf = Conf.read_conf ?conf_file () in
    let* summaries = get_summaries filename skip_absent report oreport in
    List.iter summaries ~f:(make_decision filename conf);
    Result.return ()
  end |> Result.map_error ~f:(fun s -> Printf.sprintf "-> %s: %s" filename s)

let execute args filenames =
  let total = List.length filenames in
  let bar = Progress_bar.b total in
  let go oreport freport =
    let process_one n filename =
      (match args.report with
       | `Bar -> freport 1
       | `Text -> Stdio.printf "Processing %s (%d/%d)\n%!" filename (n + 1) total
       | `None -> ());
      analyse args oreport filename
    in
    List.mapi ~f:process_one filenames
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
