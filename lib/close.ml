open Base
open Utils

(* Possible TODO: use typerex-lint ? But it's outdated *)

(* Note : usage of ocp-index in one file
ocp-index print -F Dune__exe__Close
   --context=src/close.ml:4,5
   --build=_build/default/src/.close.eobjs/byte 
   whitelist 
*)

(* TODO: support for wildcard *)

let infer_prefix file qualify =
  let* unqualified = Syntactic.get_source_fragment file qualify.start qualify.finish in
  Option.bind
    (String.chop_suffix qualify.content ~suffix:unqualified)
    ~f:(String.chop_suffix ~suffix:".")
  |> Result.of_option ~error:"Couldn't chop prefix when inferring module name"

type open_summary = {
  module_name : string;
  total : int;
  groups : int;
  layer_only : bool;
  imports_syntax : bool;
}

let is_module_id id = Char.is_uppercase id.[0]

let is_operator_id id =
  String.split ~on:'.' id
  |> List.last_exn
  |> String.exists ~f:(Fn.non Char.(fun c ->
      is_alphanum c || c = '_'
    ))

let compute_summary file uses =
  if List.is_empty uses then Result.return None
  else
    let* prefix, uses =
      match infer_prefix file (List.hd_exn uses) with
      | Ok prefix ->
        let uses =
          List.map ~f:(fun s -> s.content) uses
          |> List.map ~f:(String.chop_prefix ~prefix:(prefix ^ "."))
        in
        let* uses =
          if List.exists ~f:Option.is_none uses then
            Result.failf "Couldn't chop inferred prefix on use"
          else Result.return (List.filter_opt uses)
        in
        Result.return (prefix, uses)
      | Error _ ->
        let uses = List.map ~f:(fun s -> s.content) uses in
        Result.return ("NOTINFERRED", uses)
    in
    let total = List.length uses in
    let h = Hashtbl.create (module String) in
    List.iter ~f:(Hashtbl.incr h) uses;
    let groups = Hashtbl.length h in
    let layer_only =
      Hashtbl.keys h
      |> List.for_all ~f:is_module_id
    in
    let imports_syntax = 
      Hashtbl.keys h
      |> List.exists ~f:is_operator_id
    in
    Result.return @@
    Some {module_name = prefix; total;
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
  let rec apply = function
    | And l -> List.map ~f:apply l |> List.for_all ~f:Fn.id
    | Or l -> List.map ~f:apply l |> List.exists ~f:Fn.id
    | Not b -> not (apply b)
    | True -> true
    | False -> false
    | Min_use n -> sum.total >= n
    | Min_exported n -> sum.groups >= n
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
      bar ~style:`UTF8 ~width:(`Fixed 70) ~color:Terminal.Color.(hex "#FA0") total;
      count_to total
    ])
  let bar2 = Line.(list [ 
      const "Opens";
      bar ~style:`UTF8 ~width:(`Fixed 70) ~data:`Latest
        ~color:Terminal.Color.(hex "#5500FF") 100;
      spinner ()
    ])
  let b total = Multi.(line bar2 ++ line (bar1 ~total))
end

let get_summaries filename report oreport =
  let file = Stdio.In_channel.read_lines filename in
  let* () = Merlin.check_errors filename in
  if Poly.(report = `Text) then Stdio.printf "Merlin OK\n%!";
  let* opens = Syntactic.get_opens filename in
  let total = List.length opens in
  let* uses =
    List.mapi ~f:(fun i x -> (i, x)) opens
    |> map_result ~f:(fun (i, x) ->
        (match report with
        | `Bar -> oreport (100 * i / total)
        | `Text -> Stdio.printf "%d/%d\n%!" (i + 1) total
        | `None -> ());
        Merlin.uses_of_open filename x
      ) in
  let* summaries = map_result ~f:(compute_summary file) uses in
  List.filter_opt summaries |> Result.return

type args = {
  report : [`Bar | `Text | `None];
  conf_file : string option;
}

let analyse {conf_file; report} oreport filename =
  begin
    let conf = Conf.read_conf ?conf_file () in
    let* _ = Typed.Extraction.get_typed_tree ~report filename in
    let* summaries = get_summaries filename report oreport in
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
  if Poly.(args.report = `Bar) then
    Progress.with_reporters ~config:Progress_bar.config bar go
  else go ignore ignore
