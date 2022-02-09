open Base
open Utils

(* Purely decorative *)
module Progress_bar = struct
  open Progress
  let config = Config.(v ~persistent:false ())
  let bar1 ~total = Line.(list [ 
      const "Files";
      bar ~style:`UTF8 ~width:(`Fixed 60)
        ~color:Terminal.Color.(hex "#FA0") total;
      count_to total
    ])
  let bar2 = Line.(list [ 
      spinner ();
      string
    ])
  let b total = Multi.(line bar2 ++ line (bar1 ~total))
end

(* GADT version of the command type, to allow functions to return different
 * objects depending on the command *)
type 'a com =
  | Cmd_lint : Patch.t com
  | Cmd_dump : unit com

let analyse (type ty) conf params (com : ty com) filename : ty list res =
  let open Params in
  let error s = 
    Printf.sprintf "-> %s: %s" filename s
  in
  begin
    params.log.change "Fetching";
    let* tree = Analysis.AST.get ~params filename in
    if params.common.print_tree then Analysis.AST.print tree;
    let* summaries = Summary.compute_all tree conf params in
    let f sum : ty res = match com with
      | Cmd_lint ->
        let decision = Decision.compute tree conf sum in
        Decision.print filename sum decision;
        (* Compute and save patch for decision made *)
        let patch = Decision.to_patch filename sum decision in
        Result.return patch
      | Cmd_dump ->
        Stdio.printf "%s\n" (Summary.show sum);
        Result.return ()
    in
    map_result ~f summaries
  end |> Result.map_error ~f:error

(* Process a single file, returning the computed patche or nothing *)
let one_file (type ty) params (com : ty com) filename : ty res = 
  let open Params in
  params.log.new_file filename;
  let conf = params.conf filename in
  match com with
  | Cmd_lint ->
    let* patches = analyse conf params Cmd_lint filename in
    let non_empty = List.filter patches ~f:(Fn.non Patch.is_empty) in
    let patch =
      (* Merge all open patches into a single one *)
      if List.is_empty non_empty then Patch.empty filename
      else if conf.single && List.length patches = 1 then (
        Stdio.printf
          "\027[34mModifications in %s are ignored, \
           since there is no ambiguity.\n\027[0m"
          filename;
        Patch.empty filename
      )
      else
        List.reduce_exn patches ~f:Patch.merge
    in
    (* Flush buffer if running in parallel *)
    if params.common.parallel then Stdio.printf "%!";
    Result.return patch
  | Cmd_dump ->
    analyse conf params Cmd_dump filename |> ignore;
    Result.return ()

type outcome = Nothing_to_do | Patches_needed | Failed

let execute args filenames =
  let* filenames =
    if List.is_empty filenames then (
      Stdio.printf "Analyzing all .ml files known by dune...\n";
      Dune.all_ml_files () |> filter_errors
    )
    else Result.return filenames
  in
  let open Params in
  let total = List.length filenames in
  let bar = Progress_bar.b total in
  let go oreport freport =
    (* Prepare arguments *)
    let params = Params.of_cli args ((oreport, freport), total) in
    params.log.change "Starting";
    (* What to do with all files, depending on the command *)
    let gather =
      match params.common.command with
      | `Lint ->
        let patches =
          if params.common.parallel then
            Parmap.parmap (one_file params Cmd_lint) (L filenames)
          else
            List.map ~f:(one_file params Cmd_lint) filenames
        in
        let* patches =
          if args.common.silence_errors then
            List.filter_map patches ~f:(function
                | Ok x -> Some x
                | Error _ -> None
              ) |> Result.return
          else
            Result.combine_errors patches
        in
        let patches = List.filter ~f:(Fn.non Patch.is_empty) patches in
        if List.is_empty patches then (
          Stdio.printf "\027[92mNo modification suggested. All good!\n\027[0m";
          Result.return Nothing_to_do
        )
        else (
          Patch.exports patches params.patch_file;
          let prog_name = (Sys.get_argv ()).(0) in
          Stdio.printf
            "\027[33mModifications are needed. Run '\027[0m%s \
             patch %s\027[33m' to apply them.\n\027[0m"
            prog_name params.patch_file;
          Result.return Patches_needed
        )
      | `Dump ->
        let* () = List.map filenames ~f:(one_file params Cmd_dump)
                  |> Result.combine_errors |> Result.map ~f:ignore
        in Result.return Nothing_to_do
    in
    (* Process errors, correctly formatting them *)
    let* with_combined_errors = Result.map_error gather ~f:(fun err_list ->
        let errors = String.concat ~sep:"\n" err_list in
        Printf.sprintf "There were errors during processing:\n%s" errors
      )
    |> filter_errors
    in 
    Result.return with_combined_errors
  in
  (* Launch the general program with reporting functions *)
  match begin
    if Poly.(args.report = `Bar) && not args.common.parallel then
      Progress.with_reporters ~config:Progress_bar.config bar go
    else go ignore ignore
  end with
  | Error _ when args.common.silence_errors -> Result.return Failed
  | s -> s
