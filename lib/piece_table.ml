open Core
open Utils

type piece = {
  is_original : bool;
  start_index : int;
  length : int;
}
[@@deriving sexp_of]

type t = {
  original : string;
  add : Buffer.t;
  table : piece Doubly_linked.t;
}
[@@deriving sexp_of]

let create original =
  let piece = {
    is_original = true;
    start_index = 0;
    length = String.length original
  } in
  let table = Doubly_linked.of_list [piece] in
  {original; add = Buffer.create 16; table}

(* From a logical pos, get the corresponding piece and its corresponding
 * running logic total. If original, then we search as in dealing with the
 * single original piece *)
let piece_of_pos ?(cursor=false) ?(orig_pos=false) pos {table; _} =
  let pieces = Doubly_linked.length table in
  let* n, start =
    if orig_pos then
      Doubly_linked.to_sequence table
      |> Sequence.find_mapi ~f:(fun n piece ->
          if piece.is_original && pos >= piece.start_index
             && pos < piece.start_index + piece.length then
            Some (n, piece.start_index)
          else None
        )
      |> Result.of_option ~error:"Trying to access deleted text"
    else
      Doubly_linked.to_sequence table
      |> Sequence.folding_map ~init:0 ~f:(fun acc piece ->
          (acc + piece.length, (piece, acc))
        )
      |> Sequence.find_mapi ~f:(fun n (piece, start) -> 
          if (start <= pos && pos < start + piece.length)
          || (cursor && n = pieces - 1 && pos = start + piece.length) then
            Some (n, start)
          else None
        )
      |> Result.of_option ~error:"Index not found in piece table"
  in
  let t = Doubly_linked.findi_elt table ~f:(fun n' _ -> n = n') in
  let _, elt = Option.value_exn t in
  Result.return (elt, start)

module Elt = Doubly_linked.Elt

let delete ?orig_pos ~pos t =
  let* elt, start = piece_of_pos ?orig_pos pos t in
  let piece = Elt.value elt in
  let del_index = pos - start in
  if del_index = 0 then
    (* Special case, shrink begin *)
    Elt.set elt {piece with start_index = piece.start_index + 1;
                            length = piece.length - 1}
  else if del_index = piece.length - 1 then
    (* Special case, shrink end *)
    Elt.set elt {piece with length = piece.length - 1}
  else begin
    (* General case *)
    let before = elt in
    Elt.set before {piece with length = del_index};
    let after_piece =
      {piece with start_index = piece.start_index + del_index + 1;
                  length = piece.length - del_index - 1} in
    Doubly_linked.insert_after t.table before after_piece |> ignore
  end;
  Result.return ()

let insert ?orig_pos ~pos what t =
  let* elt, start = piece_of_pos ?orig_pos ~cursor:true pos t in
  let add_index = Buffer.length t.add in
  Buffer.add_string t.add what;
  let add_piece =
    {is_original = false; start_index = add_index;
     length = String.length what}
  in
  let piece = Elt.value elt in
  let add_pivot = pos - start in
  if add_pivot = 0 then
    (* Special case, just add a piece before *)
    Doubly_linked.insert_before t.table elt add_piece |> ignore
  else if add_pivot = piece.length then
    (* Special case, just add a piece after *)
    Doubly_linked.insert_after t.table elt add_piece |> ignore
  else begin
    (* General case *)
    let added = elt in
    Elt.set added add_piece;
    let before_piece = {piece with length = add_pivot} in
    let after_piece = {piece with start_index = piece.start_index + add_pivot;
                                  length = piece.length - add_pivot} in
    Doubly_linked.insert_before t.table added before_piece |> ignore;
    Doubly_linked.insert_after t.table added after_piece |> ignore;
  end;
  Result.return ()

let contents t =
  let buf = Buffer.create 16 in
  let add = Buffer.contents t.add in
  Doubly_linked.iter t.table ~f:(fun {is_original; start_index; length} ->
      let from = if is_original then t.original else add in
      let piece = String.sub from ~pos:start_index ~len:length in
      Buffer.add_string buf piece
    );
  Buffer.contents buf

let delete_many ~pos ~length t =
  for _ = 1 to length do
    delete ~pos t |> ignore
  done

let delete_many_orig ~pos ~length t =
  for i = pos to pos + length - 1 do
    delete ~orig_pos:true ~pos:i t |> ignore
  done

let is_original t = String.equal (contents t) t.original

let print_seq t = Stdio.printf "'%s'\n" (contents t)

let%expect_test "simple deletes" =
  let seq = create "A large span of text" in
  delete ~pos:0 seq |> ignore;
  print_seq seq;
  delete ~pos:18 seq |> ignore;
  print_seq seq;
  delete ~pos:5 seq |> ignore;
  print_seq seq;
  [%expect {|
    ' large span of text'
    ' large span of tex'
    ' larg span of tex' |}]

let%expect_test "simple adds" =
  let seq = create "A large span of text" in
  insert ~pos:20 "Fin" seq |> ignore;
  print_seq seq;
  insert ~pos:0 "Debut" seq |> ignore;
  print_seq seq;
  insert ~pos:4 "Milieu" seq |> ignore;
  print_seq seq;
  [%expect {|
    'A large span of textFin'
    'DebutA large span of textFin'
    'DebuMilieutA large span of textFin' |}]

let%expect_test "crowley" =
  let seq = create "A large span of text" in
  delete_many ~pos:2 ~length:6 seq;
  print_seq seq;
  insert ~pos:10 "English " seq |> ignore;
  print_seq seq;
  [%expect {|
    'A span of text'
    'A span of English text' |}]

let%expect_test "orig" =
  let seq = create "A large span of text" in
  delete_many_orig ~pos:2 ~length:6 seq;
  print_seq seq;
  insert ~orig_pos:true ~pos:16 "English " seq |> ignore;
  print_seq seq;
  insert ~orig_pos:true ~pos:16 "or French " seq |> ignore;
  print_seq seq;
  delete ~orig_pos:true ~pos:18 seq |> ignore;
  print_seq seq;
  delete ~orig_pos:true ~pos:19 seq |> ignore;
  print_seq seq;
  delete_many_orig ~pos:13 ~length:3 seq;
  print_seq seq;
  [%expect {|
    'A span of text'
    'A span of English text'
    'A span of English or French text'
    'A span of English or French tet'
    'A span of English or French te'
    'A span English or French te' |}]
