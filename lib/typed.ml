(*open Base*)

let type_ast s =
  let unstable = Ppxlib_ast.Selected_ast.To_ocaml.copy_structure s in
  Typemod.type_structure Env.empty unstable

(* can be untyped if needed with untypeast *)

(* TODO: we can use Tast_iterator to find opens in typedtree and directly have
 * the full qualified paths of everything *)

(* This way we could possibly avoid having to use merlin *)

(* However we have to investigate version stability when using this *)

(* we have to somehow fill the env with existing libraries :( 
   look into merlin/src/kernel/mtyped.ml possible *)
