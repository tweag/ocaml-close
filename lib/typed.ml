(*open Base*)

let type_ast = Typemod.type_structure Env.empty

(* can be untyped if needed with untypeast *)

(* TODO: we can use Tast_iterator to find opens in typedtree and directly have
 * the full qualified paths of everything *)

(* This way we could possibly avoid having to use merlin *)

(* However we have to investigate version stability when using this *)
