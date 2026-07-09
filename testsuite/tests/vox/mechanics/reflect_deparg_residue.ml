(* TEST
 flags = "-vox-dry-run";
 expect;
*)

(* Task #53 RESIDUE, now closed by nested-refined-expression support: an
   argument that is neither reflectable nor a call with an exact result
   contract is named by a synthetic loc-keyed ident (logical ANF), so it no
   longer needs a [let] workaround.  [opaque] carries no result refinement, so
   the name's fact is dropped (sound); [g] type-checks with its dependency on
   the anonymous value.  Fact threading for a refined result is exercised in
   nested_refined.ml / lean_nested.ml. *)

let opaque (n : int) : int = n + 1
let f : (n : int) -> int{ _ = n } = fun n -> n
let g (x : int) : int = f (opaque x)
[%%expect{|
val opaque : int -> int = <fun>
val f : (n : int) -> int{ _ = n } = <fun>
val g : int -> int = <fun>
|}]
