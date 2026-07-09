(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* vox: a guard-free earlier arm whose pattern is DEEP (a constructor
   with variable/wildcard leaves) contributes an EXISTENTIAL negative to
   the later arms: [not (exists f.., subject = C (..f..))], the leaves
   bound and every constant / nested-constructor position pinned.  Grind
   will not instantiate such a negative under a plain goal, so a VC that
   carries one is discharged by splitting the spec function's match and
   refuting the overlapping model case; earlier arms without a deep
   negative keep the plain [by grind]. *)

type t = A | B of t * int
[%%expect{|
type t = A | B of t * int
|}]

[%%vox.lean {lean|
@[grind, expose] def h : Vox_t -> Int
  | .B (.B _a _m) _n => 1
  | .B _ _ => 2
  | .A => 0
|lean}]
[%%expect{|
|}]

(* The second arm [B (a, n)] leaves [a] free, so the model [h]'s first
   case ([.B (.B ..) ..]) overlaps it; the arm proves [_ = h x] only
   through the first arm's existential negative
   [not (exists f.., x = B (B (f, f), f))] plus the match split. *)
let f (x : t) : int{ _ = h x } =
  match x with
  | B (B (a, m), n) -> 1
  | B (a, n) -> 2
  | A -> 0
[%%expect{|
val f : (x : t) -> int{ _ = h x } = <fun>
|}]
