(* TEST
 flags = "-vox-explain-proofs -vox-dump-vc-provenance";
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* vox: [-vox-explain-proofs] names the lemmas [grind] used per VC.  A VC
   closed by a theorem from an embedded [%%vox.lean] block -- which fires
   AMBIENTLY (it is [@[grind]]-attributed, not passed as a hypothesis) --
   is reported by [grind] as an equational fact ("[= f_id]"), NOT as a
   user fact ("[usr ...]").  Both are named here: the "used:" line shows
   [f_id], never "<arithmetic>".  (A goal that only unfolds the reflected
   definition names it too, e.g. [f]; only genuinely arithmetic/logic
   proofs show "<arithmetic>".) *)

let rec total_ f (n : int) : int =
  if n <= 0 then 0 else 1 + f (n - 1)
[@@vox.decreases n]

[%%vox.lean {lean|
@[grind =] theorem f_id (n : Int) (h : 0 <= n) : f n = n := by
  fun_induction f n <;> grind
|lean}]

(* [f (2 * k)] cannot be reduced by unfolding at a symbolic argument, so
   the ambient block theorem [f_id] must fire: "used: f_id". *)
let use_id (k : int{ _ >= 0 }) : int{ _ = 2 * k } = refine_ (f (2 * k))

(* Pure arithmetic, no user fact: "used: <arithmetic>". *)
let arith (x : int{ _ > 0 }) : int{ _ >= 0 } = refine_ (x + 1)
