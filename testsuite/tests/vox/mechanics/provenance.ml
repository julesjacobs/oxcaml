(* TEST
 flags = "-vox-dump-vc-provenance -vox-dry-run";
 expect;
*)

(* vox: [-vox-dump-vc-provenance] is [-dump-vc] plus, on every goal
   and every hypothesis, the source span it originated from, appended
   as "  @ line.col-line.col" (1-based lines, 0-based columns, to
   match the "Line N, characters A-B" header).  The span lets the
   proof-pane editor highlight, on hover over a hypothesis, the
   binder / branch / contract it came from.  Facts synthesized with no
   meaningful span (selfification equalities, mutable-version
   equations) carry no suffix. *)

(* A refined PARAMETER: the contract [_ > 0] becomes a hypothesis
   spanning the binder pattern [x]; the goal spans the checked
   expression [refine_ x]. *)
let use_param (x : int{ _ > 0 }) : int{ _ > 0 } = refine_ x
[%%expect{|
Line 1, characters 58-59: vox VC:
  goal: x > 0  @ 1.58-1.59
  hypotheses:
  x > 0  @ 1.15-1.16
val use_param : int{ _ > 0 } -> int{ _ > 0 } = <fun>
|}]

(* A BRANCH condition: the reflected test [x > 0] (and its negation on
   the else arm) is a hypothesis spanning the condition expression. *)
let branch (x : int) : int{ _ >= 0 } =
  if x > 0 then refine_ x else refine_ 0
[%%expect{|
Line 2, characters 24-25: vox VC:
  goal: x >= 0  @ 2.24-2.25
  hypotheses:
  x > 0  @ 2.5-2.10
Line 2, characters 39-40: vox VC:
  goal: 0 >= 0  @ 2.39-2.40
  hypotheses:
  not (x > 0)  @ 2.5-2.10
val branch : int -> int{ _ >= 0 } = <fun>
|}]

(* A dependent-arrow CONTRACT at a CALL SITE: the caller's obligation
   to satisfy [dep]'s precondition is a goal whose span is the
   argument [5]. *)
let dep (x : int{ _ > 0 }) : int = x
let call () = dep 5
[%%expect{|
val dep : int{ _ > 0 } -> int = <fun>
Line 2, characters 18-19: vox VC:
  goal: 5 > 0  @ 2.18-2.19
  hypotheses: <none>
val call : unit -> int = <fun>
|}]

(* A loop INVARIANT: the [@vox.invariant] formula becomes a hypothesis
   (and the entry/back-edge goals) spanning the whole attribute; the
   for-loop bounds span the loop; the fresh-version equation
   [x@2 = x@1 + 1] is synthesized and carries no span. *)
let loopy (n : int) : int{ _ >= 0 } =
  let mutable x = 0 in
  (for i = 1 to n do
     x <- x + 1
   done) [@vox.invariant x >= 0];
  refine_ x
[%%expect{|
Line 5, characters 9-32: vox VC:
  goal: x >= 0  @ 5.9-5.32
  hypotheses:
  x = 0
Line 5, characters 9-32: vox VC:
  goal: x@2 >= 0  @ 5.9-5.32
  hypotheses:
  1 <= i  @ 3.2-5.8
  i <= n  @ 3.2-5.8
  x@1 >= 0  @ 5.9-5.32
  x@2 = (x@1 + 1)
Line 6, characters 10-11: vox VC:
  goal: x@1 >= 0  @ 6.10-6.11
  hypotheses:
  x@1 >= 0  @ 5.9-5.32
val loopy : int -> int{ _ >= 0 } = <fun>
|}]
