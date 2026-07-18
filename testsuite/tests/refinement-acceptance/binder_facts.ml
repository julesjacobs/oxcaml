(* TEST
 expect;
*)

(* ============================================================= *)
(* ACCEPTANCE CORPUS: binder-as-fact                              *)
(*                                                                *)
(* plan.html "How checking works": a binder whose type carries a  *)
(* top-level refinement enters the environment at the SKELETON    *)
(* (the variable is used bare); the refined type stays on the     *)
(* pattern, and the post-typedtree verification pass records the  *)
(* instantiated predicate as an assumption in scope. Downstream   *)
(* obligations are discharged against those assumptions.          *)
(*                                                                *)
(* STATUS MARKERS (read by the corpus report + any harness gate).  *)
(* Each case is tagged with one greppable line of the shape         *)
(*   "at-acc id=CASE final=ACCEPT/REJECT today=ACCEPT/REJECT        *)
(*    stable=yes/no unlocks=STEP" (written with an @ in place of    *)
(* "at-"; grepping that tag lists exactly the corpus cases).        *)
(* stable=yes  : today already equals the finished behavior; this *)
(*               [%%expect] block is an anchor and must not drift. *)
(* stable=no   : today is the fail-closed placeholder; the block  *)
(*               MUST change to match `final` when `unlocks` lands *)
(*               (promotion at that stage is the tightening).      *)
(*                                                                *)
(* At base 213d8cb729 (surface syntax + predicate AST + rigid     *)
(* Trefine equality + elaboration; NO typechecker-integration or  *)
(* verification pass) every attempt to put a refinement on a bare *)
(* value is a rigid clash -- the feature fails closed.            *)
(* ============================================================= *)

(* @acc id=bf_use_fact final=ACCEPT today=ACCEPT stable=no unlocks=integration+verification
   A refined let-binder records its predicate as a fact; the same
   predicate is then re-imposed downstream and discharged trivially.
   FINAL: accepts (fact [x = 1] entails the obligation [x = 1]).
   TODAY: accepts via binder-fact collection and verification. *)
let bf_use_fact () =
  let x = (1 : int{ _ = 1 }) in
  (x : int{ _ = 1 })
[%%expect {|
val bf_use_fact : unit -> int{ _ = 1 } = <fun>
|}]

(* @acc id=bf_skeleton_use final=ACCEPT today=ACCEPT stable=no unlocks=integration+verification
   The refined binder is USED at its skeleton: [x + 1] needs [x : int].
   FINAL: accepts (use is skeleton-typed; the binder fact is irrelevant
   to a bare arithmetic use).
   TODAY: accepts. *)
let bf_skeleton_use () =
  let x = (1 : int{ _ = 1 }) in
  x + 1
[%%expect {|
val bf_skeleton_use : unit -> int = <fun>
|}]

(* @acc id=bf_needs_fact final=ACCEPT today=ACCEPT stable=no unlocks=integration+verification
   Correctness DEPENDS on the binder fact: the downstream obligation
   [x > 0] is only provable from the recorded fact [x = 7]. Drop the
   fact and the condition is unprovable -- this is the case that fails
   if binder facts are not carried.
   FINAL and TODAY: accepts. *)
let bf_needs_fact () =
  let x = (7 : int{ _ = 7 }) in
  (x : int{ _ > 0 })
[%%expect {|
val bf_needs_fact : unit -> int{ _ > 0 } = <fun>
|}]

(* @acc id=bf_param_fact final=ACCEPT today=ACCEPT stable=no unlocks=integration+verification
   A function PARAMETER binder contributes its contract as a fact for
   the body: from [n = 5] the result obligation [n > 0] discharges.
   The parameter binds at the skeleton; the arrow domain keeps [int{_=5}].
   FINAL and TODAY: accepts. *)
let bf_param_fact (n : int{ _ = 5 }) : int{ _ > 0 } = n
[%%expect {|
val bf_param_fact : int{ _ = 5 } -> int{ _ > 0 } = <fun>
|}]

(* A stable RHS selfifies without an explicit refinement on the binder.  The
   downstream exact-value obligation is provable only from [x = 7]. *)
let bf_stable_selfification () =
  let x = 7 in
  (x : int{ _ = 7 })
[%%expect {|
val bf_stable_selfification : unit -> int{ _ = 7 } = <fun>
|}]

(* Module-level values and exact aliases retain their equations for later
   structure items. *)
let bf_module_source = 9
let bf_module_alias = bf_module_source
let bf_module_alias_proof = (bf_module_alias : int{ _ = 9 })
[%%expect {|
val bf_module_source : int = 9
val bf_module_alias : int = 9
val bf_module_alias_proof : int{ _ = 9 } = 9
|}]

(* The value returned by this sequence happens to be [1], but the RHS is
   impure and therefore contributes no selfification equation. *)
let bf_no_impure_selfification () =
  let x = (ignore (read_int ()); 1) in
  (x : int{ _ = 1 })
[%%expect {|
Line 3, characters 2-20:
3 |   (x : int{ _ = 1 })
      ^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

type bf_record = { left : int; right : int }

(* Stable record components may still be lowered for an annotation subject. *)
let bf_stable_record_annotation () =
  ({ left = 1; right = 1 } : bf_record{ _.left = _.right })
[%%expect {|
type bf_record = { left : int; right : int; }
val bf_stable_record_annotation : unit -> bf_record{ _.left = _.right } =
  <fun>
|}]

let bf_record_counter = ref 0

let bf_record_next () =
  incr bf_record_counter;
  !bf_record_counter

(* Distinct effectful evaluations must not be identified by subject lowering. *)
let bf_no_impure_record_annotation () =
  ({ left = bf_record_next (); right = bf_record_next () } :
    bf_record{ _.left = _.right })
[%%expect {|
val bf_record_counter : int ref = {contents = 0}
val bf_record_next : unit -> int = <fun>
Line 9, characters 3-58:
9 |   ({ left = bf_record_next (); right = bf_record_next () } :
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed: this expression form cannot yet be represented in a verification condition
|}]
