(* TEST include stdlib_stable; flags = "-w -220"; expect;
*)

(* ============================================================================
   F1 CLOSURE — totality late-inference (snapshot) hole is FIXED.
   ============================================================================

   First sweep (pre-v2) reported F1: the totality "residue" restrictions
   (while/for/mutable-construction/try/local-exception/lazy) were enforced by a
   SNAPSHOT check [reject_in_total_context], which fired only if the enclosing
   closure's totality was ALREADY pinned to Total. While the mode was still an
   unresolved inference variable — the normal case for an unannotated let-bound
   closure fixed LATER by use — the check saw [None] and silently allowed the
   construct; a subsequent use at [total] then pinned the variable with nothing
   having constrained it to partial. M1 (while) and M2 (for) yielded a [total]
   value that never terminates.

   On the integrated (v2-repair) tree the snapshot is REPLACED by a submode
   CONSTRAINT: the genuinely-partial constructs call
   [constrain_enclosing_totality] (typing/typecore.ml:688, via
   Env.constrain_enclosing_totality_partial) which forces the enclosing
   closure's totality variable to partial. A later demand for [total] then fails
   by ordinary submoding — no momentary boolean. M1-M4 below now REJECT with the
   constraint-path text "This value is partial but is expected to be total".

   RESIDUE SET (verified against the constrain call sites): while (8127), for
   (8156), mutable-record (6881), mutable-array literal / mutable-array
   comprehension (7932 / 12347), overwrite (8955), letmutable (7294), and an
   explicit [@ partial] on a function literal (8189 / 8231). try/with, local
   [exception], and the [lazy] LITERAL are DELIBERATELY NOT in this set: their
   partiality (if any) comes entirely from their CONTENTS — a captured partial
   value (raise / Lazy.force / ref) or a nested residue construct — which the
   closure-lock / capture path already handles. So the pure forms M5-M7 stay
   total (sound: they denote terminating, effect-free values), while every
   effectful/divergent variant M5b/M6b/M7b is repelled. This narrowing is a
   soundness IMPROVEMENT over the pre-v2 blanket syntactic residue.

   NOTE: a separate lazy hole (the lazy PATTERN forces without being partial)
   is filed as F2 in lazy_force_at_total.ml — it is NOT about the lazy literal
   here. ============================================================================ *)

let expects_total (f @ total) = f

(* M1: while -> now REJECTED via the constraint path (was the F1 divergence). *)
let m1 =
  let bad () =
    while true do
      ()
    done
  in
  expects_total bad
;;

[%%expect
  {|
val expects_total : 'a @ total -> 'a = <fun>
Line 10, characters 16-19:
10 |   expects_total bad
                     ^^^
Error: This value is "partial" but is expected to be "total".
|}]

(* M2: for -> now REJECTED via the constraint path. *)
let m2 =
  let bad () =
    for _ = 0 to 1 do
      ()
    done
  in
  expects_total bad
;;

[%%expect
  {|
Line 7, characters 16-19:
7 |   expects_total bad
                    ^^^
Error: This value is "partial" but is expected to be "total".
|}]

(* M3: mutable-record literal -> now REJECTED (mutable label constrains). *)
type r = { mutable m : int }

let m3 =
  let bad x = { m = x } in
  expects_total bad
;;

[%%expect
  {|
type r = { mutable m : int; }
Line 5, characters 16-19:
5 |   expects_total bad
                    ^^^
Error: This value is "partial" but is expected to be "total".
|}]

(* M4: mutable array literal -> now REJECTED (Mutable array constrains). *)
let m4 =
  let bad x = [| x |] in
  expects_total bad
;;

[%%expect
  {|
Line 3, characters 16-19:
3 |   expects_total bad
                    ^^^
Error: This value is "partial" but is expected to be "total".
|}]

(* M5: pure try/with -> total. SOUND: the body [0] terminates with no effect;
   try/with is not residue (partiality would come from the body's contents). *)
let m5 =
  let bad () =
    try 0 with
    | _ -> 1
  in
  expects_total bad
;;

[%%expect
  {|
val m5 : unit -> int = <fun>
|}]

(* M5b: divergence inside the try body IS repelled (the while constrains). *)
let m5b =
  let bad () =
    try
      while true do
        ()
      done
    with
    | _ -> ()
  in
  expects_total bad
;;

[%%expect
  {|
Line 10, characters 16-19:
10 |   expects_total bad
                     ^^^
Error: This value is "partial" but is expected to be "total".
|}]

(* M6: unused local exception -> total. SOUND: [let exception E in 0]
   terminates with no effect; the decl is dead. *)
let m6 =
  let bad () =
    let exception E in
    0
  in
  expects_total bad
;;

[%%expect
  {|
val m6 : unit -> int = <fun>
|}]

(* M6b: raising the local exception IS repelled (raise is a captured partial). *)
let m6b =
  let bad () =
    let exception E in
    raise E
  in
  expects_total bad
;;

[%%expect
  {|
Line 6, characters 16-19:
6 |   expects_total bad
                    ^^^
Error: This value is "partial"
         because it closes over the value "raise" at line 4, characters 4-9
         which is "partial".
       However, the highlighted expression is expected to be "total".
|}]

(* M7: lazy literal of a total body -> total. SOUND: constructing the thunk is
   pure allocation; forcing must go through partial Lazy.force (see T2) or the
   lazy pattern (F2, filed separately). *)
let m7 =
  let bad () = lazy 0 in
  expects_total bad
;;

[%%expect
  {|
val m7 : unit -> int lazy_t = <fun>
|}]

(* M7b: divergence inside the lazy body IS repelled (the while constrains). *)
let m7b =
  let bad () =
    lazy
      (while true do
         ()
       done)
  in
  expects_total bad
;;

[%%expect
  {|
Line 8, characters 16-19:
8 |   expects_total bad
                    ^^^
Error: This value is "partial" but is expected to be "total".
|}]

(* M8: assert -> REJECTED (desugars to a captured raise via (=)); anchors that
   the fix does not regress the capture-path cases. *)
let m8 =
  let bad x = assert (x = 0) in
  expects_total bad
;;

[%%expect
  {|
Line 3, characters 16-19:
3 |   expects_total bad
                    ^^^
Error: This value is "partial"
         because it closes over the value "(=)" at line 2, characters 24-25
         which is "partial".
       However, the highlighted expression is expected to be "total".
|}]
