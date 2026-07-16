(* TEST include stdlib_stable; flags = "-w -220"; expect;
*)

(* ============================================================================
   OPEN SOUNDNESS FINDING F2 — lazy-pattern force at total.
   ============================================================================

   Reported to main 2026-07-16 (integrated tree, branch soundness-resweep from
   34c1e95414). This file is a QUARANTINE anchor like the first sweep's F1 file:
   the [%%expect] blocks record the CURRENT (unsound) behavior so the suite
   stays green and re-runnable. When F2 is fixed these expects break loudly —
   flip Q1/Q2/Q3/Q5 to the secure "expected to be total" rejection.

   THE HOLE: a [@ total] function can force an arbitrary lazy through the [lazy]
   PATTERN and thereby diverge or perform I/O at total. Two independent defects,
   either sufficient:

   (1) Lazy CONSTRUCTION does not inherit body partiality into totality: a lazy
       whose forcing diverges / has effects is nonetheless classified [total]
       (Q1, Q2). Contrast: portability IS inherited from the lazy body
       (testsuite/tests/typing-modes/lazy.ml).
   (2) The [lazy] PATTERN forces its scrutinee but is NOT partial (Q3), whereas
       [Lazy.force] correctly IS partial (Q4). The pattern-force is an unguarded
       forcing effect.

   Together (Q5): [let (f @ total) l = match l with lazy x -> x] applied to a
   diverging lazy is a total call that never terminates; with an effectful body
   it performs I/O inside a total call (verified by execution).

   CLEAN FIX POINT: make the [lazy] pattern-match a partial (forcing) operation
   mirroring [Lazy.force] — closes the hole regardless of defect (1). *)

(* Q1: a lazy whose forcing diverges is classified total (should be partial). *)
let q1 @ total = lazy (let rec loop () = loop () in loop ())
;;

[%%expect
  {|
val q1 : 'a lazy_t = <lazy>
|}]

(* Q2: a lazy whose forcing performs I/O is classified total. *)
let q2 @ total = lazy (print_string "effect"; 0)
;;

[%%expect
  {|
val q2 : int lazy_t = <lazy>
|}]

(* Q3: the lazy PATTERN forces but does not make the function partial. *)
let (q3 @ total) l = match l with lazy x -> x
;;

[%%expect
  {|
val q3 : 'a lazy_t -> 'a = <fun>
|}]

(* Q4: CONTRAST (sound) — Lazy.force IS partial, so this correctly REJECTS.
   The fix should make Q3 behave like Q4. *)
let (q4 @ total) l = Lazy.force l
;;

[%%expect
  {|
Line 1, characters 21-31:
1 | let (q4 @ total) l = Lazy.force l
                         ^^^^^^^^^^
Error: The value "Lazy.force" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 17-33
         which is expected to be "total".
|}]

(* Q5: the full exploit shape — a total function that forces its lazy argument,
   applied to a diverging lazy. The application typechecks (accepted); [boom] is
   wrapped in a thunk so the expect runner does not actually force the
   divergence. Calling [boom ()] at runtime is a total call that never
   terminates (confirmed out-of-band by execution). *)
let (force_it @ total) l = match l with lazy x -> x
let diverging : int Lazy.t = lazy (let rec loop () = loop () in loop ())
let boom () = force_it diverging
;;

[%%expect
  {|
val force_it : 'a lazy_t -> 'a = <fun>
val diverging : int Lazy.t = <lazy>
val boom : unit -> int = <fun>
|}]
