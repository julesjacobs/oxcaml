(* TEST include stdlib_stable; flags = "-w -220"; expect;
*)

(* ============================================================================
   F2 (lazy force / construction at total) — CLOSED. Secure-behavior anchor.
   ============================================================================

   History: the re-sweep at 34c1e95414 filed F2 as a MUST finding — a [@ total]
   function could force an arbitrary lazy through the [lazy] PATTERN, and a lazy
   built from a diverging/effectful thunk was still classified [total], so a
   total call could diverge or perform I/O. This file was a QUARANTINE anchor
   recording those unsound accepts. The lazy-totality repair (08d7979c46, merged
   at 031643ffda) closes both defects; the [%%expect] blocks below now record the
   SECURE rejections and break loudly if either defect ever regresses.

   The two closed defects, and how the rejection now witnesses each:

   (1) Lazy CONSTRUCTION now inherits body partiality into totality (mode_lazy
       crossing flipped to ~totality:false, mirroring portability). A lazy whose
       thunk diverges (Q1) or has effects (Q2) is itself [partial], so binding it
       at [@ total] rejects — the partiality is attributed to the offending value
       inside the thunk ("loop" / "print_string"). A pure thunk (lazy 0) stays
       total-able; see typing-modes/lazy.ml for the portability mirror.
   (2) The [lazy] PATTERN is now a partial (forcing) operation, matching
       [Lazy.force]: pattern-forcing calls constrain_enclosing_totality, so a
       [@ total] function that matches a [lazy] pattern is constrained partial
       and rejects (Q3), exactly as it does for [Lazy.force] (Q4).

   Together (Q5): [let (f @ total) l = match l with lazy x -> x] no longer even
   typechecks — the exploit is closed at f's definition, so no diverging total
   call can be constructed. *)

(* Q1: a lazy whose forcing diverges is partial, so binding at total rejects. *)
let q1 @ total = lazy (let rec loop () = loop () in loop ())
;;

[%%expect
  {|
Line 1, characters 41-45:
1 | let q1 @ total = lazy (let rec loop () = loop () in loop ())
                                             ^^^^
Error: The value "loop" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 36-48
         which is expected to be "total".
|}]

(* Q2: a lazy whose forcing performs I/O is partial (effectful thunk). *)
let q2 @ total = lazy (print_string "effect"; 0)
;;

[%%expect
  {|
Line 1, characters 23-35:
1 | let q2 @ total = lazy (print_string "effect"; 0)
                           ^^^^^^^^^^^^
Error: The value "print_string" is "partial"
       but is expected to be "total"
         because it is used inside the lazy expression at line 1, characters 17-48
         which is expected to be "total".
|}]

(* Q3: the lazy PATTERN forces, so it makes the enclosing function partial. *)
let (q3 @ total) l = match l with lazy x -> x
;;

[%%expect
  {|
Line 1, characters 34-40:
1 | let (q3 @ total) l = match l with lazy x -> x
                                      ^^^^^^
Error: The function is "partial" but is expected to be "total".
|}]

(* Q4: CONTRAST (sound, unchanged) — Lazy.force IS partial and correctly REJECTS.
   Q3 now behaves like Q4, as the fix intended. *)
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

(* Q5: the former exploit shape. [force_it] no longer typechecks: the lazy
   pattern-force constrains it partial, so the diverging total call can never be
   built. (Before the fix, [force_it] was accepted and [boom ()] diverged at
   total.) *)
let (force_it @ total) l = match l with lazy x -> x
let diverging : int Lazy.t = lazy (let rec loop () = loop () in loop ())
let boom () = force_it diverging
;;

[%%expect
  {|
Line 1, characters 40-46:
1 | let (force_it @ total) l = match l with lazy x -> x
                                            ^^^^^^
Error: The function is "partial" but is expected to be "total".
|}]
