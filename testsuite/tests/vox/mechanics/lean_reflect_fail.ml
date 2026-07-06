(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* vox: reflected (total_) definitions that must FAIL.  The positive
   twins are demo/lean_reflect.ml and demo/lean_fib.ml; these probes
   check that reflection cannot smuggle in an inconsistent or
   non-terminating definition (either would prove anything), and that
   a false spec about a sound reflected function still fails. *)

type ilist =
  | Nil
  | Cons of int * ilist
[%%expect{|
type ilist = Nil | Cons of int * ilist
|}]

(* A sound reflected definition... *)
let rec total_ len l =
  match l with
  | Nil -> 0
  | Cons (_, t) -> 1 + len t
[%%expect{|
val len : ilist -> int = <fun>
|}]

(* ...does not prove a false spec about itself. *)
let bad : ilist{ len _ = 1 } = refine_ Nil
[%%expect{|
Line 1, characters 39-42:
1 | let bad : ilist{ len _ = 1 } = refine_ Nil
                                           ^^^
Error: vox: verification failed -- goal DISPROVED (a counterexample was validated).
       Goal: len Nil = 1
Hypotheses: <none>
The goal is false unconditionally.
|}]

(* An inconsistent definition must be rejected at its binding: if it
   were admitted, [f x = f x + 1] would prove [0 = 1]. *)
let rec total_ f (x : int) = f x + 1
[%%expect{|
Line 1, characters 0-36:
1 | let rec total_ f (x : int) = f x + 1
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: vox: the reflected definition of f was rejected by the solver (is it terminating?  int-indexed recursion needs a [@vox.decreases] metric)
(lean: error: fail to show termination for)
|}]

(* A non-terminating definition must be rejected: divergence admits
   any equation.  ([spin] recurses at the same [x], so no metric can
   rescue it; the [if] pins the result sort to int.) *)
let rec total_ spin (x : int) = if x = 0 then 1 else spin x
[%%expect{|
Line 1, characters 0-59:
1 | let rec total_ spin (x : int) = if x = 0 then 1 else spin x
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: vox: the reflected definition of spin was rejected by the solver (is it terminating?  int-indexed recursion needs a [@vox.decreases] metric)
(lean: error: fail to show termination for)
|}]

(* A bogus decreases metric must not rescue a non-structural
   recursion: [n] does not decrease toward the guard. *)
let rec total_ up (n : int) = if n = 0 then 0 else up (n + 1)
[@@vox.decreases n]
[%%expect{|
Lines 1-2, characters 0-19:
1 | let rec total_ up (n : int) = if n = 0 then 0 else up (n + 1)
2 | [@@vox.decreases n]
Error: vox: the reflected definition of up was rejected by the solver (is it terminating?  int-indexed recursion needs a [@vox.decreases] metric)
(lean: error: `grind` failed)
|}]
