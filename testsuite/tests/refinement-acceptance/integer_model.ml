(* TEST
 expect;
*)

(* ============================================================= *)
(* ACCEPTANCE CORPUS: signed 63-bit integer model                 *)
(*                                                                *)
(* Ordinary [int] arithmetic is modular over 63-bit words and      *)
(* comparisons use signed order.  [Bigint.t] remains the separate  *)
(* unbounded mathematical-integer sort.                            *)
(* ============================================================= *)

(* This old unbounded-integer consequence is false at runtime and must be
   rejected by the BV63 model. *)
let int_overflow_unsound = (max_int + 1 : int{ _ > max_int })
[%%expect {|
Line 1, characters 27-61:
1 | let int_overflow_unsound = (max_int + 1 : int{ _ > max_int })
                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]
