(* TEST
 expect;
*)

(* ============================================================= *)
(* ACCEPTANCE CORPUS: integer model (ACCEPTED KNOWN LIMITATION)    *)
(*                                                                *)
(* The Lean backend maps OxCaml [int] to Lean [Int] and emits     *)
(* [+]/[-]/[*] as UNBOUNDED integer operations.  OxCaml [int] is  *)
(* 63-bit and wraps, so an arithmetic refinement that is true over *)
(* the mathematical integers but false under wraparound is proved  *)
(* although false at runtime.  This is a live end-to-end           *)
(* unsoundness now that the verification pass is wired in.         *)
(*                                                                *)
(* By user ruling (Q-002) the unbounded-Int model STAYS for now,   *)
(* as a KNOWN, DOCUMENTED, ACCEPTED soundness hole (plan.html,     *)
(* "Known accepted holes").  There is no model change to make.     *)
(* The case below is an ANCHOR recording the current unsound       *)
(* ACCEPT, so that if the model is later fixed (bounded ints, or   *)
(* an overflow side condition) this [%%expect] flips LOUDLY to a   *)
(* rejection and the change cannot land silently.                  *)
(*                                                                 *)
(* Runtime truth (OCaml 5.4): max_int + 1 = min_int, so            *)
(* [max_int + 1 > max_int] is FALSE; the accept below is unsound.  *)
(* ============================================================= *)

(* @acc id=int_overflow_unsound final=REJECT today=ACCEPT stable=no unlocks=integer-model
   ACCEPTED unsound ACCEPT under the unbounded-Int model: proves although
   [max_int + 1 > max_int] is false at runtime.  Flips to REJECT if the
   integer model is ever changed. *)
let int_overflow_unsound = (max_int + 1 : int{ _ > max_int })
[%%expect {|
val int_overflow_unsound : int{ _ > max_int } = -4611686018427387904
|}]
