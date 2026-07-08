(* TEST
 flags = "-dump-vc -vox-dry-run";
 expect;
*)

(* vox: nested refined expressions without a let-bind.  An argument in a
   refined/dependent position that is not itself nameable (a call with a
   non-exact result contract, or a chain of such calls) is named by a
   loc-keyed synthetic ident [*arg*] -- logical ANF, the [let n = a in ..]
   the user would otherwise write.  The argument's own result refinement
   threads through as a hypothesis; the type checker's dependent-arrow opening
   and this walker share the memo, so the same name flows into the callee's
   result type. *)

let g (x : int) : int{ _ <= x } = refine_ x
[%%expect{|
Line 1, characters 42-43: vox VC:
  goal: x <= x
  hypotheses: <none>
val g : (x : int) -> int{ _ <= x } = <fun>
|}]

let h (x : int) : int{ _ = x + 1 } = refine_ (x + 1)
[%%expect{|
Line 1, characters 45-52: vox VC:
  goal: x + 1 = x + 1
  hypotheses: <none>
val h : (x : int) -> int{ _ = x + 1 } = <fun>
|}]

(* PRECONDITION-only refined parameter (no binder in the result): the nested
   call's postcondition [*arg* <= 10] is threaded as a hypothesis. *)
let consume (y : int{ y <= 10 }) : int = (y :> int)
[%%expect{|
val consume : int{ _ <= 10 } -> int = <fun>
|}]

let use_precond () : int = consume (g 10)
[%%expect{|
Line 1, characters 35-41: vox VC:
  goal: *arg* <= 10
  hypotheses:
  *arg* <= 10
val use_precond : unit -> int = <fun>
|}]

(* An EXACT-result call is still named by its own contract (tier 2), no
   [*arg*]: [h 8]'s value is [8 + 1]. *)
let use_exact () : int = consume (h 8)
[%%expect{|
Line 1, characters 33-38: vox VC:
  goal: 8 + 1 <= 10
  hypotheses: <none>
val use_exact : unit -> int = <fun>
|}]

(* DEPENDENT-result parameter (binder in the callee's result): opening
   substitutes the same [*arg*] into [fd]'s result [_ = *arg* + 1], and the
   postcondition [*arg* <= 10] threads. *)
let fd (y : int) : int{ _ = y + 1 } = refine_ (y + 1)
[%%expect{|
Line 1, characters 46-53: vox VC:
  goal: y + 1 = y + 1
  hypotheses: <none>
val fd : (y : int) -> int{ _ = y + 1 } = <fun>
|}]

let use_dep () : int{ _ <= 11 } = fd (g 10)
[%%expect{|
Line 1, characters 34-43: vox VC:
  goal: *unknown7* <= 11
  hypotheses:
  *arg* <= 10
  *unknown7* = *arg* + 1
val use_dep : unit -> int{ _ <= 11 } = <fun>
|}]

(* CHAIN [consume (g (g 10))]: each call gets its own [*arg*]; the inner
   name's fact is established while walking the outer argument and carried out,
   so the transitive chain [*arg* <= *arg*#2 <= 10] closes the goal. *)
let use_chain () : int = consume (g (g 10))
[%%expect{|
Line 1, characters 33-43: vox VC:
  goal: *arg* <= 10
  hypotheses:
  *arg* <= *arg*#2
  *arg*#2 <= 10
val use_chain : unit -> int = <fun>
|}]
