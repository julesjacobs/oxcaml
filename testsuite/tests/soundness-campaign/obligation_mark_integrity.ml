(* TEST
 expect;
*)

(* SOUNDNESS CAMPAIGN — Family 3 (obligation-mark integrity + EMISSION).

   Integrated tree: the VC pass is LIVE. Imposing a refined type on a bare
   value now EMITS a verification obligation and discharges it through Lean:
   a false annotation fails "not-proved", a provable one is proved. This
   upgrades the first sweep's pending-VC "mark retention" state — the
   obligation is no longer merely recorded on the type, it is genuinely
   checked, and a bare value can no longer slip into a refined type unchecked.

   O1/O3/O4/O5 feed a bare int into a refined [_ > 0] context: the obligation
   is emitted and FAILS not-proved (a bare int cannot be proved positive) — the
   secure outcome. O2 is a structural [:>] clash (rejected before VC). O6's
   default value is the literal [1] and [1 > 0] is provable, so the obligation
   is DISCHARGED and o6 accepts. No bare value acquires a refinement without an
   emitted-and-satisfied obligation. *)

(* O1: direct annotation of a bare value at a refined type. *)
let o1 (x : int) = (x : int{ _ > 0 })
[%%expect {|
Line 1, characters 19-37:
1 | let o1 (x : int) = (x : int{ _ > 0 })
                       ^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* O2: :> coercion from bare to refined. *)
let o2 (x : int) = (x :> int{ _ > 0 })
[%%expect {|
Line 1, characters 19-38:
1 | let o2 (x : int) = (x :> int{ _ > 0 })
                       ^^^^^^^^^^^^^^^^^^^
Error: Type "int" is not a subtype of "int{ (app[Stdlib!.>] _ 0) }"
|}]

(* O3: refined return contract on a bare-producing function. *)
let o3 (x : int) : int{ _ > 0 } = x
[%%expect {|
Line 1, characters 34-35:
1 | let o3 (x : int) : int{ _ > 0 } = x
                                      ^
Error: Refinement verification failed (not-proved)
|}]

(* O4: launder through a let then annotate. *)
let o4 (x : int) =
  let y = x in
  (y : int{ _ > 0 })
[%%expect {|
Line 3, characters 2-20:
3 |   (y : int{ _ > 0 })
      ^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* O5: refined parameter fed a bare argument (contract obligation site). *)
let takes_pos (x : int{ _ > 0 }) = x
let o5 (n : int) = takes_pos n
[%%expect {|
val takes_pos : int{ (app[Stdlib!.>] _ 0) } -> int = <fun>
Line 2, characters 29-30:
2 | let o5 (n : int) = takes_pos n
                                 ^
Error: Refinement verification failed (not-proved)
|}]

(* O6: optional-argument default is the literal 1; 1 > 0 is provable, so the
   obligation is DISCHARGED and the binding accepts. *)
let opt ?(x = (1 : int{ _ > 0 })) () = x
let o6 = opt ()
[%%expect {|
val opt : ?x:int{ (app[Stdlib!.>] _ 0) } -> unit -> int = <fun>
val o6 : int = 1
|}]
