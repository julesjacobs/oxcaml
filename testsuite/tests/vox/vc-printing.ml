(* TEST
 flags = "-extension let_mutable -drefinements";
 expect;
*)

(* Vox VC generation: SMT-LIB baselines through the printing backend
   (design-docs/vc-generation.md, Tests).

   A compact set — one fixture per lowering shape — whose GREEN expected
   output contains the Prove scripts byte for byte: declare-consts, :named
   hypotheses, the bitvec operators.  Printing shares the z3 renderer, so
   these baselines are the bytes z3 receives; vc-z3.ml holds the full corpus
   with the verdicts a live solver returns for the same shapes.

   RED state: the vox driver flags do not exist yet, so this runs as a plain
   expect test with the -drefinements probe.  Every fixture compiles with its
   obligations recorded — the probe lines are the obligation map — and
   nothing is discharged: no query bytes appear anywhere in this file.
   GREEN adds -vox-backend printing to this TEST block and re-promotes, so
   the RED-to-GREEN diff of the expectations is exactly the emitted SMT-LIB.
   Dump mode does not refuse units, so every val line below survives GREEN
   with the queries printed alongside it. *)

(* --- proved-const: constant goal, empty hypothesis list ----------------- *)

let v : int{ _ > 0 } = 5;;
[%%expect{|
Line 1, characters 4-5: refined environment entry: v : int{ _ > 0 }
Line 1, characters 23-24: refinement obligation: int{ _ > 0 }
val v : int{ _ > 0 } = 5
|}]

(* --- arrow-domain: instantiation against an apply-arrow domain ---------- *)

let f1 : int{ _ > 0 } -> int = fun y -> y;;
[%%expect{|
val f1 : int{ _ > 0 } -> int = <fun>
|}]

let arrow_domain = f1 5;;
[%%expect{|
val arrow_domain : int = 5
|}]

(* --- dedup-annotated-arg: exactly one query for marker + arrow domain --- *)

let dedup_annotated_arg = f1 (5 : int{ _ > 0 });;
[%%expect{|
Line 1, characters 30-31: refinement obligation: int{ _ > 0 }
val dedup_annotated_arg : int = 5
|}]

(* --- dedup-ignore: exactly one query for the %ignore path ---------------- *)

external drop : int{ _ > 0 } -> unit = "%ignore";;
[%%expect{|
external drop : int{ _ > 0 } -> unit = "%ignore"
|}]

let dedup_ignore = drop 0;;
[%%expect{|
Line 1, characters 24-25: refinement obligation: int{ _ > 0 }
val dedup_ignore : unit = ()
|}]

(* --- fact-binder-and-path: hypotheses and bitvec operators --------------- *)
(* The query for the recursive call shows the binder fact y > 0 and the
   else-branch path condition as :named hypotheses over Bitvec 63. *)

let rec fact (y : int{ _ > 0 }) : int =
  if y <= 1 then 1 else y * fact (y - 1);;
[%%expect{|
val fact : int{ _ > 0 } -> int = <fun>
|}]

(* --- ident-fact: the value-description hypothesis ------------------------ *)

let ident_fact : int{ _ > 0 } list = [5; v];;
[%%expect{|
Line 1, characters 38-39: refinement obligation: int{ _ > 0 }
Line 1, characters 41-42: refinement obligation: int{ _ > 0 }
val ident_fact : int{ _ > 0 } list = [5; 5]
|}]

(* --- push-to-arms / match-push: one obligation, one query per arm -------- *)

let push_to_arms c : int{ _ > 0 } = if c then 1 else 2;;
[%%expect{|
Line 1, characters 36-54: refinement obligation: int{ _ > 0 }
val push_to_arms : bool -> int{ _ > 0 } = <fun>
|}]

let match_push (c : bool) : int{ _ > 0 } =
  match c with true -> 1 | false -> 2;;
[%%expect{|
Line 2, characters 2-37: refinement obligation: int{ _ > 0 }
val match_push : bool -> int{ _ > 0 } = <fun>
|}]

(* --- short-circuit: the && left-operand hypothesis ------------------------ *)

let short_circuit x = x > 0 && f1 x > 0;;
[%%expect{|
val short_circuit : int -> bool = <fun>
|}]

(* --- field-fact: the label-description hypothesis ------------------------- *)

type box = { first_pos : int{ _ > 0 }; second : int };;
[%%expect{|
type box = { first_pos : int{ _ > 0 }; second : int; }
|}]

let field_fact (b : box) : int{ _ > 0 } = b.first_pos;;
[%%expect{|
Line 1, characters 42-53: refinement obligation: int{ _ > 0 }
val field_fact : box -> int{ _ > 0 } = <fun>
|}]

(* --- stability-mutable-arg: abstraction, not Call, for int ref args ------- *)
(* The two reads_param calls must print as two distinct opaque constants,
   never one uninterpreted Call symbol. *)

let reads_param @ total = fun (r : int ref) -> r.contents;;
[%%expect{|
val reads_param : int ref -> int = <fun>
|}]

let stability_mutable_arg (r : int ref) : int{ _ = 0 } =
  let a = reads_param r in
  r.contents <- a + 1;
  reads_param r - a;;
[%%expect{|
Lines 2-4, characters 2-19: refinement obligation: int{ _ = 0 }
val stability_mutable_arg : int ref -> int{ _ = 0 } = <fun>
|}]

(* --- poly-instances: two ground declarations for one polymorphic total --- *)

let id @ total = fun a -> a;;
[%%expect{|
val id : 'a -> 'a = <fun>
|}]

let poly_instances : int{ _ > 0 } = if id true then id 5 else 1;;
[%%expect{|
Line 1, characters 4-18: refined environment entry: poly_instances :
  int{ _ > 0 }
Line 1, characters 36-63: refinement obligation: int{ _ > 0 }
val poly_instances : int{ _ > 0 } = 5
|}]

(* --- shadowed-local: stamped identity keeps two fs apart ------------------ *)

let shadowed_local (x : int{ _ > 0 }) : int{ _ > 0 } =
  let f @ total = fun a -> a in
  let u = f x in
  let f @ total = fun a -> a + 1 in
  let w = f x in
  if u < w then x else 1;;
[%%expect{|
Lines 2-6, characters 2-24: refinement obligation: int{ _ > 0 }
val shadowed_local : int{ _ > 0 } -> int{ _ > 0 } = <fun>
|}]

(* --- shift-bounds: the guarded shift lowering ------------------------------ *)
(* The queries show Ite (0 <= n && n <= 62, Bv_shl x n, c) with c opaque. *)

let shift_in_range : int{ _ > 0 } = 1 lsl 61;;
[%%expect{|
Line 1, characters 4-18: refined environment entry: shift_in_range :
  int{ _ > 0 }
Line 1, characters 36-44: refinement obligation: int{ _ > 0 }
val shift_in_range : int{ _ > 0 } = 2305843009213693952
|}]

let shift_boundary (n : int{ _ = 62 }) : int{ _ < 0 } = 1 lsl n;;
[%%expect{|
Line 1, characters 56-63: refinement obligation: int{ _ < 0 }
val shift_boundary : int{ _ = 62 } -> int{ _ < 0 } = <fun>
|}]

let shift_over (n : int{ _ = 63 }) : int{ _ = 0 } = 1 lsl n;;
[%%expect{|
Line 1, characters 52-59: refinement obligation: int{ _ = 0 }
val shift_over : int{ _ = 63 } -> int{ _ = 0 } = <fun>
|}]

let shift_negative (n : int{ _ = -1 }) : int{ _ = 0 } = 1 lsl n;;
[%%expect{|
Line 1, characters 56-63: refinement obligation: int{ _ = 0 }
val shift_negative : int{ _ = (-1) } -> int{ _ = 0 } = <fun>
|}]

let shift_lsr (x : int{ _ > 0 }) : int{ _ >= 0 } = x lsr 1;;
[%%expect{|
Line 1, characters 51-58: refinement obligation: int{ _ >= 0 }
val shift_lsr : int{ _ > 0 } -> int{ _ >= 0 } = <fun>
|}]

let shift_asr (x : int{ _ > 0 }) : int{ _ >= 0 } = x asr 1;;
[%%expect{|
Line 1, characters 51-58: refinement obligation: int{ _ >= 0 }
val shift_asr : int{ _ > 0 } -> int{ _ >= 0 } = <fun>
|}]

(* --- let-equality-opaque: codomain fact + equality over an opaque const --- *)

let g : unit -> int{ _ > 0 } = fun () -> 5;;
[%%expect{|
Line 1, characters 41-42: refinement obligation: int{ _ > 0 }
val g : unit -> int{ _ > 0 } = <fun>
|}]

let let_equality_opaque = let x = g () in (x : int{ _ > 0 });;
[%%expect{|
Line 1, characters 43-44: refinement obligation: int{ _ > 0 }
val let_equality_opaque : int = 5
|}]

(* --- mutable-fact / mutvar-reads-distinct: per-read constants -------------- *)

let mutable_fact () : int{ _ > 0 } =
  let mutable x : int{ _ > 0 } = 5 in
  x <- 6;
  x;;
[%%expect{|
Lines 2-4, characters 2-3: refinement obligation: int{ _ > 0 }
Line 2, characters 14-15: refined environment entry: x : int{ _ > 0 }
Line 2, characters 33-34: refinement obligation: int{ _ > 0 }
Line 3, characters 7-8: refinement obligation: int{ _ > 0 }
val mutable_fact : unit -> int{ _ > 0 } = <fun>
|}]

let mutvar_reads_distinct () : int{ _ = 0 } =
  let mutable x : int{ _ > 0 } = 1 in
  ((x <- x + 1); x) - x;;
[%%expect{|
Lines 2-3, characters 2-23: refinement obligation: int{ _ = 0 }
Line 2, characters 14-15: refined environment entry: x : int{ _ > 0 }
Line 2, characters 33-34: refinement obligation: int{ _ > 0 }
Line 3, characters 9-14: refinement obligation: int{ _ > 0 }
val mutvar_reads_distinct : unit -> int{ _ = 0 } = <fun>
|}]

(* --- refuted-const / unknown-opaque / bitvec-wrap: the non-green shapes ---- *)
(* Dump mode does not refuse: these print their queries and keep their val
   lines; vc-z3.ml pins their verdicts. *)

let refuted_const : int{ _ > 0 } = 0;;
[%%expect{|
Line 1, characters 4-17: refined environment entry: refuted_const :
  int{ _ > 0 }
Line 1, characters 35-36: refinement obligation: int{ _ > 0 }
val refuted_const : int{ _ > 0 } = 0
|}]

let unknown_opaque (h : unit -> int) : int{ _ >= 0 } = h ();;
[%%expect{|
Line 1, characters 55-59: refinement obligation: int{ _ >= 0 }
val unknown_opaque : (unit -> int) -> int{ _ >= 0 } = <fun>
|}]

let bitvec_wrap (x : int{ _ >= 0 }) : int{ _ >= 0 } = x + 1;;
[%%expect{|
Line 1, characters 54-59: refinement obligation: int{ _ >= 0 }
val bitvec_wrap : int{ _ >= 0 } -> int{ _ >= 0 } = <fun>
|}]

(* --- tuple-datatype: a datatype declaration in the query ------------------- *)

let tuple_datatype : (int * int){ match _ with (a, b) -> a > b } = (2, 1);;
[%%expect{|
Line 1, characters 4-18: refined environment entry: tuple_datatype :
  (int * int){ match _ with | (a, b) -> a > b }
Line 1, characters 67-73: refinement obligation:
  (int * int){ match _ with | (a, b) -> a > b }
val tuple_datatype : (int * int){ match _ with | (a, b) -> a > b } = (2, 1)
|}]

(* --- sealed-datatype: concrete inside, uninterpreted sort outside ---------- *)

module Sealed : sig
  type t
  val mk : int -> t
  val sd_in : t{ true }
end = struct
  type t = { field : int }
  let mk field = { field }
  let sd_in : t{ true } = mk 1
end;;
[%%expect{|
Line 8, characters 6-11: refined environment entry: sd_in : t{ true }
Line 8, characters 26-30: refinement obligation: t{ true }
module Sealed : sig type t val mk : int -> t val sd_in : t{ true } end
|}]

let sd_out : Sealed.t{ true } = Sealed.mk 2;;
[%%expect{|
Line 1, characters 4-10: refined environment entry: sd_out : Sealed.t{ true }
Line 1, characters 32-43: refinement obligation: Sealed.t{ true }
val sd_out : Sealed.t{ true } = <abstr>
|}]
