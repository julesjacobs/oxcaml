(* TEST
 flags = "-extension let_mutable -drefinements";
 expect;
*)

(* Vox VC generation: the full verdict corpus, discharged through the z3
   backend (design-docs/vc-generation.md, Tests).

   RED state: the vox driver flags do not exist yet, so this runs as a plain
   expect test with the -drefinements probe.  Every fixture compiles with its
   obligations recorded — the probe lines below are the obligation map — and
   nothing is discharged: no verdict, no refusal, no located verification
   error appears anywhere in this file.  GREEN adds the z3 gate stanza
   (script has_z3.sh, skip on exit 125, per vox-solver/z3_backend.ml) and
   -vox-backend z3 to this TEST block and re-promotes, so the RED-to-GREEN
   diff of the expectations is exactly the set of verdicts a live solver
   returns: located failure reports for the Refuted/Unknown fixtures and the
   located rejections, and unchanged val lines for the Proved ones. *)

(* --- proved-const: the end-to-end spine -------------------------------- *)
(* GREEN: goal 5 > 0, no hypotheses: Proved (silent). *)

let v : int{ _ > 0 } = 5;;
[%%expect{|
Line 1, characters 4-5: refined environment entry: v : int{ _ > 0 }
Line 1, characters 23-24: refinement obligation: int{ _ > 0 }
val v : int{ _ > 0 } = 5
|}]

(* --- arrow-domain: apply-arrow collection (no marker exists) ----------- *)
(* GREEN: goal 5 > 0: Proved. *)

let f1 : int{ _ > 0 } -> int = fun y -> y;;
[%%expect{|
val f1 : int{ _ > 0 } -> int = <fun>
|}]

let arrow_domain = f1 5;;
[%%expect{|
val arrow_domain : int = 5
|}]

(* --- optional-and-letop: the marker shapes of argument normalisation --- *)
(* GREEN: both Proved (goals 5 > 0; and the default's 1 > 0 at the
   definition). *)

let f4 : ?o:int{ _ > 0 } -> unit -> int = fun ?(o = 1) () -> o;;
[%%expect{|
Line 1, characters 52-53: refinement obligation: int{ _ > 0 }
val f4 : ?o:int{ _ > 0 } -> unit -> int = <fun>
|}]

let optional_arg = f4 ~o:5 ();;
[%%expect{|
Line 1, characters 25-26: refinement obligation: int{ _ > 0 }
val optional_arg : int = 5
|}]

let ( let+ ) : int{ _ > 0 } -> (int -> int) -> int = fun _ f -> f 1;;
[%%expect{|
val ( let+ ) : int{ _ > 0 } -> (int -> int) -> int = <fun>
|}]

let letop_arg = let+ y = 5 in y;;
[%%expect{|
Line 1, characters 25-26: refinement obligation: int{ _ > 0 }
val letop_arg : int = 1
|}]

(* --- partial-application: Omitted-then-supplied ------------------------ *)
(* GREEN: the obligation fires at the second apply; goal 5 > 0: Proved. *)

let f3 : a:int{ _ > 0 } -> b:int -> int = fun ~a ~b -> a + b;;
[%%expect{|
val f3 : a:int{ _ > 0 } -> b:int -> int = <fun>
|}]

let partial_application = (f3 ~b:2) ~a:5;;
[%%expect{|
val partial_application : int = 7
|}]

(* --- late-solved-arrow: collection from the solved arrow --------------- *)
(* The argument 0 was pre-stripped as a Known_arg: no funnel marker exists;
   the refined domain on the instantiated apply arrow is the only durable
   record.  GREEN: goal 0 > 0: Refuted, located error. *)

let late_solved_arrow = let app f x = f x in app f1 0;;
[%%expect{|
val late_solved_arrow : int = 0
|}]

(* --- late-solved-residue: argument typed before the domain solved ------ *)
(* y is checked against a still-open variable; f1 solves it afterwards,
   leaving exp_type residue the fact rules ignore while the arrow walk still
   collects.  GREEN: goal y > 0, no hypothesis about y: Unknown. *)

let late_solved_residue y = let app x f = f x in app y f1;;
[%%expect{|
Line 1, characters 53-54: refined head on expression: int{ _ > 0 }
val late_solved_residue : int{ _ > 0 } -> int = <fun>
|}]

(* --- dedup-annotated-arg: marker and arrow domain coincide ------------- *)
(* GREEN: one obligation, not two (its printing baseline shows exactly one
   query); goal 5 > 0: Proved. *)

let dedup_annotated_arg = f1 (5 : int{ _ > 0 });;
[%%expect{|
Line 1, characters 30-31: refinement obligation: int{ _ > 0 }
val dedup_annotated_arg : int = 5
|}]

(* --- dedup-ignore: the %ignore special application path ---------------- *)
(* The argument gets a funnel marker and the funct arrow retains the refined
   domain.  GREEN: one failure reported, not two; goal 0 > 0: Refuted. *)

external drop : int{ _ > 0 } -> unit = "%ignore";;
[%%expect{|
external drop : int{ _ > 0 } -> unit = "%ignore"
|}]

let dedup_ignore = drop 0;;
[%%expect{|
Line 1, characters 24-25: refinement obligation: int{ _ > 0 }
val dedup_ignore : unit = ()
|}]

(* --- dependent-arrow-escape: higher-order solving past the rejection --- *)
(* Direct application of d is rejected upstream; the higher-order shape
   compiles and hands this pass an arrow whose binder lives inside the
   argument's type.  GREEN: a located unsupported rejection — never a crash,
   never a silent skip. *)

external d : m:int{ m > 0 } -> int = "%identity";;
[%%expect{|
external d : m:int{ m > 0 } -> int = "%identity"
|}]

let dependent_arrow_escape = let app f x = f x in app d 5;;
[%%expect{|
val dependent_arrow_escape : int = 5
|}]

(* --- fact-binder-and-path: the centrepiece ------------------------------ *)
(* GREEN: the recursive call's argument obligation, goal y - 1 > 0 under the
   binder fact y > 0 and the path condition not (y <= 1): Proved, in
   Bitvec 63.  Fails if the binder fact, the path condition or the machine
   arithmetic is disabled alone. *)

let rec fact (y : int{ _ > 0 }) : int =
  if y <= 1 then 1 else y * fact (y - 1);;
[%%expect{|
val fact : int{ _ > 0 } -> int = <fun>
|}]

let fact_5 = fact 5;;
[%%expect{|
val fact_5 : int = 120
|}]

(* --- ident-fact: value-description facts at occurrences ----------------- *)
(* GREEN: the v element discharges only through the value-description fact
   v > 0: Proved; Unknown if that fact source is disabled. *)

let ident_fact : int{ _ > 0 } list = [5; v];;
[%%expect{|
Line 1, characters 38-39: refinement obligation: int{ _ > 0 }
Line 1, characters 41-42: refinement obligation: int{ _ > 0 }
val ident_fact : int{ _ > 0 } list = [5; 5]
|}]

(* --- push-to-arms: result-position pushing through if ------------------- *)
(* GREEN: two goals, 1 > 0 and 2 > 0, both Proved; an opaque Unknown if the
   push is disabled. *)

let push_to_arms c : int{ _ > 0 } = if c then 1 else 2;;
[%%expect{|
Line 1, characters 36-54: refinement obligation: int{ _ > 0 }
val push_to_arms : bool -> int{ _ > 0 } = <fun>
|}]

(* --- match-push: a refined match result pushed to its arms -------------- *)
(* Load-bearing: a match result does not lower as a term at all, so without
   the push this is an opaque Unknown.  GREEN: both arm goals Proved. *)

let match_push (c : bool) : int{ _ > 0 } =
  match c with true -> 1 | false -> 2;;
[%%expect{|
Line 2, characters 2-37: refinement obligation: int{ _ > 0 }
val match_push : bool -> int{ _ > 0 } = <fun>
|}]

(* --- short-circuit: the && left-operand path condition ------------------ *)
(* GREEN: f1's argument obligation x > 0 under the fact (x > 0) = true from
   the left operand: Proved. *)

let short_circuit x = x > 0 && f1 x > 0;;
[%%expect{|
val short_circuit : int -> bool = <fun>
|}]

(* --- eta-domain: the synthetic apply's argument obligation --------------- *)
(* Optional-argument elimination eta-expands gopt2; the synthesized
   application checks the eta binder against the refined domain.  GREEN:
   Proved via the eta binder's own pattern fact. *)

let ho2 (h : int{ _ > 0 } -> int) = h 5;;
[%%expect{|
val ho2 : (int{ _ > 0 } -> int) -> int = <fun>
|}]

let gopt2 : ?o:bool -> int{ _ > 0 } -> int = fun ?o:_ y -> y;;
[%%expect{|
val gopt2 : ?o:bool -> int{ _ > 0 } -> int = <fun>
|}]

let eta_domain = ho2 gopt2;;
[%%expect{|
val eta_domain : int = 5
|}]

(* --- field-fact: the label-description fact source ---------------------- *)
(* GREEN: the read of an immutable field declared int{ _ > 0 } deposits the
   predicate as a fact about the read: Proved. *)

type box = { first_pos : int{ _ > 0 }; second : int };;
[%%expect{|
type box = { first_pos : int{ _ > 0 }; second : int; }
|}]

let field_fact (b : box) : int{ _ > 0 } = b.first_pos;;
[%%expect{|
Line 1, characters 42-53: refinement obligation: int{ _ > 0 }
val field_fact : box -> int{ _ > 0 } = <fun>
|}]

(* --- stability-mutable-arg: the logicality half of the stability gate --- *)
(* reads_param is total but its argument type (int ref) does not cross
   logicality, so the two calls abstract to distinct opaque constants and
   the false equality is unprovable.  At run time this returns 1, so a
   Call-equality lowering would prove a false spec.  GREEN: Unknown. *)

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

(* --- poly-instances: one total polymorphic function at two sorts --------- *)
(* GREEN: id used at bool (the condition) and at int (the then-arm) in one
   obligation: two declarations in the signature; both arm goals Proved
   (id 5 abstracts? no — id is total, so Call id<Bv63> 5 is uninterpreted:
   the then-arm goal id 5 > 0 is Unknown; the else arm 1 > 0 is Proved).
   The observable is the pair of instance declarations in the printing
   baseline; the verdict here pins that an uninterpreted total call is not
   assumed positive. *)

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

(* --- shadowed-local: stamped identity in the symbol allocator ------------ *)
(* Two shadowed local total fs must become two distinct symbols; collapsing
   them would prove u = w.  GREEN: both arm goals Proved (binder fact x > 0
   and the constant 1 > 0) — the discriminator is the two declarations in
   the printing baseline. *)

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

(* --- mutable-in-predicate: rejected at VC time --------------------------- *)
(* The predicate filter is syntactic, so int{ _ = y } with y mutable compiles
   today; no fact or goal built from it has one denotation.  GREEN: a located
   "this predicate reads mutable state" rejection. *)

let mutable_in_predicate () =
  let mutable y = 1 in
  let q : int{ _ = y } = 1 in
  y <- 2;
  q + y;;
[%%expect{|
Line 3, characters 25-26: refinement obligation: int{ _ = y }
val mutable_in_predicate : unit -> int = <fun>
|}]

(* --- predicate-sort-error: the located predicate sort checker ------------ *)
(* Nothing upstream checks predicate sorts: this compiles today.  GREEN: a
   located sort error at the obligation's site, not a solver failure. *)

let predicate_sort_error : int{ 1 + true } = 0;;
[%%expect{|
Line 1, characters 4-24: refined environment entry: predicate_sort_error :
  int{ 1 + true }
Line 1, characters 45-46: refinement obligation: int{ 1 + true }
val predicate_sort_error : int{ 1 + true } = 0
|}]

(* --- shift-bounds: the guarded shift rows at their boundaries ------------ *)
(* x lsl n lowers to Ite (0 <= n && n <= 62, Bv_shl x n, c) with c opaque.
   GREEN: in-range counts are interpreted (Proved); 63 and negative counts
   fall into the opaque arm (Unknown). *)

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

(* --- let-equality-opaque: apply-codomain fact + opaque equality ---------- *)
(* GREEN: Proved only if both fire — the codomain fact p(c) about the call's
   opaque constant and the let equality x = c. *)

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

(* --- mutable-fact: per-read declared facts ------------------------------- *)
(* GREEN: the read's per-read fact x > 0 proves the codomain goal: Proved. *)

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

(* --- mutvar-reads-distinct: the per-read subjects sentinel --------------- *)
(* Evaluates to 1, so a single-symbol lowering would prove a false spec; the
   per-read constants make the equality unprovable.  GREEN: Unknown. *)

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

(* --- refuted-const: the trustworthy negative verdict --------------------- *)
(* GREEN: prove query sat, disprove query (0 > 0, no hypotheses) unsat:
   Refuted, located "the predicate is refutable" error. *)

let refuted_const : int{ _ > 0 } = 0;;
[%%expect{|
Line 1, characters 4-17: refined environment entry: refuted_const :
  int{ _ > 0 }
Line 1, characters 35-36: refinement obligation: int{ _ > 0 }
val refuted_const : int{ _ > 0 } = 0
|}]

(* --- unknown-opaque: the partial-parameter half of the stability gate ---- *)
(* h is a partial parameter: the call abstracts to an opaque constant c, and
   c >= 0 is neither provable nor refutable.  GREEN: Unknown (Incomplete _). *)

let unknown_opaque (h : unit -> int) : int{ _ >= 0 } = h ();;
[%%expect{|
Line 1, characters 55-59: refinement obligation: int{ _ >= 0 }
val unknown_opaque : (unit -> int) -> int{ _ >= 0 } = <fun>
|}]

(* --- bitvec-wrap: machine arithmetic is not the integers ----------------- *)
(* ints lower to Bitvec 63 and OCaml int arithmetic wraps: x = max_int
   defeats the prove query, x = 0 defeats the disprove query.  GREEN:
   Unknown — not Proved, which the mathematical integers would give. *)

let bitvec_wrap (x : int{ _ >= 0 }) : int{ _ >= 0 } = x + 1;;
[%%expect{|
Line 1, characters 54-59: refinement obligation: int{ _ >= 0 }
val bitvec_wrap : int{ _ >= 0 } -> int{ _ >= 0 } = <fun>
|}]

(* --- tuple-datatype: one datatype through Signature.instantiate ---------- *)
(* GREEN: the predicate projects the tuple subject through the match
   lowering (Select on the single-constructor datatype); goal 2 > 1:
   Proved. *)

let tuple_datatype : (int * int){ match _ with (a, b) -> a > b } = (2, 1);;
[%%expect{|
Line 1, characters 4-18: refined environment entry: tuple_datatype :
  (int * int){ match _ with | (a, b) -> a > b }
Line 1, characters 67-73: refinement obligation:
  (int * int){ match _ with | (a, b) -> a > b }
val tuple_datatype : (int * int){ match _ with | (a, b) -> a > b } = (2, 1)
|}]

(* --- sealed-datatype: the deciding environment is the obligation site ---- *)
(* GREEN: the inside obligation sees t as a concrete record datatype; the
   outside obligation sees Sealed.t as an uninterpreted sort — the printing
   baselines differ, and client proofs cannot lean on the hidden
   representation.  Both goals are true: Proved. *)

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

(* --- open-datatype-reject: no finite constructor list to close ----------- *)
(* GREEN: a located rejection — an extensible variant cannot become a
   Datatype.decl, and silent abstraction is refused. *)

type ext = ..;;
[%%expect{|
type ext = ..
|}]

type ext += Ext_case;;
[%%expect{|
type ext += Ext_case
|}]

let open_datatype_reject : ext{ true } = Ext_case;;
[%%expect{|
Line 1, characters 4-24: refined environment entry: open_datatype_reject :
  ext{ true }
Line 1, characters 41-49: refinement obligation: ext{ true }
val open_datatype_reject : ext{ true } = Ext_case
|}]

(* --- alias: expansion in the collection gate and the lowering ------------ *)
(* GREEN: both goals 5 >= 0 through the nat alias: Proved. *)

type nat = int{ _ >= 0 };;
[%%expect{|
type nat = int{ _ >= 0 }
|}]

let alias_intro : nat = 5;;
[%%expect{|
Line 1, characters 4-15: refined environment entry: alias_intro : nat
Line 1, characters 24-25: refinement obligation: nat
val alias_intro : nat = 5
|}]

let f5 : nat -> int = fun z -> z;;
[%%expect{|
val f5 : nat -> int = <fun>
|}]

let alias_arg = f5 5;;
[%%expect{|
val alias_arg : int = 5
|}]

(* --- unrepresentable: tier 2's located error ------------------------------ *)
(* A refined annotation on a function-typed value: the subject's sort cannot
   be represented.  GREEN: "this expression cannot yet be represented in a
   verification condition", pinned so it never degrades into silence. *)

let unrepresentable : (int -> int){ true } = fun a -> a;;
[%%expect{|
Line 1, characters 4-19: refined environment entry: unrepresentable :
  (int -> int){ true }
Line 1, characters 45-55: refinement obligation: (int -> int){ true }
val unrepresentable : (int -> int){ true } = <fun>
|}]

(* --- continue-past-failure: the failure protocol -------------------------- *)
(* Two independent variable-free defects (0 > 0 and 1 < 0) in one unit: both
   reported, the unit refused once, and the c obligation *after* the first
   failure proves by leaning on a's failed spec — the localisation trade,
   documented.  GREEN: two located failures plus one refusal naming the
   count. *)

let continue_past_failure =
  let a : int{ _ > 0 } = 0 in
  let b : int{ _ < 0 } = 1 in
  let c : int{ _ > 0 } = a in
  a + b + c;;
[%%expect{|
Line 2, characters 25-26: refinement obligation: int{ _ > 0 }
Line 3, characters 25-26: refinement obligation: int{ _ < 0 }
Line 4, characters 25-26: refinement obligation: int{ _ > 0 }
val continue_past_failure : int = 1
|}]
