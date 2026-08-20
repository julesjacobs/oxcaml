(* TEST
 readonly_files = "has_z3.sh";
 script = "sh ${test_source_directory}/has_z3.sh";
 script;
 flags = "-extension let_mutable -drefinements -vox-backend z3";
 expect;
*)

(* Vox VC generation: the full verdict corpus, discharged through the z3
   backend (design-docs/vc-generation.md, Tests); skipped when no solver is
   installed (has_z3.sh exits 125), exactly like the solver piece's z3
   tests.  The gate and the driver resolve z3 identically ($VOX_TEST_Z3,
   then PATH, then the pinned install), so a skip decision and a run
   decision can never disagree.

   Every verdict below was promoted from a live solver run (z3 4.8.5), never
   hand-written.  The -drefinements probe lines are the obligation map;
   Proved obligations are silent, so their fixtures keep bare val lines —
   their query bytes are pinned in vc-printing.ml — while the
   Refuted/Unknown fixtures and the located rejections carry their reports
   in the blocks, followed by the one refusal error naming the count, which
   also suppresses the phrase's val line. *)

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
Line 1, characters 52-53:
1 | let late_solved_arrow = let app f x = f x in app f1 0;;
                                                        ^
Error: Refinement verification failed: the predicate is refutable.
Line 1, characters 52-53:
1 | let late_solved_arrow = let app f x = f x in app f1 0;;
                                                        ^
Error: 1 refinement obligation was not verified.
|}]

(* --- late-solved-residue: argument typed before the domain solved ------ *)
(* y is checked against a still-open variable; f1 solves it afterwards,
   leaving exp_type residue the fact rules ignore while the arrow walk still
   collects.  Late solving also refines y's pattern type and its environment
   entry (the strips ran before the domain was determined), so the binder
   fact and the value-description fact each supply the goal: Proved
   (silent).  The duplicate hypothesis is the fingerprint, pinned in
   vc-printing.ml. *)

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
Line 1, characters 24-25:
1 | let dedup_ignore = drop 0;;
                            ^
Error: Refinement verification failed: the predicate is refutable.
Line 1, characters 24-25:
1 | let dedup_ignore = drop 0;;
                            ^
Error: 1 refinement obligation was not verified.
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
Line 1, characters 56-57:
1 | let dependent_arrow_escape = let app f x = f x in app d 5;;
                                                            ^
Error: This application involves a dependent function type that cannot yet be verified.
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

(* --- predicate-ident-fact and wildcard-read: declared facts reach goals
   whose predicates mention them ------------------------------------------- *)
(* w3's declared fact (w3 = 3) rides ident resolution in BOTH front ends:
   `direct`'s goal predicate mentions w3 while its subject does not (the
   deposit fires at goal assembly), and `wildcard_read` additionally pins
   that the rhs of every binding pattern is lowered — a wildcard lemma
   read deposits exactly as a named read does.  GREEN: both Proved
   (silent); the hypothesis shapes are pinned in vc-printing.ml. *)

let w3 : int{ _ = 3 } = 3;;
[%%expect{|
Line 1, characters 4-6: refined environment entry: w3 : int{ _ = 3 }
Line 1, characters 24-25: refinement obligation: int{ _ = 3 }
val w3 : int{ _ = 3 } = 3
|}]

let direct : int{ _ > w3 } = 5;;
[%%expect{|
Line 1, characters 4-10: refined environment entry: direct : int{ _ > w3 }
Line 1, characters 29-30: refinement obligation: int{ _ > w3 }
val direct : int{ _ > w3 } = 5
|}]

let wildcard_read : int{ _ > w3 } = let _ = w3 in 5;;
[%%expect{|
Line 1, characters 4-17: refined environment entry: wildcard_read :
  int{ _ > w3 }
Line 1, characters 36-51: refinement obligation: int{ _ > w3 }
val wildcard_read : int{ _ > w3 } = 5
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

(* --- assert-fact: the assert arm of the path-condition rule -------------- *)
(* GREEN: the sequenced assert's condition is a fact for the tail, so the
   annotation goal c > 0 is Proved (silent; the query is pinned in
   vc-printing.ml).  vc-z3-noassert.ml pins the flag gate: under -noassert
   translcore erases the assert and this same shape must refuse. *)

let assert_fact (c : int) : int =
  assert (c > 0);
  (c : int{ _ > 0 });;
[%%expect{|
Line 3, characters 3-4: refinement obligation: int{ _ > 0 }
val assert_fact : int -> int = <fun>
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
Line 4, characters 2-19:
4 |   reads_param r - a;;
      ^^^^^^^^^^^^^^^^^
Error: This refinement obligation could not be verified (prove query: sat; disprove query: sat).
Line 4, characters 2-19:
4 |   reads_param r - a;;
      ^^^^^^^^^^^^^^^^^
Error: 1 refinement obligation was not verified.
|}]

(* --- poly-instances: one total polymorphic function at two sorts --------- *)
(* GREEN: id used at bool (the condition) and at int (the then-arm) in one
   obligation yields two ground declarations in the signature (pinned in
   vc-printing.ml).  id is total, so id 5 lowers to an uninterpreted Call
   about which nothing is assumed: the then-arm goal id 5 > 0 is Unknown
   (the report below), the else-arm goal 1 > 0 is Proved. *)

let id @ total = fun a -> a;;
[%%expect{|
val id : 'a -> 'a = <fun>
|}]

let poly_instances : int{ _ > 0 } = if id true then id 5 else 1;;
[%%expect{|
Line 1, characters 4-18: refined environment entry: poly_instances :
  int{ _ > 0 }
Line 1, characters 36-63: refinement obligation: int{ _ > 0 }
Line 1, characters 52-56:
1 | let poly_instances : int{ _ > 0 } = if id true then id 5 else 1;;
                                                        ^^^^
Error: This refinement obligation could not be verified (prove query: sat; disprove query: sat).
Line 1, characters 52-56:
1 | let poly_instances : int{ _ > 0 } = if id true then id 5 else 1;;
                                                        ^^^^
Error: 1 refinement obligation was not verified.
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
Line 3, characters 19-20:
3 |   let q : int{ _ = y } = 1 in
                       ^
Error: This predicate reads mutable state, which cannot yet be verified.
|}]

(* --- poly-in-predicate: an honest rejection for an immutable value -------- *)
(* A generic scheme fails the logicality crossing conservatively (its
   variables promise nothing), so [pnil] used to be misdiagnosed as "reads
   mutable state"; rexp is untyped, so there is no occurrence type to
   instantiate the scheme at.  GREEN: a located polymorphic-value
   rejection. *)

let pnil = [];;
[%%expect{|
val pnil : 'a list = []
|}]

let poly_in_predicate : int{ _ > 0 && pnil = pnil } = 5;;
[%%expect{|
Line 1, characters 4-21: refined environment entry: poly_in_predicate :
  int{ (_ > 0) && (pnil = pnil) }
Line 1, characters 54-55: refinement obligation:
  int{ (_ > 0) && (pnil = pnil) }
Line 1, characters 38-42:
1 | let poly_in_predicate : int{ _ > 0 && pnil = pnil } = 5;;
                                          ^^^^
Error: This expression cannot yet be represented in a verification condition:
       a value with a polymorphic type cannot yet appear in a predicate.
|}]

(* --- predicate-sort-error: the located predicate sort checker ------------ *)
(* Nothing upstream checks predicate sorts: this compiles today.  GREEN: a
   located sort error at the obligation's site, not a solver failure. *)

let predicate_sort_error : int{ 1 + true } = 0;;
[%%expect{|
Line 1, characters 4-24: refined environment entry: predicate_sort_error :
  int{ 1 + true }
Line 1, characters 45-46: refinement obligation: int{ 1 + true }
Line 1, characters 32-40:
1 | let predicate_sort_error : int{ 1 + true } = 0;;
                                    ^^^^^^^^
Error: This refinement predicate is ill-sorted:
       Stdlib.+ is applied to operand(s) of sort Bv63, Bool.
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
Line 1, characters 52-59:
1 | let shift_over (n : int{ _ = 63 }) : int{ _ = 0 } = 1 lsl n;;
                                                        ^^^^^^^
Error: This refinement obligation could not be verified (prove query: sat; disprove query: sat).
Line 1, characters 52-59:
1 | let shift_over (n : int{ _ = 63 }) : int{ _ = 0 } = 1 lsl n;;
                                                        ^^^^^^^
Error: 1 refinement obligation was not verified.
|}]

let shift_negative (n : int{ _ = -1 }) : int{ _ = 0 } = 1 lsl n;;
[%%expect{|
Line 1, characters 56-63: refinement obligation: int{ _ = 0 }
Line 1, characters 56-63:
1 | let shift_negative (n : int{ _ = -1 }) : int{ _ = 0 } = 1 lsl n;;
                                                            ^^^^^^^
Error: This refinement obligation could not be verified (prove query: sat; disprove query: sat).
Line 1, characters 56-63:
1 | let shift_negative (n : int{ _ = -1 }) : int{ _ = 0 } = 1 lsl n;;
                                                            ^^^^^^^
Error: 1 refinement obligation was not verified.
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
Line 3, characters 2-23:
3 |   ((x <- x + 1); x) - x;;
      ^^^^^^^^^^^^^^^^^^^^^
Error: This refinement obligation could not be verified (prove query: sat; disprove query: sat).
Line 3, characters 9-14:
3 |   ((x <- x + 1); x) - x;;
             ^^^^^
Error: This refinement obligation could not be verified (prove query: sat; disprove query: sat).
Line 3, characters 2-23:
3 |   ((x <- x + 1); x) - x;;
      ^^^^^^^^^^^^^^^^^^^^^
Error: 2 refinement obligations were not verified.
|}]

(* --- refuted-const: the trustworthy negative verdict --------------------- *)
(* GREEN: prove query sat, disprove query (0 > 0, no hypotheses) unsat:
   Refuted, located "the predicate is refutable" error. *)

let refuted_const : int{ _ > 0 } = 0;;
[%%expect{|
Line 1, characters 4-17: refined environment entry: refuted_const :
  int{ _ > 0 }
Line 1, characters 35-36: refinement obligation: int{ _ > 0 }
Line 1, characters 35-36:
1 | let refuted_const : int{ _ > 0 } = 0;;
                                       ^
Error: Refinement verification failed: the predicate is refutable.
Line 1, characters 35-36:
1 | let refuted_const : int{ _ > 0 } = 0;;
                                       ^
Error: 1 refinement obligation was not verified.
|}]

(* --- unknown-opaque: the partial-parameter half of the stability gate ---- *)
(* h is a partial parameter: the call abstracts to an opaque constant c, and
   c >= 0 is neither provable nor refutable.  GREEN: Unknown (Incomplete _). *)

let unknown_opaque (h : unit -> int) : int{ _ >= 0 } = h ();;
[%%expect{|
Line 1, characters 55-59: refinement obligation: int{ _ >= 0 }
Line 1, characters 55-59:
1 | let unknown_opaque (h : unit -> int) : int{ _ >= 0 } = h ();;
                                                           ^^^^
Error: This refinement obligation could not be verified (prove query: sat; disprove query: sat).
Line 1, characters 55-59:
1 | let unknown_opaque (h : unit -> int) : int{ _ >= 0 } = h ();;
                                                           ^^^^
Error: 1 refinement obligation was not verified.
|}]

(* --- bitvec-wrap: machine arithmetic is not the integers ----------------- *)
(* ints lower to Bitvec 63 and OCaml int arithmetic wraps: x = max_int
   defeats the prove query, x = 0 defeats the disprove query.  GREEN:
   Unknown — not Proved, which the mathematical integers would give. *)

let bitvec_wrap (x : int{ _ >= 0 }) : int{ _ >= 0 } = x + 1;;
[%%expect{|
Line 1, characters 54-59: refinement obligation: int{ _ >= 0 }
Line 1, characters 54-59:
1 | let bitvec_wrap (x : int{ _ >= 0 }) : int{ _ >= 0 } = x + 1;;
                                                          ^^^^^
Error: This refinement obligation could not be verified (prove query: sat; disprove query: sat).
Line 1, characters 54-59:
1 | let bitvec_wrap (x : int{ _ >= 0 }) : int{ _ >= 0 } = x + 1;;
                                                          ^^^^^
Error: 1 refinement obligation was not verified.
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
  val sd_env : int{ _ > 0 }
end = struct
  type t = { field : int }
  let mk field = { field }
  let sd_env : int{ _ > 0 } = let _s = mk 1 in 1
end;;
[%%expect{|
Line 8, characters 6-12: refined environment entry: sd_env : int{ _ > 0 }
Line 8, characters 30-48: refinement obligation: int{ _ > 0 }
module Sealed : sig type t val mk : int -> t val sd_env : int{ _ > 0 } end
|}]

let sd_env_out : int{ _ > 0 } = let _s = Sealed.mk 2 in 2;;
[%%expect{|
Line 1, characters 4-14: refined environment entry: sd_env_out : int{ _ > 0 }
Line 1, characters 32-57: refinement obligation: int{ _ > 0 }
val sd_env_out : int{ _ > 0 } = 2
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
Line 1, characters 41-49:
1 | let open_datatype_reject : ext{ true } = Ext_case;;
                                             ^^^^^^^^
Error: This expression cannot yet be represented in a verification condition:
       its type is an open (extensible) variant.
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
Line 1, characters 45-55:
1 | let unrepresentable : (int -> int){ true } = fun a -> a;;
                                                 ^^^^^^^^^^
Error: This expression cannot yet be represented in a verification condition:
       it has a function type.
|}]

(* --- continue-past-failure: the failure protocol -------------------------- *)
(* Two independent variable-free defects (0 > 0 and 1 < 0) in one unit: both
   reported, the unit refused once, and the cpf_c obligation *after* the
   first failure proves by leaning on cpf_a's failed spec (its
   value-description fact) — the localisation trade, documented.  The three
   bindings are sibling structure items, not a nested let chain: a failed
   spec's binder fact plus its let equality are contradictory hypotheses,
   so anything downstream in the same *expression* fact scope would prove
   vacuously and the second defect would go unreported; structure items
   record no let equality, which keeps the defects independent.  The module
   wrapper makes the three items one toplevel phrase — the expect runner
   runs each top-level structure item as its own phrase and stops the block
   at its first error, so unwrapped siblings would show only the first
   defect (batch compilation of the unwrapped siblings gives exactly the
   output below).  GREEN: two located failures plus one refusal naming the
   count. *)

module Cpf = struct
  let cpf_a : int{ _ > 0 } = 0
  let cpf_b : int{ _ < 0 } = 1
  let cpf_c : int{ _ > 0 } = cpf_a
end;;
[%%expect{|
Line 2, characters 6-11: refined environment entry: cpf_a : int{ _ > 0 }
Line 3, characters 6-11: refined environment entry: cpf_b : int{ _ < 0 }
Line 4, characters 6-11: refined environment entry: cpf_c : int{ _ > 0 }
Line 2, characters 29-30: refinement obligation: int{ _ > 0 }
Line 3, characters 29-30: refinement obligation: int{ _ < 0 }
Line 4, characters 29-34: refinement obligation: int{ _ > 0 }
Line 2, characters 29-30:
2 |   let cpf_a : int{ _ > 0 } = 0
                                 ^
Error: Refinement verification failed: the predicate is refutable.
Line 3, characters 29-30:
3 |   let cpf_b : int{ _ < 0 } = 1
                                 ^
Error: Refinement verification failed: the predicate is refutable.
Line 2, characters 29-30:
2 |   let cpf_a : int{ _ > 0 } = 0
                                 ^
Error: 2 refinement obligations were not verified.
|}]

(* --- admission-report: assumed contracts are visible output ---------------- *)
(* ax_source is an external whose refined codomain nothing checked: using its
   codomain contract as a fact makes the verdict conditional, and discharge
   mode says so in the admission report.  The %identity coercion from unit is
   sort-guarded out of the operator table, so the call abstracts to an opaque
   constant carrying the codomain fact. *)

external ax_source : unit -> int{ _ > 0 } = "%identity";;
[%%expect{|
external ax_source : unit -> int{ _ > 0 } = "%identity"
|}]

let admission : int{ _ > 0 } = ax_source ();;
[%%expect{|
Line 1, characters 4-13: refined environment entry: admission : int{ _ > 0 }
Line 1, characters 31-43: refinement obligation: int{ _ > 0 }
Refinement verdicts are conditional on 1 assumed contract:
  ax_source : unit -> int{ _ > 0 }
val admission : int{ _ > 0 } = 0
|}]

(* --- admission-generic: a polymorphic axiom reports its declared scheme ----- *)
(* ax_poly deposits at two ground instantiations in one phrase; the report
   names the generic declaration — the whole trust surface in one line —
   not whichever specialization happened to deposit first (which would be
   source-order-dependent). *)

external ax_poly : 'a -> int{ _ > 0 } = "%identity";;
[%%expect{|
external ax_poly : 'a -> int{ _ > 0 } = "%identity"
|}]

let admission_generic : int{ _ > 0 } =
  let u = ax_poly true in
  let v = ax_poly () in
  ignore u; ignore v; 1;;
[%%expect{|
Line 1, characters 4-21: refined environment entry: admission_generic :
  int{ _ > 0 }
Lines 2-4, characters 2-23: refinement obligation: int{ _ > 0 }
Refinement verdicts are conditional on 1 assumed contract:
  ax_poly : 'a -> int{ _ > 0 }
val admission_generic : int{ _ > 0 } = 1
|}]

(* --- recursive-knot-hole: a RECORDED KNOWN HOLE, pinned -------------------- *)
(* KNOWN HOLE (owner-deferred to a later piece): a recursive value binding
   self-justifies its declared predicate — lowering the right-hand side
   resolves knot_false at its own declared refined type and the
   value-description deposit adds the very fact the obligation must
   establish (hypothesis false |- goal false), and the false fact then
   propagates (knot_boom verifies 0 > 1).  Excluding a recursive group's
   own idents needs recursive-group infrastructure this piece does not
   carry (design-docs/vc-generation.md, Known holes).  These fixtures pin
   the CURRENT accepting behaviour so the later piece has a discriminating
   test to flip; if either block stops accepting, the hole moved —
   re-record it there, not here.  The type carries a base constructor so
   the hole rides a well-founded datatype: a baseless recursive variant
   lowers to an uninterpreted sort, whose opaque subjects never resolve
   the ident and so cannot exhibit the deposit. *)

type knot = Stop | K of knot;;
[%%expect{|
type knot = Stop | K of knot
|}]

let rec knot_false : knot{ false } = K knot_false;;
[%%expect{|
Line 1, characters 8-18: refined environment entry: knot_false :
  knot{ false }
Line 1, characters 37-49: refinement obligation: knot{ false }
val knot_false : knot{ false } = K <cycle>
|}]

let knot_boom : int{ 0 > 1 } = let _ = knot_false in 0;;
[%%expect{|
Line 1, characters 4-13: refined environment entry: knot_boom : int{ 0 > 1 }
Line 1, characters 31-54: refinement obligation: int{ 0 > 1 }
val knot_boom : int{ 0 > 1 } = 0
|}]

(* --- local-knot: the local let-rec route does NOT self-justify ------------- *)
(* A local group's recursive occurrences carry the unrefined payload type in
   the right-hand sides' environment, so no self-deposit fires there: each
   rhs obligation is the bare goal [false] and is REFUTED, where the
   module-level knot above self-justifies and accepts (queries pinned in
   vc-printing.ml).  If this fixture ever starts accepting, the
   self-justification hole has spread to the local route — re-record it in
   the design doc's known-holes entry. *)

let local_knot : int{ 0 > 1 } =
  let rec a : knot{ false } = K b
  and b : knot{ false } = K a in
  let _ = a in 0;;
[%%expect{|
Line 1, characters 4-14: refined environment entry: local_knot : int{ 0 > 1 }
Lines 2-4, characters 2-16: refinement obligation: int{ 0 > 1 }
Line 2, characters 30-33: refinement obligation: knot{ false }
Line 3, characters 26-29: refinement obligation: knot{ false }
Line 2, characters 30-33:
2 |   let rec a : knot{ false } = K b
                                  ^^^
Error: Refinement verification failed: the predicate is refutable.
Line 3, characters 26-29:
3 |   and b : knot{ false } = K a in
                              ^^^
Error: Refinement verification failed: the predicate is refutable.
Line 2, characters 30-33:
2 |   let rec a : knot{ false } = K b
                                  ^^^
Error: 2 refinement obligations were not verified.
|}]

(* --- cross-obligation: two obligations on one subject, one shared ident --- *)
(* The marker (annotation) and the arrow domain impose on the same subject;
   each pending owns its seen-idents snapshot, so the second obligation
   still deposits the w3 fact its own goal needs (the queries are pinned in
   vc-printing.ml).  GREEN: all Proved. *)

let w3 : int{ _ = 3 } = 3;;
[%%expect{|
Line 1, characters 4-6: refined environment entry: w3 : int{ _ = 3 }
Line 1, characters 24-25: refinement obligation: int{ _ = 3 }
val w3 : int{ _ = 3 } = 3
|}]

let cross_f (x : int{ _ > w3 }) = x;;
[%%expect{|
val cross_f : int{ _ > w3 } -> int = <fun>
|}]

let cross_probe = cross_f (5 : int{ _ > w3 - 1 });;
[%%expect{|
Line 1, characters 27-28: refinement obligation: int{ _ > (w3 - 1) }
val cross_probe : int = 5
|}]

(* --- shadowed-modules: two local Ms with distinct t in one query ----------- *)
(* The symbol allocator stamps the head of a dotted path, so the two [M.t]s
   are two datatypes ([M_1.t], [M_2.t]) rather than one declaration whose
   second constructor is undeclared.  GREEN: Proved. *)

let shadowed_modules : int{ _ > 0 } =
  let module M = struct type t = A end in
  let p = M.A in
  let module M = struct type t = B end in
  let q = M.B in
  ignore p; ignore q; 1;;
[%%expect{|
Line 1, characters 4-20: refined environment entry: shadowed_modules :
  int{ _ > 0 }
Lines 2-6, characters 2-23: refinement obligation: int{ _ > 0 }
val shadowed_modules : int{ _ > 0 } = 1
|}]

(* --- poly-value-sorts: one polymorphic value at two ground sorts ----------- *)
(* Value symbols are sort-sensitive: [nil] read at [int list] and at
   [bool list] in one obligation is two constants, never one name declared
   at the first sort and reused ill-sorted at the second — which the solver
   rejects, refusing valid source.  GREEN: Proved (query pinned in
   vc-printing.ml); the module-imported shape rides the same rule. *)

let nil = [];;
[%%expect{|
val nil : 'a list = []
|}]

let poly_value_sorts : int{ _ > 0 } =
  let a = (nil : int list) in
  let b = (nil : bool list) in
  ignore a; ignore b; 1;;
[%%expect{|
Line 1, characters 4-20: refined environment entry: poly_value_sorts :
  int{ _ > 0 }
Lines 2-4, characters 2-23: refinement obligation: int{ _ > 0 }
val poly_value_sorts : int{ _ > 0 } = 1
|}]

module Poly_source = struct let none = None end;;
[%%expect{|
module Poly_source : sig val none : 'a option end
|}]

let poly_value_module : int{ _ > 0 } =
  let a = (Poly_source.none : int option) in
  let b = (Poly_source.none : bool option) in
  ignore a; ignore b; 1;;
[%%expect{|
Line 1, characters 4-21: refined environment entry: poly_value_module :
  int{ _ > 0 }
Lines 2-4, characters 2-23: refinement obligation: int{ _ > 0 }
val poly_value_module : int{ _ > 0 } = 1
|}]

(* --- selfish-cycle: a baseless recursive variant is an uninterpreted sort -- *)
(* As an SMT datatype [selfish] would be rejected as non-well-founded, so a
   benign program touching the type could never verify (and a strictly
   inductive solver would read the sort as empty, vacating facts over its
   values).  It lowers to a declared uninterpreted sort instead
   (declaration pinned in vc-printing.ml).  GREEN: Proved. *)

type selfish = C of selfish;;
[%%expect{|
type selfish = C of selfish
|}]

let rec selfish_cycle : selfish = C selfish_cycle;;
[%%expect{|
val selfish_cycle : selfish = C <cycle>
|}]

let selfish_benign : int{ _ > 0 } = let y = selfish_cycle in ignore y; 1;;
[%%expect{|
Line 1, characters 4-18: refined environment entry: selfish_benign :
  int{ _ > 0 }
Line 1, characters 36-72: refinement obligation: int{ _ > 0 }
val selfish_benign : int{ _ > 0 } = 1
|}]

(* --- wf-box: well-foundedness is decided per ground instance --------------- *)
(* [wbox]'s declaration alone looks well-founded (its constructor's field is
   the parameter), but the instance [wbox<wfb>] carries [wfb] itself: the
   group has no base constructor anywhere, and as an SMT datatype group the
   solver rejects it — this fixture failed with "the solver rejected the
   query" under a declaration-name score.  Scoring the ground instance
   lowers [wfb] to a declared uninterpreted sort (pinned in vc-printing.ml)
   and the benign goal proves.  GREEN: Proved. *)

type 'a wbox = Box of 'a
type wfb = Knot of wfb wbox;;
[%%expect{|
type 'a wbox = Box of 'a
type wfb = Knot of wfb wbox
|}]

let rec wfb_cycle : wfb = Knot (Box wfb_cycle);;
[%%expect{|
val wfb_cycle : wfb = Knot (Box <cycle>)
|}]

let wfb_benign : int{ _ > 0 } = let y = Knot (Box wfb_cycle) in ignore y; 1;;
[%%expect{|
Line 1, characters 4-14: refined environment entry: wfb_benign : int{ _ > 0 }
Line 1, characters 32-75: refinement obligation: int{ _ > 0 }
val wfb_benign : int{ _ > 0 } = 1
|}]

(* --- good-bad: a well-founded instance reaching a baseless one ------------- *)
(* [int good] is well-founded via [Good]; its [Wrap] field is at the baseless
   [int bad], which lowers to an uninterpreted sort (rendering pinned in
   vc-printing.ml).  A declaration-name score rejected this grounded use
   with "its type is not fully determined".  GREEN: Proved. *)

type 'a bad = Bad of 'a bad
type 'a good = Good | Wrap of 'a bad;;
[%%expect{|
type 'a bad = Bad of 'a bad
type 'a good = Good | Wrap of 'a bad
|}]

let good_ground : (int good){ true } = Good;;
[%%expect{|
Line 1, characters 4-15: refined environment entry: good_ground :
  int good{ true }
Line 1, characters 39-43: refinement obligation: int good{ true }
val good_ground : int good{ true } = Good
|}]
