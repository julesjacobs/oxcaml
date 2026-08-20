(* TEST
 flags = "-extension let_mutable -drefinements -vox-backend printing";
 expect;
*)

(* Vox VC generation: SMT-LIB baselines through the printing backend
   (design-docs/vc-generation.md, Tests).

   A compact set — one fixture per lowering shape — whose expected output
   contains the Prove scripts byte for byte: declare-consts, :named
   hypotheses, the bitvec operators.  Printing shares the z3 renderer, so
   these baselines are the bytes z3 receives; vc-z3.ml holds the full corpus
   with the verdicts a live solver returns for the same shapes.  The
   -drefinements probe lines are the obligation map, kept so the recorded
   obligations and the emitted queries cross-check in one block.  Dump mode
   does not refuse units on Unknown, so every val line below survives with
   the queries printed alongside it; symbols are canonicalised
   per obligation, so these baselines do not churn when unrelated edits
   shift Ident stamps. *)

(* --- proved-const: constant goal, empty hypothesis list ----------------- *)

let v : int{ _ > 0 } = 5;;
[%%expect{|
Line 1, characters 4-5: refined environment entry: v : int{ _ > 0 }
Line 1, characters 23-24: refinement obligation: int{ _ > 0 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(assert (not (bvsgt (_ bv5 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val v : int{ _ > 0 } = 5
|}]

(* --- arrow-domain: instantiation against an apply-arrow domain ---------- *)

let f1 : int{ _ > 0 } -> int = fun y -> y;;
[%%expect{|
val f1 : int{ _ > 0 } -> int = <fun>
|}]

let arrow_domain = f1 5;;
[%%expect{|
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(assert (not (bvsgt (_ bv5 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val arrow_domain : int = 5
|}]

(* --- optional-and-letop: the marker shapes of argument normalisation ---- *)
(* The two funnel-marker shapes that bypass the apply arrow; the f4
   definition's own query is the default's 1 > 0. *)

let f4 : ?o:int{ _ > 0 } -> unit -> int = fun ?(o = 1) () -> o;;
[%%expect{|
Line 1, characters 52-53: refinement obligation: int{ _ > 0 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(assert (not (bvsgt (_ bv1 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val f4 : ?o:int{ _ > 0 } -> unit -> int = <fun>
|}]

let optional_arg = f4 ~o:5 ();;
[%%expect{|
Line 1, characters 25-26: refinement obligation: int{ _ > 0 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(assert (not (bvsgt (_ bv5 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val optional_arg : int = 5
|}]

let ( let+ ) : int{ _ > 0 } -> (int -> int) -> int = fun _ f -> f 1;;
[%%expect{|
val ( let+ ) : int{ _ > 0 } -> (int -> int) -> int = <fun>
|}]

let letop_arg = let+ y = 5 in y;;
[%%expect{|
Line 1, characters 25-26: refinement obligation: int{ _ > 0 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(assert (not (bvsgt (_ bv5 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val letop_arg : int = 1
|}]

(* --- partial-application: the obligation fires at the second apply ------- *)

let f3 : a:int{ _ > 0 } -> b:int -> int = fun ~a ~b -> a + b;;
[%%expect{|
val f3 : a:int{ _ > 0 } -> b:int -> int = <fun>
|}]

let partial_application = (f3 ~b:2) ~a:5;;
[%%expect{|
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(assert (not (bvsgt (_ bv5 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val partial_application : int = 7
|}]

(* --- eta-domain: the synthetic apply's argument obligation --------------- *)
(* Optional-argument elimination eta-expands gopt2; the second query is the
   eta binder against the refined domain, proved by the binder's own
   pattern fact (the h1 hypothesis below is that fact; the goal is
   provable only through it). *)

let ho2 (h : int{ _ > 0 } -> int) = h 5;;
[%%expect{|
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(assert (not (bvsgt (_ bv5 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val ho2 : (int{ _ > 0 } -> int) -> int = <fun>
|}]

let gopt2 : ?o:bool -> int{ _ > 0 } -> int = fun ?o:_ y -> y;;
[%%expect{|
val gopt2 : ?o:bool -> int{ _ > 0 } -> int = <fun>
|}]

let eta_domain = ho2 gopt2;;
[%%expect{|
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-const eta_1 (_ BitVec 63))
(assert (! (bvsgt eta_1 (_ bv0 63)) :named h1))
(assert (not (bvsgt eta_1 (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val eta_domain : int = 5
|}]

(* --- alias: expansion in the collection gate and the lowering ------------ *)

type nat = int{ _ >= 0 };;
[%%expect{|
type nat = int{ _ >= 0 }
|}]

let alias_intro : nat = 5;;
[%%expect{|
Line 1, characters 4-15: refined environment entry: alias_intro : nat
Line 1, characters 24-25: refinement obligation: nat
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(assert (not (bvsge (_ bv5 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val alias_intro : nat = 5
|}]

let f5 : nat -> int = fun z -> z;;
[%%expect{|
val f5 : nat -> int = <fun>
|}]

let alias_arg = f5 5;;
[%%expect{|
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(assert (not (bvsge (_ bv5 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val alias_arg : int = 5
|}]

(* --- dedup-annotated-arg: exactly one query for marker + arrow domain --- *)

let dedup_annotated_arg = f1 (5 : int{ _ > 0 });;
[%%expect{|
Line 1, characters 30-31: refinement obligation: int{ _ > 0 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(assert (not (bvsgt (_ bv5 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
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
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(assert (not (bvsgt (_ bv0 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val dedup_ignore : unit = ()
|}]

(* --- late-solved-residue: collection from the solved arrow ---------------- *)
(* No funnel marker exists for y's occurrence (the domain was an open
   variable when y was typed); the query below is the only observable that
   pins the arrow-walk collection, because the goal proves in vc-z3.ml
   (silently): late solving refines y's pattern type AND its environment
   entry (the strips ran before the domain was determined), so the binder
   fact and the value-description fact each supply the goal — the duplicate
   hypothesis is this fixture's fingerprint. *)

let late_solved_residue y = let app x f = f x in app y f1;;
[%%expect{|
Line 1, characters 53-54: refined head on expression: int{ _ > 0 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-const y_1 (_ BitVec 63))
(assert (! (bvsgt y_1 (_ bv0 63)) :named h1))
(assert (! (bvsgt y_1 (_ bv0 63)) :named h2))
(assert (not (bvsgt y_1 (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val late_solved_residue : int{ _ > 0 } -> int = <fun>
|}]

(* --- fact-binder-and-path: hypotheses and bitvec operators --------------- *)
(* The query for the recursive call shows the binder fact y > 0 and the
   else-branch path condition as :named hypotheses over Bitvec 63. *)

let rec fact (y : int{ _ > 0 }) : int =
  if y <= 1 then 1 else y * fact (y - 1);;
[%%expect{|
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-const y_1 (_ BitVec 63))
(assert (! (bvsgt y_1 (_ bv0 63)) :named h1))
(assert (! (not (bvsle y_1 (_ bv1 63))) :named h2))
(assert (not (bvsgt (bvsub y_1 (_ bv1 63)) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val fact : int{ _ > 0 } -> int = <fun>
|}]

(* --- ident-fact: the value-description hypothesis ------------------------ *)

let ident_fact : int{ _ > 0 } list = [5; v];;
[%%expect{|
Line 1, characters 38-39: refinement obligation: int{ _ > 0 }
Line 1, characters 41-42: refinement obligation: int{ _ > 0 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(assert (not (bvsgt (_ bv5 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-const v_1 (_ BitVec 63))
(assert (! (bvsgt v_1 (_ bv0 63)) :named h1))
(assert (not (bvsgt v_1 (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val ident_fact : int{ _ > 0 } list = [5; 5]
|}]

(* --- predicate-ident-fact / wildcard-read: declared facts for idents the
   goal's predicate mentions ------------------------------------------------ *)
(* `direct`'s single hypothesis is deposited at goal assembly (the
   predicate front end resolving w3); `wildcard_read` carries it twice —
   once from the wildcard binding's rhs lowering (the scope fact), once
   from assembly — so each mechanism is one hypothesis line here. *)

let w3 : int{ _ = 3 } = 3;;
[%%expect{|
Line 1, characters 4-6: refined environment entry: w3 : int{ _ = 3 }
Line 1, characters 24-25: refinement obligation: int{ _ = 3 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(assert (not (= (_ bv3 63) (_ bv3 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val w3 : int{ _ = 3 } = 3
|}]

let direct : int{ _ > w3 } = 5;;
[%%expect{|
Line 1, characters 4-10: refined environment entry: direct : int{ _ > w3 }
Line 1, characters 29-30: refinement obligation: int{ _ > w3 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-const w3_1 (_ BitVec 63))
(assert (! (= w3_1 (_ bv3 63)) :named h1))
(assert (not (bvsgt (_ bv5 63) w3_1)))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val direct : int{ _ > w3 } = 5
|}]

let wildcard_read : int{ _ > w3 } = let _ = w3 in 5;;
[%%expect{|
Line 1, characters 4-17: refined environment entry: wildcard_read :
  int{ _ > w3 }
Line 1, characters 36-51: refinement obligation: int{ _ > w3 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-const w3_1 (_ BitVec 63))
(assert (! (= w3_1 (_ bv3 63)) :named h1))
(assert (! (= w3_1 (_ bv3 63)) :named h2))
(assert (not (bvsgt (_ bv5 63) w3_1)))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val wildcard_read : int{ _ > w3 } = 5
|}]

(* --- push-to-arms / match-push: one obligation, one query per arm -------- *)

let push_to_arms c : int{ _ > 0 } = if c then 1 else 2;;
[%%expect{|
Line 1, characters 36-54: refinement obligation: int{ _ > 0 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-const c_1 Bool)
(assert (! c_1 :named h1))
(assert (not (bvsgt (_ bv1 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-const c_1 Bool)
(assert (! (not c_1) :named h1))
(assert (not (bvsgt (_ bv2 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val push_to_arms : bool -> int{ _ > 0 } = <fun>
|}]

let match_push (c : bool) : int{ _ > 0 } =
  match c with true -> 1 | false -> 2;;
[%%expect{|
Line 2, characters 2-37: refinement obligation: int{ _ > 0 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(assert (not (bvsgt (_ bv1 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(assert (not (bvsgt (_ bv2 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val match_push : bool -> int{ _ > 0 } = <fun>
|}]

(* --- assert-fact / short-circuit: the non-if arms of the path-condition
   rule (assert is gated on -noassert; vc-z3-noassert.ml pins the gate) --- *)

let assert_fact (c : int) : int =
  assert (c > 0);
  (c : int{ _ > 0 });;
[%%expect{|
Line 3, characters 3-4: refinement obligation: int{ _ > 0 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-const c_1 (_ BitVec 63))
(assert (! (bvsgt c_1 (_ bv0 63)) :named h1))
(assert (not (bvsgt c_1 (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val assert_fact : int -> int = <fun>
|}]

let short_circuit x = x > 0 && f1 x > 0;;
[%%expect{|
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-const x_1 (_ BitVec 63))
(assert (! (bvsgt x_1 (_ bv0 63)) :named h1))
(assert (not (bvsgt x_1 (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
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
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-datatypes ((box_1 0)) (
  ((mk_box_1 (box_1.first_pos (_ BitVec 63)) (box_1.second (_ BitVec 63))))))
(declare-const b_1 box_1)
(assert (! (bvsgt (box_1.first_pos b_1) (_ bv0 63)) :named h1))
(assert (not (bvsgt (box_1.first_pos b_1) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
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
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-const a_1 (_ BitVec 63))
(declare-const result/1 (_ BitVec 63))
(declare-const result/2 (_ BitVec 63))
(assert (! (= a_1 result/1) :named h1))
(assert (not (= (bvsub result/2 a_1) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
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
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-fun |id_1<Bool,Bool>| (Bool) Bool)
(declare-fun |id_1<Bv63,Bv63>| ((_ BitVec 63)) (_ BitVec 63))
(assert (! (|id_1<Bool,Bool>| true) :named h1))
(assert (not (bvsgt (|id_1<Bv63,Bv63>| (_ bv5 63)) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-fun |id_1<Bool,Bool>| (Bool) Bool)
(assert (! (not (|id_1<Bool,Bool>| true)) :named h1))
(assert (not (bvsgt (_ bv1 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
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
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-const x_1 (_ BitVec 63))
(declare-const u_1 (_ BitVec 63))
(declare-const w_1 (_ BitVec 63))
(declare-fun |f_1<Bv63,Bv63>| ((_ BitVec 63)) (_ BitVec 63))
(declare-fun |f_2<Bv63,Bv63>| ((_ BitVec 63)) (_ BitVec 63))
(assert (! (bvsgt x_1 (_ bv0 63)) :named h1))
(assert (! (= u_1 (|f_1<Bv63,Bv63>| x_1)) :named h2))
(assert (! (= w_1 (|f_2<Bv63,Bv63>| x_1)) :named h3))
(assert (! (bvslt u_1 w_1) :named h4))
(assert (not (bvsgt x_1 (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-const x_1 (_ BitVec 63))
(declare-const u_1 (_ BitVec 63))
(declare-const w_1 (_ BitVec 63))
(declare-fun |f_1<Bv63,Bv63>| ((_ BitVec 63)) (_ BitVec 63))
(declare-fun |f_2<Bv63,Bv63>| ((_ BitVec 63)) (_ BitVec 63))
(assert (! (bvsgt x_1 (_ bv0 63)) :named h1))
(assert (! (= u_1 (|f_1<Bv63,Bv63>| x_1)) :named h2))
(assert (! (= w_1 (|f_2<Bv63,Bv63>| x_1)) :named h3))
(assert (! (not (bvslt u_1 w_1)) :named h4))
(assert (not (bvsgt (_ bv1 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val shadowed_local : int{ _ > 0 } -> int{ _ > 0 } = <fun>
|}]

(* --- shift-bounds: the guarded shift lowering ------------------------------ *)
(* The queries show Ite (0 <= n && n <= 62, Bv_shl x n, c) with c opaque. *)

let shift_in_range : int{ _ > 0 } = 1 lsl 61;;
[%%expect{|
Line 1, characters 4-18: refined environment entry: shift_in_range :
  int{ _ > 0 }
Line 1, characters 36-44: refinement obligation: int{ _ > 0 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-const result/1 (_ BitVec 63))
(assert (not (bvsgt (ite (and (bvsle (_ bv0 63) (_ bv61 63)) (bvsle (_ bv61 63) (_ bv62 63))) (bvshl (_ bv1 63) (_ bv61 63)) result/1) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val shift_in_range : int{ _ > 0 } = 2305843009213693952
|}]

let shift_boundary (n : int{ _ = 62 }) : int{ _ < 0 } = 1 lsl n;;
[%%expect{|
Line 1, characters 56-63: refinement obligation: int{ _ < 0 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-const n_1 (_ BitVec 63))
(declare-const result/1 (_ BitVec 63))
(assert (! (= n_1 (_ bv62 63)) :named h1))
(assert (not (bvslt (ite (and (bvsle (_ bv0 63) n_1) (bvsle n_1 (_ bv62 63))) (bvshl (_ bv1 63) n_1) result/1) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val shift_boundary : int{ _ = 62 } -> int{ _ < 0 } = <fun>
|}]

let shift_over (n : int{ _ = 63 }) : int{ _ = 0 } = 1 lsl n;;
[%%expect{|
Line 1, characters 52-59: refinement obligation: int{ _ = 0 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-const n_1 (_ BitVec 63))
(declare-const result/1 (_ BitVec 63))
(assert (! (= n_1 (_ bv63 63)) :named h1))
(assert (not (= (ite (and (bvsle (_ bv0 63) n_1) (bvsle n_1 (_ bv62 63))) (bvshl (_ bv1 63) n_1) result/1) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val shift_over : int{ _ = 63 } -> int{ _ = 0 } = <fun>
|}]

let shift_negative (n : int{ _ = -1 }) : int{ _ = 0 } = 1 lsl n;;
[%%expect{|
Line 1, characters 56-63: refinement obligation: int{ _ = 0 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-const n_1 (_ BitVec 63))
(declare-const result/1 (_ BitVec 63))
(assert (! (= n_1 (_ bv9223372036854775807 63)) :named h1))
(assert (not (= (ite (and (bvsle (_ bv0 63) n_1) (bvsle n_1 (_ bv62 63))) (bvshl (_ bv1 63) n_1) result/1) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val shift_negative : int{ _ = (-1) } -> int{ _ = 0 } = <fun>
|}]

let shift_lsr (x : int{ _ > 0 }) : int{ _ >= 0 } = x lsr 1;;
[%%expect{|
Line 1, characters 51-58: refinement obligation: int{ _ >= 0 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-const x_1 (_ BitVec 63))
(declare-const result/1 (_ BitVec 63))
(assert (! (bvsgt x_1 (_ bv0 63)) :named h1))
(assert (not (bvsge (ite (and (bvsle (_ bv0 63) (_ bv1 63)) (bvsle (_ bv1 63) (_ bv62 63))) (bvlshr x_1 (_ bv1 63)) result/1) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val shift_lsr : int{ _ > 0 } -> int{ _ >= 0 } = <fun>
|}]

let shift_asr (x : int{ _ > 0 }) : int{ _ >= 0 } = x asr 1;;
[%%expect{|
Line 1, characters 51-58: refinement obligation: int{ _ >= 0 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-const x_1 (_ BitVec 63))
(declare-const result/1 (_ BitVec 63))
(assert (! (bvsgt x_1 (_ bv0 63)) :named h1))
(assert (not (bvsge (ite (and (bvsle (_ bv0 63) (_ bv1 63)) (bvsle (_ bv1 63) (_ bv62 63))) (bvashr x_1 (_ bv1 63)) result/1) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val shift_asr : int{ _ > 0 } -> int{ _ >= 0 } = <fun>
|}]

(* --- let-equality-opaque: codomain fact + equality over an opaque const --- *)

let g : unit -> int{ _ > 0 } = fun () -> 5;;
[%%expect{|
Line 1, characters 41-42: refinement obligation: int{ _ > 0 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(assert (not (bvsgt (_ bv5 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val g : unit -> int{ _ > 0 } = <fun>
|}]

let let_equality_opaque = let x = g () in (x : int{ _ > 0 });;
[%%expect{|
Line 1, characters 43-44: refinement obligation: int{ _ > 0 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-const result/1 (_ BitVec 63))
(declare-const x_1 (_ BitVec 63))
(assert (! (bvsgt result/1 (_ bv0 63)) :named h1))
(assert (! (= x_1 result/1) :named h2))
(assert (not (bvsgt x_1 (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
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
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(assert (not (bvsgt (_ bv5 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(assert (not (bvsgt (_ bv6 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-const result/1 (_ BitVec 63))
(assert (! (bvsgt result/1 (_ bv0 63)) :named h1))
(assert (not (bvsgt result/1 (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
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
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(assert (not (bvsgt (_ bv1 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-const result/1 (_ BitVec 63))
(declare-const result/2 (_ BitVec 63))
(assert (! (bvsgt result/1 (_ bv0 63)) :named h1))
(assert (! (bvsgt result/2 (_ bv0 63)) :named h2))
(assert (not (= (bvsub result/1 result/2) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-const result/1 (_ BitVec 63))
(assert (! (bvsgt result/1 (_ bv0 63)) :named h1))
(assert (not (bvsgt (bvadd result/1 (_ bv1 63)) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
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
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(assert (not (bvsgt (_ bv0 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val refuted_const : int{ _ > 0 } = 0
|}]

let unknown_opaque (h : unit -> int) : int{ _ >= 0 } = h ();;
[%%expect{|
Line 1, characters 55-59: refinement obligation: int{ _ >= 0 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-const result/1 (_ BitVec 63))
(assert (not (bvsge result/1 (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val unknown_opaque : (unit -> int) -> int{ _ >= 0 } = <fun>
|}]

let bitvec_wrap (x : int{ _ >= 0 }) : int{ _ >= 0 } = x + 1;;
[%%expect{|
Line 1, characters 54-59: refinement obligation: int{ _ >= 0 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-const x_1 (_ BitVec 63))
(assert (! (bvsge x_1 (_ bv0 63)) :named h1))
(assert (not (bvsge (bvadd x_1 (_ bv1 63)) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val bitvec_wrap : int{ _ >= 0 } -> int{ _ >= 0 } = <fun>
|}]

(* --- tuple-datatype: a datatype declaration in the query ------------------- *)

let tuple_datatype : (int * int){ match _ with (a, b) -> a > b } = (2, 1);;
[%%expect{|
Line 1, characters 4-18: refined environment entry: tuple_datatype :
  (int * int){ match _ with | (a, b) -> a > b }
Line 1, characters 67-73: refinement obligation:
  (int * int){ match _ with | (a, b) -> a > b }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-datatypes ((|tuple<Bv63,Bv63>| 0)) (
  ((|mk_tuple<Bv63,Bv63>| (|tuple<Bv63,Bv63>.0| (_ BitVec 63)) (|tuple<Bv63,Bv63>.1| (_ BitVec 63))))))
(assert (not (bvsgt (|tuple<Bv63,Bv63>.0| (|mk_tuple<Bv63,Bv63>| (_ bv2 63) (_ bv1 63))) (|tuple<Bv63,Bv63>.1| (|mk_tuple<Bv63,Bv63>| (_ bv2 63) (_ bv1 63))))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val tuple_datatype : (int * int){ match _ with | (a, b) -> a > b } = (2, 1)
|}]

(* --- member-namespace: datatype members are qualified and instance-mangled  *)
(* Constructors and selectors share the solver's one namespace, so members
   are qualified with the stamped declaration name (ab_1.A vs cd_1.A;
   r1_1.shared vs r2_1.shared) — two datatypes sharing a constructor or
   label name in one obligation is valid source, not an ill-formed
   obligation — and a parametric constructor in a term carries the
   instance suffix its instantiated declaration carries (option.Some<Bv63>). *)

type ab = A | B;;
[%%expect{|
type ab = A | B
|}]

type cd = A | C;;
[%%expect{|
type cd = A | C
|}]

let dup_constructor : int{ _ > 0 } =
  let p : ab = A in
  let q : cd = A in
  ignore p; ignore q; 1;;
[%%expect{|
Line 1, characters 4-19: refined environment entry: dup_constructor :
  int{ _ > 0 }
Lines 2-4, characters 2-23: refinement obligation: int{ _ > 0 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-datatypes ((ab_1 0)) (
  ((ab_1.A) (ab_1.B))))
(declare-datatypes ((cd_1 0)) (
  ((cd_1.A) (cd_1.C))))
(declare-const p_1 ab_1)
(declare-const q_1 cd_1)
(assert (! (= p_1 ab_1.A) :named h1))
(assert (! (= q_1 cd_1.A) :named h2))
(assert (not (bvsgt (_ bv1 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val dup_constructor : int{ _ > 0 } = 1
|}]

type r1 = { shared : int };;
[%%expect{|
type r1 = { shared : int; }
|}]

type r2 = { shared : bool };;
[%%expect{|
type r2 = { shared : bool; }
|}]

let dup_label : int{ _ > 0 } =
  let a : r1 = { shared = 1 } in
  let b : r2 = { shared = true } in
  ignore a; ignore b; 1;;
[%%expect{|
Line 1, characters 4-13: refined environment entry: dup_label : int{ _ > 0 }
Lines 2-4, characters 2-23: refinement obligation: int{ _ > 0 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-datatypes ((r1_1 0)) (
  ((mk_r1_1 (r1_1.shared (_ BitVec 63))))))
(declare-datatypes ((r2_1 0)) (
  ((mk_r2_1 (r2_1.shared Bool)))))
(declare-const a_1 r1_1)
(declare-const result/1 r1_1)
(declare-const b_1 r2_1)
(declare-const result/2 r2_1)
(assert (! (= a_1 result/1) :named h1))
(assert (! (= b_1 result/2) :named h2))
(assert (not (bvsgt (_ bv1 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val dup_label : int{ _ > 0 } = 1
|}]

let parametric_constructor : int{ _ >= 0 } =
  let sx = Some 1 in
  ignore sx; 0;;
[%%expect{|
Line 1, characters 4-26: refined environment entry: parametric_constructor :
  int{ _ >= 0 }
Lines 2-3, characters 2-14: refinement obligation: int{ _ >= 0 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-datatypes ((option<Bv63> 0)) (
  ((option.None<Bv63>) (option.Some<Bv63> (option.Some.0<Bv63> (_ BitVec 63))))))
(declare-const sx_1 option<Bv63>)
(assert (! (= sx_1 (option.Some<Bv63> (_ bv1 63))) :named h1))
(assert (not (bvsge (_ bv0 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val parametric_constructor : int{ _ >= 0 } = 0
|}]

(* --- bigint-sort: Stdlib.Bigint.t is the mathematical Int sort ------------- *)

let bigint_sort (b : Stdlib.Bigint.t) : int{ _ > 0 } =
  let _same = b in
  1;;
[%%expect{|
Lines 2-3, characters 2-3: refinement obligation: int{ _ > 0 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-const _same_1 Int)
(declare-const b_1 Int)
(assert (! (= _same_1 b_1) :named h1))
(assert (not (bvsgt (_ bv1 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val bigint_sort : Bigint.t -> int{ _ > 0 } = <fun>
|}]

(* --- bigint-shadow: recognition is by unit identity, not spelling ---------- *)
(* A module literally named Stdlib__Bigint is not Stdlib.Bigint: its variant
   declares as its own datatype, never the mathematical Int (the control is
   bigint-sort above, whose sorts stay Int). *)

let bigint_shadow : int{ _ > 0 } =
  let module Stdlib__Bigint = struct type t = Zero | One end in
  let v = Stdlib__Bigint.Zero in
  ignore v; 1;;
[%%expect{|
Line 1, characters 4-17: refined environment entry: bigint_shadow :
  int{ _ > 0 }
Lines 2-4, characters 2-13: refinement obligation: int{ _ > 0 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-datatypes ((Stdlib__Bigint_1.t 0)) (
  ((Stdlib__Bigint_1.t.Zero) (Stdlib__Bigint_1.t.One))))
(declare-const v_1 Stdlib__Bigint_1.t)
(assert (! (= v_1 Stdlib__Bigint_1.t.Zero) :named h1))
(assert (not (bvsgt (_ bv1 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val bigint_shadow : int{ _ > 0 } = 1
|}]

(* --- mutable-record: an uninterpreted sort, never a datatype --------------- *)
(* A datatype's constructor would equate two states of the record that
   differ only across a write (extensional equality); opacity merely loses
   completeness, so a record with a mutable field declares as a sort. *)

type mrec = { mutable cur : int };;
[%%expect{|
type mrec = { mutable cur : int; }
|}]

let mutable_record : int{ _ > 0 } =
  let mr = { cur = 1 } in
  ignore mr; 1;;
[%%expect{|
Line 1, characters 4-18: refined environment entry: mutable_record :
  int{ _ > 0 }
Lines 2-3, characters 2-14: refinement obligation: int{ _ > 0 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-sort mrec_1 0)
(declare-const mr_1 mrec_1)
(declare-const result/1 mrec_1)
(assert (! (= mr_1 result/1) :named h1))
(assert (not (bvsgt (_ bv1 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val mutable_record : int{ _ > 0 } = 1
|}]

(* --- sealed-datatype: concrete inside, uninterpreted sort outside ---------- *)
(* The sd_env pair makes the two baselines differ: a t-sorted value reaches
   the signature through the let equality, printing a declare-datatypes
   inside the module and a declare-sort outside it. *)

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
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-datatypes ((t_1 0)) (
  ((mk_t_1 (t_1.field (_ BitVec 63))))))
(declare-const _s_1 t_1)
(declare-const result/1 t_1)
(assert (! (= _s_1 result/1) :named h1))
(assert (not (bvsgt (_ bv1 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
module Sealed : sig type t val mk : int -> t val sd_env : int{ _ > 0 } end
|}]

let sd_env_out : int{ _ > 0 } = let _s = Sealed.mk 2 in 2;;
[%%expect{|
Line 1, characters 4-14: refined environment entry: sd_env_out : int{ _ > 0 }
Line 1, characters 32-57: refinement obligation: int{ _ > 0 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-sort Sealed_1.t 0)
(declare-const _s_1 Sealed_1.t)
(declare-const result/1 Sealed_1.t)
(assert (! (= _s_1 result/1) :named h1))
(assert (not (bvsgt (_ bv2 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val sd_env_out : int{ _ > 0 } = 2
|}]

(* --- recursive-knot-hole: a RECORDED KNOWN HOLE, pinned --------------------- *)
(* KNOWN HOLE (owner-deferred to a later piece): the h1 hypothesis below IS
   the goal — the right-hand side's lowering resolves knot_false at its own
   declared refined type and deposits the predicate the obligation must
   establish (design-docs/vc-generation.md, Known holes; verdicts pinned in
   vc-z3.ml).  This baseline pins the self-justifying query so the later
   piece has a discriminating test to flip: the corrected behaviour is this
   query with no hypothesis line.  The type carries a base constructor so
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
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(assert (! false :named h1))
(assert (not false))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val knot_false : knot{ false } = K <cycle>
|}]

(* --- local-knot: the local let-rec route does NOT self-justify -------------- *)
(* A local group's recursive occurrences carry the unrefined payload type in
   the right-hand sides' environment, so no self-deposit fires: each rhs
   obligation below is the bare goal (no hypothesis line), where the
   module-level knot above shows h1.  The body still receives the binder
   facts (the group's declared predicates).  If a hypothesis line ever
   appears on the rhs queries, the self-justification hole has spread to
   the local route — re-record it in the design doc's known-holes entry. *)

let local_knot : int{ 0 > 1 } =
  let rec a : knot{ false } = K b
  and b : knot{ false } = K a in
  let _ = a in 0;;
[%%expect{|
Line 1, characters 4-14: refined environment entry: local_knot : int{ 0 > 1 }
Lines 2-4, characters 2-16: refinement obligation: int{ 0 > 1 }
Line 2, characters 30-33: refinement obligation: knot{ false }
Line 3, characters 26-29: refinement obligation: knot{ false }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(assert (not false))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(assert (not false))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(assert (! false :named h1))
(assert (! false :named h2))
(assert (not (bvsgt (_ bv0 63) (_ bv1 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val local_knot : int{ 0 > 1 } = 0
|}]

(* --- cross-obligation: both queries carry the shared-ident hypothesis ------ *)
(* Marker and arrow domain impose on the same subject; each pending owns its
   seen-idents snapshot, so assembling the first goal (which resolves w3)
   does not rob the second of its (= w3 3) hypothesis. *)

let w3 : int{ _ = 3 } = 3;;
[%%expect{|
Line 1, characters 4-6: refined environment entry: w3 : int{ _ = 3 }
Line 1, characters 24-25: refinement obligation: int{ _ = 3 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(assert (not (= (_ bv3 63) (_ bv3 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val w3 : int{ _ = 3 } = 3
|}]

let cross_f (x : int{ _ > w3 }) = x;;
[%%expect{|
val cross_f : int{ _ > w3 } -> int = <fun>
|}]

let cross_probe = cross_f (5 : int{ _ > w3 - 1 });;
[%%expect{|
Line 1, characters 27-28: refinement obligation: int{ _ > (w3 - 1) }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-const w3_1 (_ BitVec 63))
(assert (! (= w3_1 (_ bv3 63)) :named h1))
(assert (not (bvsgt (_ bv5 63) (bvsub w3_1 (_ bv1 63)))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-const w3_1 (_ BitVec 63))
(assert (! (= w3_1 (_ bv3 63)) :named h1))
(assert (not (bvsgt (_ bv5 63) w3_1)))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val cross_probe : int = 5
|}]

(* --- shadowed-modules: dotted paths keep the head's stamp ------------------- *)
(* Two local modules M with distinct t in one obligation: the symbol
   allocator stamps the head of a dotted path, so they declare as two
   datatypes (canonically M_1.t and M_2.t) instead of colliding into one
   declaration with an undeclared constructor. *)

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
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-datatypes ((M_1.t 0)) (
  ((M_1.t.A))))
(declare-datatypes ((M_2.t 0)) (
  ((M_2.t.B))))
(declare-const p_1 M_1.t)
(declare-const q_1 M_2.t)
(assert (! (= p_1 M_1.t.A) :named h1))
(assert (! (= q_1 M_2.t.B) :named h2))
(assert (not (bvsgt (_ bv1 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val shadowed_modules : int{ _ > 0 } = 1
|}]

(* --- nested-instance: canonical stamps inside instance suffixes ------------- *)
(* The unrelated preceding binding shifts the raw Ident stamps; the instance
   suffix renumbers with the sort it names, so the pinned query carries
   wrap_1<leaf_1> and is byte-stable against unrelated edits above it. *)

type leaf = L;;
[%%expect{|
type leaf = L
|}]

type 'a wrap = W of 'a;;
[%%expect{|
type 'a wrap = W of 'a
|}]

let nested_unrelated = 0;;
[%%expect{|
val nested_unrelated : int = 0
|}]

let nested_instance : int{ _ > 0 } =
  let v : leaf wrap = W L in
  ignore v; 1;;
[%%expect{|
Line 1, characters 4-19: refined environment entry: nested_instance :
  int{ _ > 0 }
Lines 2-3, characters 2-13: refinement obligation: int{ _ > 0 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-datatypes ((leaf_1 0)) (
  ((leaf_1.L))))
(declare-datatypes ((wrap_1<leaf_1> 0)) (
  ((wrap_1.W<leaf_1> (wrap_1.W.0<leaf_1> leaf_1)))))
(declare-const v_1 wrap_1<leaf_1>)
(assert (! (= v_1 (wrap_1.W<leaf_1> leaf_1.L)) :named h1))
(assert (not (bvsgt (_ bv1 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val nested_instance : int{ _ > 0 } = 1
|}]

(* --- operator-stamp: delimiter characters inside an operator's own name ----- *)
(* The canonical renumbering knows [< > .] are name material inside an
   operator identifier: the stamp in [+>_n<...>] renumbers with its base
   instead of leaking raw.  The unrelated declaration above the operator is
   the point of the fixture — pre-fix, its presence shifted the leaked
   stamp, so this baseline is byte-stable exactly because the scanner now
   renumbers it. *)

let op_unrelated = 0;;
[%%expect{|
val op_unrelated : int = 0
|}]

let ( +> ) @ total = fun x y -> x + y;;
[%%expect{|
val ( +> ) : int -> int -> int = <fun>
|}]

let op_stamp : int{ _ > 0 } = 1 +> 2;;
[%%expect{|
Line 1, characters 4-12: refined environment entry: op_stamp : int{ _ > 0 }
Line 1, characters 30-36: refinement obligation: int{ _ > 0 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-fun |+>_1<Bv63,Bv63,Bv63>| ((_ BitVec 63) (_ BitVec 63)) (_ BitVec 63))
(assert (not (bvsgt (|+>_1<Bv63,Bv63,Bv63>| (_ bv1 63) (_ bv2 63)) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val op_stamp : int{ _ > 0 } = 3
|}]

(* --- poly-value-sorts: one polymorphic value at two ground sorts ------------ *)
(* Value symbols are sort-sensitive, the discipline function symbols follow:
   [nil] read at [int list] and at [bool list] in one obligation is two
   constants ([nil_1<list<Bv63>>], [nil_1<list<Bool>>]), never one name
   declared at the first sort and reused ill-sorted at the second (which
   the solver rejects).  Verdict in vc-z3.ml. *)

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
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-datatypes ((list<Bv63> 0)) (
  ((|list.[]<Bv63>|) (|list.::<Bv63>| (|list.::.0<Bv63>| (_ BitVec 63)) (|list.::.1<Bv63>| list<Bv63>)))))
(declare-datatypes ((list<Bool> 0)) (
  ((|list.[]<Bool>|) (|list.::<Bool>| (|list.::.0<Bool>| Bool) (|list.::.1<Bool>| list<Bool>)))))
(declare-const a_1 list<Bv63>)
(declare-const nil_1<list<Bv63>> list<Bv63>)
(declare-const b_1 list<Bool>)
(declare-const nil_1<list<Bool>> list<Bool>)
(assert (! (= a_1 nil_1<list<Bv63>>) :named h1))
(assert (! (= b_1 nil_1<list<Bool>>) :named h2))
(assert (not (bvsgt (_ bv1 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val poly_value_sorts : int{ _ > 0 } = 1
|}]

(* --- selfish-cycle: a baseless recursive variant is an uninterpreted sort --- *)
(* [type selfish = C of selfish] has no reachable base constructor, so as an
   SMT datatype it would be rejected as non-well-founded (and a strictly
   inductive reading would make the sort empty, vacating every fact over
   its values).  The OCaml type is inhabited via cycles, so it lowers to a
   declared uninterpreted sort: opaque values, constructor reasoning
   deferred with cyclic data (design-docs/vc-generation.md, Signature
   assembly and datatypes).  Verdict in vc-z3.ml. *)

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
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-sort selfish_1 0)
(declare-const y_1 selfish_1)
(declare-const selfish_cycle_1 selfish_1)
(assert (! (= y_1 selfish_cycle_1) :named h1))
(assert (not (bvsgt (_ bv1 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val selfish_benign : int{ _ > 0 } = 1
|}]

(* --- mutual-datatype: a mutual group WITH a base case stays a datatype ------ *)
(* Well-foundedness is a property of the group, not the single declaration:
   [odd] has no nullary constructor but grounds through [even]'s [Zero], so
   the pair declares as one datatype group. *)

type even = Zero | Succ_e of odd
and odd = Succ_o of even;;
[%%expect{|
type even = Zero | Succ_e of odd
and odd = Succ_o of even
|}]

let mutual_datatype : int{ _ > 0 } = let v = Succ_o Zero in ignore v; 1;;
[%%expect{|
Line 1, characters 4-19: refined environment entry: mutual_datatype :
  int{ _ > 0 }
Line 1, characters 37-71: refinement obligation: int{ _ > 0 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-datatypes ((even_1 0) (odd_1 0)) (
  ((even_1.Zero) (even_1.Succ_e (even_1.Succ_e.0 odd_1)))
  ((odd_1.Succ_o (odd_1.Succ_o.0 even_1)))))
(declare-const v_1 odd_1)
(assert (! (= v_1 (odd_1.Succ_o even_1.Zero)) :named h1))
(assert (not (bvsgt (_ bv1 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val mutual_datatype : int{ _ > 0 } = 1
|}]
