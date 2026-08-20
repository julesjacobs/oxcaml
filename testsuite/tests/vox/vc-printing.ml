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

(* --- short-circuit: the && left-operand hypothesis ------------------------ *)

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
  ((mk_box_1 (first_pos (_ BitVec 63)) (second (_ BitVec 63))))))
(declare-const b_1 box_1)
(assert (! (bvsgt (first_pos b_1) (_ bv0 63)) :named h1))
(assert (not (bvsgt (first_pos b_1) (_ bv0 63))))
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

(* --- sealed-datatype: concrete inside, uninterpreted sort outside ---------- *)
(* The sd_env pair is what makes the two baselines differ: a t-sorted value
   reaches the signature through the let equality, printing a
   declare-datatypes inside the module and a declare-sort outside it.  The
   t{ true } impositions pin acceptance but their goals mention no symbol,
   so their queries carry no signature at all. *)

module Sealed : sig
  type t
  val mk : int -> t
  val sd_in : t{ true }
  val sd_env : int{ _ > 0 }
end = struct
  type t = { field : int }
  let mk field = { field }
  let sd_in : t{ true } = mk 1
  let sd_env : int{ _ > 0 } = let _s = mk 1 in 1
end;;
[%%expect{|
Line 9, characters 6-11: refined environment entry: sd_in : t{ true }
Line 10, characters 6-12: refined environment entry: sd_env : int{ _ > 0 }
Line 9, characters 26-30: refinement obligation: t{ true }
Line 10, characters 30-48: refinement obligation: int{ _ > 0 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(assert (not true))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-datatypes ((t_1 0)) (
  ((mk_t_1 (field (_ BitVec 63))))))
(declare-const _s_1 t_1)
(declare-const result/1 t_1)
(assert (! (= _s_1 result/1) :named h1))
(assert (not (bvsgt (_ bv1 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
module Sealed :
  sig
    type t
    val mk : int -> t
    val sd_in : t{ true }
    val sd_env : int{ _ > 0 }
  end
|}]

let sd_out : Sealed.t{ true } = Sealed.mk 2;;
[%%expect{|
Line 1, characters 4-10: refined environment entry: sd_out : Sealed.t{ true }
Line 1, characters 32-43: refinement obligation: Sealed.t{ true }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(assert (not true))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val sd_out : Sealed.t{ true } = <abstr>
|}]

let sd_env_out : int{ _ > 0 } = let _s = Sealed.mk 2 in 2;;
[%%expect{|
Line 1, characters 4-14: refined environment entry: sd_env_out : int{ _ > 0 }
Line 1, characters 32-57: refinement obligation: int{ _ > 0 }
(set-option :timeout 10000)
(set-option :produce-unsat-cores true)
(declare-sort Sealed.t 0)
(declare-const _s_1 Sealed.t)
(declare-const result/1 Sealed.t)
(assert (! (= _s_1 result/1) :named h1))
(assert (not (bvsgt (_ bv2 63) (_ bv0 63))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
val sd_env_out : int{ _ > 0 } = 2
|}]
