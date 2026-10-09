(* Adversarial unit + property tests for the ADR-0005 EUF THEORY adapter
   ({!Oxsmt_euf.Euf_adapter}). Drives the adapter through the frozen THEORY public surface
   ONLY (create / register_atom / assert_lit / check / explain / push / pop / model) — no
   peeking at adapter internals — and cross-checks every observable against an INDEPENDENT
   naive congruence closure written here from the spec (union-find + brute-force O(n^2)
   congruence, with predicate atoms encoded against true/false like the adapter and a
   standing true<>false disequality). Determinism uses a fixed-seed xorshift, no
   wall-clock.

   The adapter is a thin relabeling layer over the engine (which self-checks its own
   explanations); this suite re-verifies the adapter's SOUNDNESS from public outputs:
   - conflict/propagation explanations are replayed into the naive closure and must entail
     the claimed (dis)equality;
   - every returned premise is a literal the driver actually asserted (no fabricated or
     axiom-leaked premise — the reserved true<>false axiom must never surface);
   - the published model's induced equality matches the oracle, and predicate truth values
     are correct;
   - push/pop restores state exactly (deep nesting, pop-below-propagation, and
     assert-after-pop with a DIFFERENT assertion — no stale state). *)

open Oxsmt_core
module A = Oxsmt_euf.Euf_adapter

let checks = ref 0
let failures = ref 0

let check name cond =
  incr checks;
  if not cond
  then (
    incr failures;
    Printf.printf "  FAIL %s\n" name)
;;

let check_raises name f =
  incr checks;
  match f () with
  | exception _ -> ()
  | _ ->
    incr failures;
    Printf.printf "  FAIL %s (expected an exception, got none)\n" name
;;

(* ------------------------------------------------------------------ *)
(* Deterministic PRNG: xorshift64*, fixed seed. *)

let rng = ref 0x1E3779B97F4A7C15

let rand_bits () =
  let x = !rng in
  let x = x lxor (x lsr 12) in
  let x = x lxor (x lsl 25) in
  let x = x lxor (x lsr 27) in
  rng := x;
  x * 0x2545F4914F6CDD1D land max_int
;;

let rand_int n = rand_bits () mod n
let set_seed s = rng := s

(* ------------------------------------------------------------------ *)
(* Env / term-building helpers. U uninterpreted sort; f,g : U->U; p : U->Bool; constants
   c0.. : U; q : Bool nullary predicate. *)

let make_env () =
  let env = Env.create () in
  let u = Env.declare_sort env "U" in
  let usort = Sort.uninterpreted u in
  let unary name = Env.declare_fun env name (Rank.create [ usort ] usort) in
  let pred name = Env.declare_fun env name (Rank.create [ usort ] Sort.bool) in
  let konst name = Env.declare_fun env name (Rank.create [] usort) in
  let bpred name = Env.declare_fun env name (Rank.create [] Sort.bool) in
  env, usort, unary, pred, konst, bpred
;;

(* A test harness bundling the adapter with an atom<->term registry so premises can be
   decoded back into facts for the oracle. *)
type harness =
  { adapter : A.t
  ; alloc : Atom.allocator
  ; term_of_atom : Term.t Atom.Table.t
  ; mutable asserted : Lit.Set.t
  }

let make_harness env ctx =
  { adapter = A.create ctx env
  ; alloc = Atom.create_allocator ()
  ; term_of_atom = Atom.Table.create 64
  ; asserted = Lit.Set.empty
  }
;;

(* Mint a fresh Atom for [term], register it, record the mapping. Idempotent-ish for the
   test: re-registering the same term mints a NEW atom (mirrors a fresh clausifier var);
   callers that want one atom per term keep the returned handle. *)
let reg h term =
  let atom = Atom.fresh h.alloc in
  A.register_atom h.adapter atom term;
  Atom.Table.replace h.term_of_atom atom term;
  atom
;;

let assert_lit h lit =
  h.asserted <- Lit.Set.add lit h.asserted;
  A.assert_lit h.adapter lit
;;

(* Drive [check] to a terminal (Sat/Conflict) result, treating Propagations as "continue"
   (the engine loop). Bounded to guard against a bug producing endless propagations. *)
let settle h effort =
  let rec go fuel =
    if fuel = 0 then failwith "settle: propagation did not converge";
    match A.check h.adapter effort with
    | Theory.Propagations (_ :: _) -> go (fuel - 1)
    | other -> other
  in
  go 1000
;;

(* ------------------------------------------------------------------ *)
(* Independent naive closure over a fixed universe (shares no code with the engine). *)

module Naive = struct
  type t =
    { univ : Term.t array
    ; idx : int Term.Table.t
    ; parent : int array
    }

  let build univ =
    let idx = Term.Table.create 64 in
    Array.iteri (fun i t -> Term.Table.replace idx t i) univ;
    { univ; idx; parent = Array.init (Array.length univ) (fun i -> i) }
  ;;

  let rec find t i = if t.parent.(i) = i then i else find t t.parent.(i)

  let union t i j =
    let ri = find t i
    and rj = find t j in
    if ri <> rj then t.parent.(ri) <- rj
  ;;

  let index t term = Term.Table.find t.idx term

  (* seed unions then congruence-close (App congruence over the whole universe). *)
  let saturate t (unions : (Term.t * Term.t) list) =
    Array.iteri (fun i _ -> t.parent.(i) <- i) t.univ;
    List.iter (fun (a, b) -> union t (index t a) (index t b)) unions;
    let n = Array.length t.univ in
    let changed = ref true in
    while !changed do
      changed := false;
      for i = 0 to n - 1 do
        for j = i + 1 to n - 1 do
          match Term.(t.univ.(i).node, t.univ.(j).node) with
          | Term.App (si, ai), Term.App (sj, aj)
            when Symbol.equal si sj
                 && Iarr.length ai = Iarr.length aj
                 && find t i <> find t j ->
            let all = ref true in
            List.iteri
              (fun k arg ->
                 let bj = Iarr.get aj k in
                 if find t (index t arg) <> find t (index t bj) then all := false)
              (Iarr.to_list ai);
            if !all
            then (
              union t i j;
              changed := true)
          | _ -> ()
        done
      done
    done
  ;;

  let equal t a b = find t (index t a) = find t (index t b)
end

(* subterm closure of seeds, deterministically ordered by Term tag. *)
let closure (seeds : Term.t list) : Term.t array =
  let tbl = Term.Table.create 64 in
  let rec go (t : Term.t) =
    if not (Term.Table.mem tbl t)
    then (
      Term.Table.replace tbl t ();
      let kids =
        Term.(
          match t.node with
          | Bool_const _ | Int_const _ -> []
          | App (_, a) -> Iarr.to_list a
          | Arith { coeffs; _ } -> List.map fst (Iarr.to_list coeffs)
          | Le a -> [ a ]
          | Eq (a, b) -> [ a; b ]
          | Not a -> [ a ]
          | And a | Or a -> Iarr.to_list a
          | Ite (c, a, b) -> [ c; a; b ])
      in
      List.iter go kids)
  in
  List.iter go seeds;
  let arr = Term.Table.fold (fun t () acc -> t :: acc) tbl [] |> Array.of_list in
  Array.sort Term.compare arr;
  arr
;;

(* Decode an asserted literal into the (a,b) union it induces for the oracle, given the
   true/false constants. A negative equality / predicate=false is NOT a union — it is a
   disequality, returned separately. *)
type fact =
  | Union of Term.t * Term.t
  | Diseq of Term.t * Term.t

let fact_of_lit h ~true_c ~false_c lit =
  let atom = Lit.atom lit in
  let term = Atom.Table.find h.term_of_atom atom in
  let pos = Lit.sign lit in
  match Theory_view.atom term with
  | Theory_view.Equality (a, b) -> if pos then Union (a, b) else Diseq (a, b)
  | Theory_view.Predicate (_, _) | Theory_view.Bool_lit _ ->
    if pos then Union (term, true_c) else Union (term, false_c)
  | Theory_view.Le_zero _ -> assert false
;;

(* ================================================================== *)
(* 1. Textbook refutation via the adapter: f(x)=a, x=y, f(y)<>a. *)

let test_textbook () =
  let env, _u, unary, _pred, konst, _bpred = make_env () in
  let ctx = Context.create env in
  let a = Context.const ctx (konst "a") in
  let x = Context.const ctx (konst "x") in
  let y = Context.const ctx (konst "y") in
  let f = unary "f" in
  let fx = Context.app ctx f [ x ] in
  let fy = Context.app ctx f [ y ] in
  let h = make_harness env ctx in
  let a_fxa = reg h (Context.eq ctx fx a) in
  let a_xy = reg h (Context.eq ctx x y) in
  let a_fya = reg h (Context.eq ctx fy a) in
  let l1 = Lit.make a_fxa true in
  let l2 = Lit.make a_xy true in
  let l3 = Lit.make a_fya false in
  assert_lit h l1;
  assert_lit h l2;
  assert_lit h l3;
  match settle h Theory.Final with
  | Theory.Conflict e ->
    let prem = Lit.Set.of_list e.Explanation.premises in
    check
      "textbook: premises = {l1;l2;l3}"
      (Lit.Set.equal prem (Lit.Set.of_list [ l1; l2; l3 ]));
    check "textbook: rule = Euf_congruence" (e.Explanation.rule = Euf_congruence);
    check
      "textbook: no premise outside asserted set (no axiom leak)"
      (Lit.Set.subset prem h.asserted);
    check "textbook: premises non-empty" (e.Explanation.premises <> [])
  | _ -> check "textbook: expected a conflict" false
;;

(* ------------------------------------------------------------------ *)
(* 2. Predicate atoms: p(x), p(y), x=y with +p(x), -p(y) => conflict via true<>false. *)

let test_predicate_conflict () =
  let env, _u, _unary, pred, konst, _bpred = make_env () in
  let ctx = Context.create env in
  let x = Context.const ctx (konst "x") in
  let y = Context.const ctx (konst "y") in
  let p = pred "p" in
  let h = make_harness env ctx in
  let a_px = reg h (Context.app ctx p [ x ]) in
  let a_py = reg h (Context.app ctx p [ y ]) in
  let a_xy = reg h (Context.eq ctx x y) in
  let l_px = Lit.make a_px true in
  let l_py = Lit.make a_py false in
  let l_xy = Lit.make a_xy true in
  assert_lit h l_px;
  assert_lit h l_py;
  assert_lit h l_xy;
  match settle h Theory.Final with
  | Theory.Conflict e ->
    let prem = Lit.Set.of_list e.Explanation.premises in
    check
      "pred-conflict: premises = {+p(x);-p(y);x=y}"
      (Lit.Set.equal prem (Lit.Set.of_list [ l_px; l_py; l_xy ]));
    check
      "pred-conflict: no axiom leak (premises subset asserted)"
      (Lit.Set.subset prem h.asserted);
    check "pred-conflict: rule = Euf_congruence" (e.Explanation.rule = Euf_congruence)
  | _ -> check "pred-conflict: expected a conflict" false
;;

(* 2b. A Bool constant atom asserted against its own truth is an immediate conflict. *)

let test_bool_lit_conflict () =
  let env, _u, _unary, _pred, _konst, _bpred = make_env () in
  let ctx = Context.create env in
  let h = make_harness env ctx in
  (* atom is the term [true]; asserting it NEGATIVE says true=false. *)
  let a_true = reg h (Context.bool_const ctx true) in
  let l = Lit.make a_true false in
  assert_lit h l;
  match settle h Theory.Final with
  | Theory.Conflict e ->
    check "bool-lit: premises = {l}" (e.Explanation.premises = [ l ]);
    check
      "bool-lit: no axiom leak"
      (Lit.Set.subset (Lit.Set.of_list e.premises) h.asserted)
  | _ -> check "bool-lit: expected a conflict" false
;;

(* ------------------------------------------------------------------ *)
(* 3. Propagation of a watched equality atom + lazy explanation via the adapter. *)

let test_propagation () =
  let env, _u, _unary, _pred, konst, _bpred = make_env () in
  let ctx = Context.create env in
  let a = Context.const ctx (konst "a") in
  let b = Context.const ctx (konst "b") in
  let c = Context.const ctx (konst "c") in
  let h = make_harness env ctx in
  let a_ab = reg h (Context.eq ctx a b) in
  let a_ac = reg h (Context.eq ctx a c) in
  let a_cb = reg h (Context.eq ctx c b) in
  (* assert a=c, c=b; then (a=b) is theory-implied true. *)
  assert_lit h (Lit.make a_ac true);
  assert_lit h (Lit.make a_cb true);
  match A.check h.adapter Theory.Propagate with
  | Theory.Propagations lits ->
    let ab_pos = Lit.make a_ab true in
    check "prop: (a=b) propagated true" (List.exists (Lit.equal ab_pos) lits);
    (* explain the propagated literal; premises must be a subset of asserted, and
       replay-suffices for a=b. *)
    let e = A.explain h.adapter ab_pos in
    let prem = Lit.Set.of_list e.Explanation.premises in
    check "prop: explanation subset asserted" (Lit.Set.subset prem h.asserted);
    check "prop: explanation non-empty" (e.Explanation.premises <> []);
    (* determinism C2: same explanation twice *)
    let e2 = A.explain h.adapter ab_pos in
    check "prop: explain deterministic" (e.Explanation.premises = e2.Explanation.premises)
  | _ -> check "prop: expected propagations" false
;;

(* 3b. ⊤/⊥ flow-back: assert +p(a) and a=b; then p(b) is theory-implied TRUE by congruence
   ([p(a) ~ true_const], [p(a) ~ p(b)] => [p(b) ~ true_const]) and must be PROPAGATED as a
   literal — not merely caught reactively via the [true <> false] axiom on a later wrong
   guess (that path is {!test_predicate_conflict}). DISCRIMINATION: before predicate
   watching, the adapter propagated only Eq atoms, so [check] returned no [p(b)] literal
   and the [List.exists] below failed. *)

let test_predicate_propagation () =
  let env, _u, _unary, pred, konst, _bpred = make_env () in
  let ctx = Context.create env in
  let a = Context.const ctx (konst "a") in
  let b = Context.const ctx (konst "b") in
  let p = pred "p" in
  let h = make_harness env ctx in
  let a_pa = reg h (Context.app ctx p [ a ]) in
  let a_pb = reg h (Context.app ctx p [ b ]) in
  let a_ab = reg h (Context.eq ctx a b) in
  ignore a_pa;
  (* assert +p(a) and a=b; the first check reports p(a) self-true AND the congruence
     flow-back p(b)=true. *)
  assert_lit h (Lit.make a_pa true);
  assert_lit h (Lit.make a_ab true);
  match A.check h.adapter Theory.Propagate with
  | Theory.Propagations lits ->
    let pb_pos = Lit.make a_pb true in
    check "pred-prop: p(b) propagated true" (List.exists (Lit.equal pb_pos) lits);
    let e = A.explain h.adapter pb_pos in
    let prem = Lit.Set.of_list e.Explanation.premises in
    check "pred-prop: explanation subset asserted" (Lit.Set.subset prem h.asserted);
    check "pred-prop: explanation non-empty" (e.Explanation.premises <> []);
    check "pred-prop: rule = Euf_congruence" (e.Explanation.rule = Euf_congruence);
    (* determinism C2: same explanation twice *)
    let e2 = A.explain h.adapter pb_pos in
    check
      "pred-prop: explain deterministic"
      (e.Explanation.premises = e2.Explanation.premises)
  | _ -> check "pred-prop: expected propagations" false
;;

(* 3c. Predicate watch survives push/pop + re-assertion (belt-and-suspenders for the
   mid-solve-registration / pop lifecycle probe): assert +p(a) at base, push+assert a=b →
   p(b) propagates true; pop drops a=b so p(b) is no longer entailed (no stale
   re-propagation); re-asserting a=b re-derives p(b) true (the watch was not lost by the
   pop). pa/pb/ab are registered at base, so pop of the inner frame does not truncate
   their e-nodes — the watch persists exactly as an Eq watch would. *)

let test_predicate_pushpop_restore () =
  let env, _u, _unary, pred, konst, _bpred = make_env () in
  let ctx = Context.create env in
  let a = Context.const ctx (konst "a") in
  let b = Context.const ctx (konst "b") in
  let p = pred "p" in
  let h = make_harness env ctx in
  let a_pa = reg h (Context.app ctx p [ a ]) in
  let a_pb = reg h (Context.app ctx p [ b ]) in
  let a_ab = reg h (Context.eq ctx a b) in
  let pb_pos = Lit.make a_pb true in
  let propagated_pb () =
    match A.check h.adapter Theory.Propagate with
    | Theory.Propagations lits -> List.exists (Lit.equal pb_pos) lits
    | _ -> false
  in
  (* base: +p(a) *)
  assert_lit h (Lit.make a_pa true);
  ignore (A.check h.adapter Theory.Propagate : Theory.check_result);
  (* frame 1: a=b => p(b) propagates true *)
  A.push h.adapter;
  assert_lit h (Lit.make a_ab true);
  check "pred-pushpop: p(b) propagated true under a=b" (propagated_pb ());
  (* pop: a=b retracted; p(b) no longer entailed — must NOT be (stale-)re-propagated, and
     no conflict. *)
  A.pop h.adapter 1;
  h.asserted <- Lit.Set.remove (Lit.make a_ab true) h.asserted;
  check
    "pred-pushpop: no stale p(b) after pop"
    (match A.check h.adapter Theory.Final with
     | Theory.Conflict _ -> false
     | Theory.Sat | Theory.Split _ -> true
     | Theory.Propagations lits -> not (List.exists (Lit.equal pb_pos) lits));
  (* re-assert a=b after the pop: the watch survived, p(b) is entailed true again. *)
  assert_lit h (Lit.make a_ab true);
  check "pred-pushpop: p(b) re-propagated true after re-assert" (propagated_pb ())
;;

(* 3d. LATE BINDING (codex MEDIUM): a predicate first seen via [internalize_term]
   (boundary/buried — engine watch created, no atom bound) can have its one-shot propagate
   flip consumed (w_reported advanced) and then DROPPED for lacking an atom. A later
   [register_atom] must RE-ARM the watch so the currently-entailed truth re-propagates;
   without the re-arm, [register]'s idempotent early return leaves w_reported stale and
   the predicate's theory propagation is permanently lost. Sequence: internalize p(a);
   bind + assert p(b)=true and a=b; a [check] consumes p(a)'s flip unbound (dropped); THEN
   register_atom p(a); the next [check] must still propagate +p(a). DISCRIMINATION: RED
   without the re-arm (no p(a) propagation ever), GREEN with it. *)

let test_predicate_late_binding () =
  let env, _u, _unary, pred, konst, _bpred = make_env () in
  let ctx = Context.create env in
  let a = Context.const ctx (konst "a") in
  let b = Context.const ctx (konst "b") in
  let p = pred "p" in
  let pa_term = Context.app ctx p [ a ] in
  let h = make_harness env ctx in
  (* p(a) first appears as a boundary-only internalization: engine watch created, NO atom. *)
  A.internalize_term h.adapter pa_term;
  let a_pb = reg h (Context.app ctx p [ b ]) in
  let a_ab = reg h (Context.eq ctx a b) in
  (* p(b)=true and a=b ⇒ p(a) entailed true by congruence. *)
  assert_lit h (Lit.make a_pb true);
  assert_lit h (Lit.make a_ab true);
  (* a check while p(a) is still unbound: the engine reports p(a)'s flip, the adapter
     drops it (no atom) and the engine's w_reported for p(a) is now consumed. *)
  ignore (A.check h.adapter Theory.Propagate : Theory.check_result);
  (* NOW p(a) surfaces as a real atom (e.g. a mid-solve lemma instance). *)
  let a_pa = Atom.fresh h.alloc in
  A.register_atom h.adapter a_pa pa_term;
  Atom.Table.replace h.term_of_atom a_pa pa_term;
  (* the currently-entailed truth of p(a) must re-propagate. *)
  let pa_pos = Lit.make a_pa true in
  (match A.check h.adapter Theory.Propagate with
   | Theory.Propagations lits ->
     check
       "pred-latebind: p(a) propagated true after late register_atom"
       (List.exists (Lit.equal pa_pos) lits);
     let e = A.explain h.adapter pa_pos in
     check
       "pred-latebind: explanation subset asserted"
       (Lit.Set.subset (Lit.Set.of_list e.Explanation.premises) h.asserted);
     check "pred-latebind: explanation non-empty" (e.Explanation.premises <> [])
   | Theory.Sat | Theory.Split _ ->
     check "pred-latebind: p(a) propagated true after late register_atom" false
   | Theory.Conflict _ -> check "pred-latebind: unexpected conflict" false);
  ignore (a_ab, a_pb)
;;

(* 3e. LATE-BINDING POP RECURRENCE (codex MED / board #161). The register-time re-arm (3d)
   is TRAILED, so a [pop] below the binding frame restores the bound predicate watch's
   stale [w_reported] while the atom binding survives ([t.watched] is monotone, not
   trailed) — the propagation is lost AGAIN. The CHECK-TIME re-arm (a [pop]-set flag
   drives one O(#watches) recovery pass) must re-deliver the currently-entailed truth. The
   entailing facts ([p(b)], [a=b]) live at BASE so they survive the pop; only the
   binding + its register-time re-arm were at the popped frame. DISCRIMINATION: each
   post-pop propagation is RED without the check-time re-arm (register-time re-arm alone
   does not survive the pop). *)
let test_predicate_latebind_pop_recurrence () =
  print_endline "predicate late-binding pop recurrence (#161):";
  (* internalize p(a) unbound; p(b)=[value] and a=b entail p(a)=[value]; bind p(a) at a
     pushed frame [depth] deep; pop back below the binding; the truth must re-propagate. *)
  let one_case ~name ~value ~depth =
    let env, _u, _unary, pred, konst, _bpred = make_env () in
    let ctx = Context.create env in
    let a = Context.const ctx (konst "a") in
    let b = Context.const ctx (konst "b") in
    let p = pred "p" in
    let pa_term = Context.app ctx p [ a ] in
    let h = make_harness env ctx in
    A.internalize_term h.adapter pa_term;
    let a_pb = reg h (Context.app ctx p [ b ]) in
    let a_ab = reg h (Context.eq ctx a b) in
    assert_lit h (Lit.make a_pb value);
    assert_lit h (Lit.make a_ab true);
    (* a check while p(a) is unbound: the engine reports its flip, the adapter drops it
       (no atom) and the engine's [w_reported] for p(a) is consumed. *)
    ignore (A.check h.adapter Theory.Propagate : Theory.check_result);
    for _ = 1 to depth do
      A.push h.adapter
    done;
    (* bind p(a) at the pushed (deep) frame — the register-time re-arm fires here. *)
    let a_pa = Atom.fresh h.alloc in
    A.register_atom h.adapter a_pa pa_term;
    Atom.Table.replace h.term_of_atom a_pa pa_term;
    let pa_lit = Lit.make a_pa value in
    let propagates_pa () =
      match A.check h.adapter Theory.Propagate with
      | Theory.Propagations lits -> List.exists (Lit.equal pa_lit) lits
      | _ -> false
    in
    check (name ^ ": propagated at binding frame") (propagates_pa ());
    (* pop back below the binding: the register-time re-arm is undone, the binding
       survives. Without the check-time re-arm the propagation is lost again (the
       recurrence). *)
    A.pop h.adapter depth;
    check
      (name ^ ": RE-propagated after pop-below-binding (recurrence fixed)")
      (propagates_pa ());
    (* the recovered propagation still explains soundly (non-empty, subset asserted). *)
    let e = A.explain h.adapter pa_lit in
    check
      (name ^ ": recovered explanation subset asserted")
      (Lit.Set.subset (Lit.Set.of_list e.Explanation.premises) h.asserted);
    check (name ^ ": recovered explanation non-empty") (e.Explanation.premises <> [])
  in
  (* (i) codex repro (true-valued, one frame); (ii) false-valued; (iii) deeper two-level
     push with the binding after the pushes and a pop of both frames. *)
  one_case ~name:"latebind-pop true" ~value:true ~depth:1;
  one_case ~name:"latebind-pop false" ~value:false ~depth:1;
  one_case ~name:"latebind-pop deep(2)" ~value:true ~depth:2
;;

(* ------------------------------------------------------------------ *)
(* 4. push/pop restoration: deep nesting, pop-below a conflict, and assert-after-pop with
   a DIFFERENT assertion (recheck-after-backtrack, no stale state). *)

let test_pushpop_restore () =
  let env, _u, unary, _pred, konst, _bpred = make_env () in
  let ctx = Context.create env in
  let a = Context.const ctx (konst "a") in
  let b = Context.const ctx (konst "b") in
  let c = Context.const ctx (konst "c") in
  let d = Context.const ctx (konst "d") in
  let f = unary "f" in
  let h = make_harness env ctx in
  let a_ab = reg h (Context.eq ctx a b) in
  let a_ac = reg h (Context.eq ctx a c) in
  let a_bc = reg h (Context.eq ctx b c) in
  let a_ad = reg h (Context.eq ctx a d) in
  (* also register a congruence pair to exercise use-list restore *)
  let _ = reg h (Context.eq ctx (Context.app ctx f [ a ]) (Context.app ctx f [ b ])) in
  (* base: assert a=b *)
  assert_lit h (Lit.make a_ab true);
  check
    "pushpop: base consistent"
    (match settle h Theory.Final with
     | Theory.Conflict _ -> false
     | _ -> true);
  (* frame 1: a=c ; frame 2: b<>c => conflict at level 2 *)
  A.push h.adapter;
  assert_lit h (Lit.make a_ac true);
  A.push h.adapter;
  assert_lit h (Lit.make a_bc false);
  check
    "pushpop: conflict at depth 2"
    (match settle h Theory.Final with
     | Theory.Conflict _ -> true
     | _ -> false);
  (* pop below the conflicting frame: back to just {a=b, a=c} *)
  A.pop h.adapter 1;
  check
    "pushpop: consistent after popping the conflicting frame"
    (match settle h Theory.Final with
     | Theory.Conflict _ -> false
     | _ -> true);
  (* pop to base: only a=b remains *)
  A.pop h.adapter 1;
  (* recheck-after-backtrack: assert a DIFFERENT fact (a=d). Must be consistent and must
     NOT resurrect the popped a=c / b<>c. *)
  assert_lit h (Lit.make a_ad true);
  (match settle h Theory.Final with
   | Theory.Conflict _ -> check "pushpop: post-backtrack re-assert consistent" false
   | _ -> check "pushpop: post-backtrack re-assert consistent" true);
  (* model must reflect a=b=d but NOT a=c (c was only ever equal under a popped frame) *)
  let m = A.model h.adapter in
  let v t = Model.value m t in
  check "pushpop: a=b in model" (v a = v b);
  check "pushpop: a=d in model" (v a = v d);
  check "pushpop: a<>c in model (no stale)" (v a <> v c)
;;

(* 4b. boundary: pop 0 is a no-op; pop more frames than exist raises. *)

let test_pop_boundaries () =
  let env, _u, _unary, _pred, konst, _bpred = make_env () in
  let ctx = Context.create env in
  let a = Context.const ctx (konst "a") in
  let b = Context.const ctx (konst "b") in
  let h = make_harness env ctx in
  let a_ab = reg h (Context.eq ctx a b) in
  assert_lit h (Lit.make a_ab true);
  A.pop h.adapter 0;
  (* pop 0 = no-op *)
  check
    "pop0: still consistent, a=b intact"
    (match settle h Theory.Final with
     | Theory.Conflict _ -> false
     | _ -> Model.value (A.model h.adapter) a = Model.value (A.model h.adapter) b);
  check_raises "pop too many raises" (fun () -> A.pop h.adapter 5)
;;

(* ------------------------------------------------------------------ *)
(* 5. register_atom idempotence (C7): re-registering the SAME atom+term must not perturb
   verdict or model. *)

let test_register_idempotent () =
  let env, _u, unary, _pred, konst, _bpred = make_env () in
  let ctx = Context.create env in
  let a = Context.const ctx (konst "a") in
  let b = Context.const ctx (konst "b") in
  let f = unary "f" in
  let h = make_harness env ctx in
  let eqterm = Context.eq ctx (Context.app ctx f [ a ]) (Context.app ctx f [ b ]) in
  let atom = Atom.fresh h.alloc in
  A.register_atom h.adapter atom eqterm;
  Atom.Table.replace h.term_of_atom atom eqterm;
  (* re-register the same atom+term several times *)
  A.register_atom h.adapter atom eqterm;
  A.register_atom h.adapter atom eqterm;
  let a_ab = reg h (Context.eq ctx a b) in
  assert_lit h (Lit.make a_ab true);
  (* congruence: f a = f b implied; verdict consistent *)
  check
    "idempotent: consistent after re-registration"
    (match settle h Theory.Final with
     | Theory.Conflict _ -> false
     | _ -> true);
  check
    "idempotent: f(a)=f(b) in model"
    (Model.value (A.model h.adapter) (Context.app ctx f [ a ])
     = Model.value (A.model h.adapter) (Context.app ctx f [ b ]))
;;

(* ------------------------------------------------------------------ *)
(* 6. Error contracts: foreign (Le) atom REGISTERS but must not be asserted; non-atom
   rejected; unregistered assert raises. *)

let test_error_contracts () =
  let env, _u, _unary, _pred, konst, _bpred = make_env () in
  let ctx = Context.create env in
  let h = make_harness env ctx in
  (* A genuine (non-constant-folded) Le atom: [xi <= 0] over an Int variable — a LIA
     (foreign) atom. The combinator registers it with EUF (register-not-assert) so
     congruence sees its subterms; registration must SUCCEED, but asserting it must fail
     loud. (A constant [1 <= 0] would fold to [Bool_const false], a legitimate Bool atom,
     so it must NOT be used here.) *)
  let xi = Context.const ctx (Env.declare_fun env "xi" (Rank.create [] Sort.int)) in
  let le_atom = Context.le ctx xi (Context.int_const ctx 0) in
  let le_id = Atom.fresh h.alloc in
  (match A.register_atom h.adapter le_id le_atom with
   | () -> check "register foreign (Le) atom succeeds" true
   | exception _ -> check "register foreign (Le) atom succeeds" false);
  check_raises "assert_lit on a foreign (Le) atom raises" (fun () ->
    A.assert_lit h.adapter (Lit.make le_id true));
  (* a non-atom (a genuine 2-conjunct conjunction) is still rejected at register: it is
     not an atom at all. Distinct conjuncts, since [Eq(a,b)]/[Eq(b,a)] are the same
     tag-ordered term and would dedup to one atom. *)
  let a = Context.const ctx (konst "a") in
  let b = Context.const ctx (konst "b") in
  let c = Context.const ctx (konst "c") in
  let d = Context.const ctx (konst "d") in
  let conj = Context.and_ ctx [ Context.eq ctx a b; Context.eq ctx c d ] in
  check_raises "register non-atom raises" (fun () ->
    A.register_atom h.adapter (Atom.fresh h.alloc) conj);
  (* asserting an unregistered atom raises *)
  let ghost = Atom.fresh h.alloc in
  check_raises "assert unregistered atom raises" (fun () ->
    A.assert_lit h.adapter (Lit.make ghost true))
;;

(* ------------------------------------------------------------------ *)
(* 6b. Register-non-owned (task/euf-register-nonown): registering a foreign atom (e.g. a
   LIA [Le]) internalises its App-subterm closure into the e-graph so congruence fires
   over terms EUF does not own, and model() can value them — without EUF asserting or
   watching the atom. This is what lets the combinator register-with-child /
   assert-only-owned. *)

let test_register_non_owned () =
  let env, usort, _unary, _pred, konst, _bpred = make_env () in
  let ctx = Context.create env in
  (* f : U -> Int and g : Int -> Int, so the foreign Le atoms [f(x) <= f(y)] and
     [g(f(x)) <= 0] typecheck (Le is an Int comparison) — the W1 shape. *)
  let f = Env.declare_fun env "f" (Rank.create [ usort ] Sort.int) in
  let g = Env.declare_fun env "g" (Rank.create [ Sort.int ] Sort.int) in
  let x = Context.const ctx (konst "x") in
  let y = Context.const ctx (konst "y") in
  let fx = Context.app ctx f [ x ] in
  let fy = Context.app ctx f [ y ] in
  (* (a) A foreign Le atom over f(x), f(y): [f(x) - f(y) <= 0]. Registering it must put
         BOTH f(x) and f(y) in the e-graph; then asserting x=y (a normal EUF atom) fires
         congruence over the registered-not-asserted terms => f(x) ~ f(y), and model()
         values them equally. *)
  let h = make_harness env ctx in
  let le = Context.le ctx fx fy in
  let _le_id = reg h le in
  (* x=y via an owned EUF atom *)
  let a_xy = reg h (Context.eq ctx x y) in
  assert_lit h (Lit.make a_xy true);
  (match settle h Theory.Final with
   | Theory.Conflict _ -> check "nonown(a): consistent" false
   | _ -> check "nonown(a): consistent" true);
  let m = A.model h.adapter in
  check
    "nonown(a): f(x), f(y) both valued by model"
    (Model.value m fx <> None && Model.value m fy <> None);
  check
    "nonown(a): congruence fired f(x)=f(y) over foreign-registered terms"
    (Model.value m fx = Model.value m fy);
  (* (b) Nested closure g(f(x)): registering a foreign Le over g(f(x)) internalises the
     whole nest; asserting x=z makes g(f(x)) ~ g(f(z)) by congruence. *)
  let z = Context.const ctx (konst "z") in
  let gfx = Context.app ctx g [ Context.app ctx f [ x ] ] in
  let gfz = Context.app ctx g [ Context.app ctx f [ z ] ] in
  let h2 = make_harness env ctx in
  (* both nests registered via foreign Le atoms (as the combinator would) — congruence can
     only relate g(f(x)) and g(f(z)) if BOTH are in the e-graph *)
  let _ = reg h2 (Context.le ctx gfx (Context.int_const ctx 0)) in
  let _ = reg h2 (Context.le ctx gfz (Context.int_const ctx 0)) in
  let a_xz = reg h2 (Context.eq ctx x z) in
  assert_lit h2 (Lit.make a_xz true);
  ignore (settle h2 Theory.Final : Theory.check_result);
  let m2 = A.model h2.adapter in
  check "nonown(b): nested g(f(x)) valued" (Model.value m2 gfx <> None);
  check
    "nonown(b): g(f(x))=g(f(z)) by nested congruence over foreign-registered nests"
    (Model.value m2 gfx = Model.value m2 gfz);
  (* (d) push/pop: a term registered only via a foreign atom inside a frame is truncated
     on pop and rederivable after — identical to a normal registration (CONTRACT-REG). *)
  let h3 = make_harness env ctx in
  let a_xy3 = reg h3 (Context.eq ctx x y) in
  assert_lit h3 (Lit.make a_xy3 true);
  A.push h3.adapter;
  let _ = reg h3 (Context.le ctx fx fy) in
  (* inside the frame, f(x) ~ f(y) holds (x=y at base, congruence over the just-registered
     foreign terms) *)
  let m_in = A.model h3.adapter in
  check
    "nonown(d): in-frame congruence f(x)=f(y)"
    (Model.value m_in fx = Model.value m_in fy);
  A.pop h3.adapter 1;
  (match settle h3 Theory.Final with
   | Theory.Conflict _ -> check "nonown(d): consistent after pop" false
   | _ -> check "nonown(d): consistent after pop" true);
  (* re-register after pop: rederives identically (no stale state) *)
  let _ = reg h3 (Context.le ctx fx fy) in
  let m_re = A.model h3.adapter in
  check
    "nonown(d): rederived f(x)=f(y) after pop+re-register"
    (Model.value m_re fx = Model.value m_re fy)
;;

(* ------------------------------------------------------------------ *)
(* 6c. Model currency of an equality atom (codex HIGH repro): a registered Eq(a,b) atom is
   Bool-sorted; its model value must be Bool (its truth), not a stray Uninterp class id —
   both polarities. The atom's e-node is never merged with true/false_const (assertion
   merges its SIDES), so a naive Bool-sort/true-const check would fall through to
   Uninterp; model() special-cases Eq via are_equal on the sides. *)

let test_eq_atom_model_currency () =
  let env, _u, _unary, _pred, konst, _bpred = make_env () in
  let ctx = Context.create env in
  let a = Context.const ctx (konst "a") in
  let b = Context.const ctx (konst "b") in
  let eqterm = Context.eq ctx a b in
  (* +A => a=b => Eq(a,b) is true *)
  let hp = make_harness env ctx in
  let ap = reg hp eqterm in
  assert_lit hp (Lit.make ap true);
  (match settle hp Theory.Final with
   | Theory.Sat ->
     let m = A.model hp.adapter in
     check
       "eq-currency+: model(Eq(a,b)) = Bool true"
       (Model.value m eqterm = Some (Model.Bool true));
     check "eq-currency+: sides share a witness" (Model.value m a = Model.value m b)
   | _ -> check "eq-currency+: expected Final Sat" false);
  (* -A => a<>b => Eq(a,b) is false *)
  let hn = make_harness env ctx in
  let an = reg hn eqterm in
  assert_lit hn (Lit.make an false);
  match settle hn Theory.Final with
  | Theory.Sat ->
    let m = A.model hn.adapter in
    check
      "eq-currency-: model(Eq(a,b)) = Bool false"
      (Model.value m eqterm = Some (Model.Bool false));
    check
      "eq-currency-: sides have distinct witnesses"
      (Model.value m a <> Model.value m b)
  | _ -> check "eq-currency-: expected Final Sat" false
;;

(* ------------------------------------------------------------------ *)
(* 7. Determinism: identical script on two fresh adapters => identical model snapshot. *)

let model_snapshot h terms =
  let m = A.model h.adapter in
  List.map (fun t -> t.Term.tag, Model.value m t) terms
;;

let test_determinism () =
  let build () =
    let env, _u, unary, pred, konst, _bpred = make_env () in
    let ctx = Context.create env in
    let a = Context.const ctx (konst "a") in
    let b = Context.const ctx (konst "b") in
    let cc = Context.const ctx (konst "c") in
    let f = unary "f" in
    let p = pred "p" in
    let h = make_harness env ctx in
    let a_ab = reg h (Context.eq ctx a b) in
    let a_bc = reg h (Context.eq ctx b cc) in
    let a_pa = reg h (Context.app ctx p [ a ]) in
    let _ = reg h (Context.eq ctx (Context.app ctx f [ a ]) (Context.app ctx f [ cc ])) in
    assert_lit h (Lit.make a_ab true);
    assert_lit h (Lit.make a_bc true);
    assert_lit h (Lit.make a_pa true);
    ignore (settle h Theory.Final);
    model_snapshot h [ a; b; cc; Context.app ctx f [ a ]; Context.app ctx p [ a ] ]
  in
  check "determinism: identical model snapshot across runs" (build () = build ())
;;

(* ------------------------------------------------------------------ *)
(* 8. Randomized cross-check vs the independent oracle:
   - consistency verdict matches;
   - model-induced equality on uninterp terms matches;
   - predicate truth values match;
   - every conflict explanation replays to an inconsistency and leaks no axiom. *)

let rand_cases = ref 0

let test_random () =
  set_seed 0x0DA9_7E51_C0FF_EE01;
  let trials = 400 in
  for _ = 1 to trials do
    let env, usort, unary, pred, konst, bpred = make_env () in
    let ctx = Context.create env in
    let true_c = Context.bool_const ctx true in
    let false_c = Context.bool_const ctx false in
    let c0 = Context.const ctx (konst "c0") in
    let c1 = Context.const ctx (konst "c1") in
    let c2 = Context.const ctx (konst "c2") in
    let f = unary "f" in
    let g = unary "g" in
    let p = pred "p" in
    let q = Context.const ctx (bpred "q") in
    ignore usort;
    let uterms =
      [ c0
      ; c1
      ; c2
      ; Context.app ctx f [ c0 ]
      ; Context.app ctx f [ c1 ]
      ; Context.app ctx g [ c0 ]
      ]
    in
    let uarr = Array.of_list uterms in
    (* candidate atoms: equalities among uterms + predicates *)
    let eq_atoms =
      [ Context.eq ctx c0 c1
      ; Context.eq ctx c1 c2
      ; Context.eq ctx c0 (Context.app ctx f [ c1 ])
      ; Context.eq ctx (Context.app ctx f [ c0 ]) (Context.app ctx g [ c0 ])
      ; Context.eq ctx c2 (Context.app ctx f [ c0 ])
      ]
    in
    let pred_atoms =
      [ Context.app ctx p [ c0 ]; Context.app ctx p [ c1 ]; Context.app ctx p [ c2 ]; q ]
    in
    let atom_terms = eq_atoms @ pred_atoms in
    let h = make_harness env ctx in
    let handles = List.map (fun t -> reg h t) atom_terms in
    let handle_arr = Array.of_list handles in
    (* assert a random subset with random signs *)
    let n_atoms = Array.length handle_arr in
    let steps = 3 + rand_int 6 in
    let facts = ref [] in
    for _ = 1 to steps do
      incr rand_cases;
      let k = rand_int n_atoms in
      let pos = rand_int 2 = 0 in
      let lit = Lit.make handle_arr.(k) pos in
      assert_lit h lit;
      facts := fact_of_lit h ~true_c ~false_c lit :: !facts
    done;
    (* oracle universe + saturation *)
    let univ = closure (true_c :: false_c :: (atom_terms @ uterms)) in
    let nz = Naive.build univ in
    let unions =
      List.filter_map
        (function
          | Union (a, b) -> Some (a, b)
          | Diseq _ -> None)
        !facts
    in
    let diseqs =
      (true_c, false_c)
      :: List.filter_map
           (function
             | Diseq (a, b) -> Some (a, b)
             | Union _ -> None)
           !facts
    in
    Naive.saturate nz unions;
    let oracle_consistent =
      List.for_all (fun (a, b) -> not (Naive.equal nz a b)) diseqs
    in
    let result = settle h Theory.Final in
    let adapter_consistent =
      match result with
      | Theory.Conflict _ -> false
      | _ -> true
    in
    check
      "random: consistency verdict matches oracle"
      (oracle_consistent = adapter_consistent);
    if adapter_consistent
    then (
      (* model-induced equality on uninterp terms must match the oracle *)
      let m = A.model h.adapter in
      let ok = ref true in
      for i = 0 to Array.length uarr - 1 do
        for j = i + 1 to Array.length uarr - 1 do
          let adapter_eq = Model.value m uarr.(i) = Model.value m uarr.(j) in
          if adapter_eq <> Naive.equal nz uarr.(i) uarr.(j) then ok := false
        done
      done;
      check "random: model-induced equality matches oracle" !ok;
      (* predicate truth values in the model match the oracle's true/false class *)
      let pok = ref true in
      List.iter
        (fun pt ->
           match Model.value m pt with
           | Some (Model.Bool bv) ->
             let oracle_true = Naive.equal nz pt true_c in
             let oracle_false = Naive.equal nz pt false_c in
             if bv && not oracle_true then pok := false;
             if (not bv) && not oracle_false then pok := false
           | _ ->
             (* undetermined predicate: oracle must not have pinned it to true or false *)
             if Naive.equal nz pt true_c || Naive.equal nz pt false_c then pok := false)
        pred_atoms;
      check "random: predicate truth matches oracle" !pok)
    else (
      match result with
      | Theory.Conflict e ->
        let prem = Lit.Set.of_list e.Explanation.premises in
        (* no fabricated / axiom-leaked premise *)
        check "random: conflict premises subset asserted" (Lit.Set.subset prem h.asserted);
        check "random: conflict premises non-empty" (e.Explanation.premises <> []);
        (* replay ONLY the cited premises (+ standing axiom) into a fresh oracle; must be
           inconsistent — the explanation is sufficient. *)
        let nz2 = Naive.build univ in
        let pfacts = List.map (fact_of_lit h ~true_c ~false_c) e.Explanation.premises in
        let punions =
          List.filter_map
            (function
              | Union (a, b) -> Some (a, b)
              | Diseq _ -> None)
            pfacts
        in
        let pdiseqs =
          (true_c, false_c)
          :: List.filter_map
               (function
                 | Diseq (a, b) -> Some (a, b)
                 | Union _ -> None)
               pfacts
        in
        Naive.saturate nz2 punions;
        let replay_inconsistent =
          not (List.for_all (fun (a, b) -> not (Naive.equal nz2 a b)) pdiseqs)
        in
        check "random: conflict explanation replays to inconsistency" replay_inconsistent
      | _ -> ())
  done
;;

(* ------------------------------------------------------------------ *)
(* 3b(i). Explanation reason-content on the EQUALITY path (#102). NOT a discriminator for
   the ask-time bug — a redundant same-class merge is skipped (euf.ml), so the forest path
   from a to d stays unique and the old ask-time code returned the same reason, so this
   PASSES against both implementations. Its job is to PIN the equality-path reason
   content: a=d propagated via the chain a=b,b=c,c=d must be explained by exactly that
   chain, and a later shorter path a=e,e=d must not change the cached content. A future
   regression that corrupted equality-path reason content (in a way the diseq
   discriminator below would not catch) turns this red. Kept alongside 3b(ii) per the
   team-lead's ruling. *)
let test_explain_precedence_eq_path_property () =
  let env, _u, _unary, _pred, konst, _bpred = make_env () in
  let ctx = Context.create env in
  let a = Context.const ctx (konst "a") in
  let b = Context.const ctx (konst "b") in
  let c = Context.const ctx (konst "c") in
  let d = Context.const ctx (konst "d") in
  let e = Context.const ctx (konst "e") in
  let h = make_harness env ctx in
  let a_ad = reg h (Context.eq ctx a d) in
  let a_ab = reg h (Context.eq ctx a b) in
  let a_bc = reg h (Context.eq ctx b c) in
  let a_cd = reg h (Context.eq ctx c d) in
  let a_ae = reg h (Context.eq ctx a e) in
  let a_ed = reg h (Context.eq ctx e d) in
  assert_lit h (Lit.make a_ab true);
  assert_lit h (Lit.make a_bc true);
  assert_lit h (Lit.make a_cd true);
  let ad_pos = Lit.make a_ad true in
  (match A.check h.adapter Theory.Propagate with
   | Theory.Propagations lits ->
     check "eq-path: (a=d) propagated true" (List.exists (Lit.equal ad_pos) lits)
   | _ -> check "eq-path: expected propagation of (a=d)" false);
  let chain =
    Lit.Set.of_list [ Lit.make a_ab true; Lit.make a_bc true; Lit.make a_cd true ]
  in
  let expl1 = A.explain h.adapter ad_pos in
  check
    "eq-path: reason is exactly the causal chain {a=b,b=c,c=d}"
    (Lit.Set.equal (Lit.Set.of_list expl1.Explanation.premises) chain);
  (* a later shorter path must not change the cached reason content *)
  assert_lit h (Lit.make a_ae true);
  assert_lit h (Lit.make a_ed true);
  let expl2 = A.explain h.adapter ad_pos in
  check
    "eq-path: reason content stable under a later shorter path"
    (Lit.Set.equal (Lit.Set.of_list expl2.Explanation.premises) chain)
;;

(* 3b(ii). Explanation PRECEDENCE regression (#102), on the DISEQUALITY path — the actual
   home of the defect (codex review). [distinct_witness] (euf.ml) scans asserted diseqs in
   fixed ASSERTION order and returns the FIRST that separates the two classes; the
   ask-time re-derivation therefore picks whichever separating diseq was asserted earliest
   AT ASK TIME, then explains the two congruence legs against the CURRENT forest — legs
   that a later merge can shorten, yielding premises that POSTDATE the explained literal
   (CONTRACT-EX violation -> poison -> unknown). Snapshotting the reason at propagation
   time fixes it.

   Scenario (codex's exact construction): assert e<>f EARLY (so it sits first in the diseq
   scan), then c<>d, a=c, b=d — now a,b are separated only by the c<>d witness, so ~(a=b)
   propagates with reason [{a=c, b=d, c<>d}] (all pre-propagation). THEN assert a=e, b=f:
   a~c~e and b~d~f, so e<>f ALSO now separates a,b — and it is earlier in the scan, so an
   ask-time re-derivation switches to it and cites the LATER a=e / b=f. [explain] must
   still return the snapshotted [{a=c, b=d, c<>d}]. (This test FAILS against the pre-fix
   ask-time implementation and passes with the cache — the "locks the bug shut" criterion;
   the equality-path variant did not, because a redundant same-class merge is skipped so
   the forest path stays unique. Verified both directions in a scratch worktree, see the
   fix-round report.) *)
let test_explain_precedence_diseq_regression () =
  let env, _u, _unary, _pred, konst, _bpred = make_env () in
  let ctx = Context.create env in
  let a = Context.const ctx (konst "a") in
  let b = Context.const ctx (konst "b") in
  let c = Context.const ctx (konst "c") in
  let d = Context.const ctx (konst "d") in
  let e = Context.const ctx (konst "e") in
  let f = Context.const ctx (konst "f") in
  let h = make_harness env ctx in
  let a_ab = reg h (Context.eq ctx a b) in
  let a_ef = reg h (Context.eq ctx e f) in
  let a_cd = reg h (Context.eq ctx c d) in
  let a_ac = reg h (Context.eq ctx a c) in
  let a_bd = reg h (Context.eq ctx b d) in
  let a_ae = reg h (Context.eq ctx a e) in
  let a_bf = reg h (Context.eq ctx b f) in
  (* e<>f asserted FIRST (earliest in the diseq scan), then the c<>d witness + merges *)
  assert_lit h (Lit.make a_ef false);
  assert_lit h (Lit.make a_cd false);
  assert_lit h (Lit.make a_ac true);
  assert_lit h (Lit.make a_bd true);
  (* a~c, b~d, c<>d => ~(a=b) theory-implied (distinct) *)
  let ab_neg = Lit.make a_ab false in
  (match A.check h.adapter Theory.Propagate with
   | Theory.Propagations lits ->
     check "precedence(diseq): ~(a=b) propagated" (List.exists (Lit.equal ab_neg) lits)
   | _ -> check "precedence(diseq): expected propagation of ~(a=b)" false);
  (* assertions present AT propagation time — every valid premise must be among these *)
  let at_prop = h.asserted in
  (* NOW merge a~e and b~f (strictly AFTER the propagation): e<>f now ALSO separates a,b,
     and it is earlier in the diseq scan, so an ask-time re-derivation would cite a=e/b=f. *)
  assert_lit h (Lit.make a_ae true);
  assert_lit h (Lit.make a_bf true);
  let expl = A.explain h.adapter ab_neg in
  let prem = Lit.Set.of_list expl.Explanation.premises in
  check "precedence(diseq): reason non-empty" (expl.Explanation.premises <> []);
  (* THE property (fails on the ask-time implementation): every premise predates the
     propagation — the later a=e / b=f are excluded. *)
  check
    "precedence(diseq): reason excludes later-asserted premises"
    (Lit.Set.subset prem at_prop);
  check
    "precedence(diseq): reason excludes a=e"
    (not (Lit.Set.mem (Lit.make a_ae true) prem));
  check
    "precedence(diseq): reason excludes b=f"
    (not (Lit.Set.mem (Lit.make a_bf true) prem))
;;

(* 3c. Cache lifecycle under push/pop (the AP1 trailed-state class). A propagated
   literal's snapshotted reason lives in the frame that produced it; [pop] must drop it in
   lockstep (else [explain] serves a reason whose premises unwound with the frame). The
   recovery invariant: popping every open frame (the [cancel_until 0] analogue) leaves
   only base-frame reasons — every deeper reason is gone, and a re-propagation re-caches
   afresh. *)
let test_explain_cache_pushpop () =
  let env, _u, _unary, _pred, konst, _bpred = make_env () in
  let ctx = Context.create env in
  let a = Context.const ctx (konst "a") in
  let b = Context.const ctx (konst "b") in
  let c = Context.const ctx (konst "c") in
  let d = Context.const ctx (konst "d") in
  let e = Context.const ctx (konst "e") in
  let g = Context.const ctx (konst "g") in
  let h = make_harness env ctx in
  let a_ab = reg h (Context.eq ctx a b) in
  let a_ac = reg h (Context.eq ctx a c) in
  let a_cb = reg h (Context.eq ctx c b) in
  let a_de = reg h (Context.eq ctx d e) in
  let a_dg = reg h (Context.eq ctx d g) in
  let a_ge = reg h (Context.eq ctx g e) in
  let propagated effort =
    match A.check h.adapter effort with
    | Theory.Propagations lits -> lits
    | _ -> []
  in
  let explains lit = (A.explain h.adapter lit).Explanation.premises <> [] in
  (* base: a=c, c=b -> (a=b) propagates true, reason cached in the base frame *)
  assert_lit h (Lit.make a_ac true);
  assert_lit h (Lit.make a_cb true);
  let ab_pos = Lit.make a_ab true in
  check
    "cache: (a=b) propagated at base"
    (List.exists (Lit.equal ab_pos) (propagated Theory.Propagate));
  check "cache: base reason explains" (explains ab_pos);
  (* push a frame; d=g, g=e -> (d=e) propagates true, reason cached in the deeper frame *)
  A.push h.adapter;
  assert_lit h (Lit.make a_dg true);
  assert_lit h (Lit.make a_ge true);
  let de_pos = Lit.make a_de true in
  check
    "cache: (d=e) propagated in frame 1"
    (List.exists (Lit.equal de_pos) (propagated Theory.Propagate));
  check "cache: frame-1 reason explains" (explains de_pos);
  check "cache: base reason still explains under frame 1" (explains ab_pos);
  (* pop the deeper frame (cancel_until analogue): its reason MUST be gone (fail-loud),
     while the base reason survives. *)
  A.pop h.adapter 1;
  check_raises "cache: popped frame's reason is dropped" (fun () ->
    A.explain h.adapter de_pos);
  check "cache: base reason survives the pop" (explains ab_pos);
  (* re-propagate (d=e) at base after re-asserting: cache is repopulated afresh. *)
  assert_lit h (Lit.make a_dg true);
  assert_lit h (Lit.make a_ge true);
  check
    "cache: (d=e) re-propagated at base"
    (List.exists (Lit.equal de_pos) (propagated Theory.Propagate));
  check "cache: re-cached reason explains" (explains de_pos)
;;

(* 3d. Fix x euf-perf incremental-propagate interaction (budget-reviewer F1 / probe 4).
   euf-perf's [Euf.propagate] is delta-driven: a watched atom's flip is reported at the
   next [propagate] whose dirty set touches its class, which may be a LATER [check] than
   the one right after the causal merge. The snapshotted reason must still be
   precedence-valid and causal regardless of WHICH check reports it (precedence is tied to
   "before the literal is trailed", structural, not to report timing). Here (a=b)'s causal
   merge c=b lands with NO intervening check; an unrelated d=e is then asserted; the flip
   surfaces only at the next [check] — and [explain] must return the causal [{a=c, c=b}],
   never the unrelated d=e. *)
let test_explain_euf_perf_deferral () =
  let env, _u, _unary, _pred, konst, _bpred = make_env () in
  let ctx = Context.create env in
  let a = Context.const ctx (konst "a") in
  let b = Context.const ctx (konst "b") in
  let c = Context.const ctx (konst "c") in
  let d = Context.const ctx (konst "d") in
  let e = Context.const ctx (konst "e") in
  let h = make_harness env ctx in
  let a_ab = reg h (Context.eq ctx a b) in
  let a_ac = reg h (Context.eq ctx a c) in
  let a_cb = reg h (Context.eq ctx c b) in
  let a_de = reg h (Context.eq ctx d e) in
  let propd effort =
    match A.check h.adapter effort with
    | Theory.Propagations lits -> lits
    | _ -> []
  in
  let ab_pos = Lit.make a_ab true in
  (* a=c, then DRAIN propagate so its delta watermark advances (no flip for a=b yet) *)
  assert_lit h (Lit.make a_ac true);
  check
    "deferral: (a=b) not yet implied after only a=c"
    (not (List.exists (Lit.equal ab_pos) (propd Theory.Propagate)));
  (* causal merge c=b lands with NO check; then an unrelated d=e; the (a=b) flip is
     deferred to the NEXT check, reported alongside d=e's dirty entry. *)
  assert_lit h (Lit.make a_cb true);
  assert_lit h (Lit.make a_de true);
  check
    "deferral: (a=b) reported at the later check (after c=b + unrelated d=e)"
    (List.exists (Lit.equal ab_pos) (propd Theory.Propagate));
  let expl = A.explain h.adapter ab_pos in
  let prem = Lit.Set.of_list expl.Explanation.premises in
  check "deferral: reason non-empty" (expl.Explanation.premises <> []);
  (* causal {a=c, c=b} only — the unrelated later d=e must not appear *)
  check
    "deferral: reason is the causal pair {a=c, c=b}"
    (Lit.Set.equal prem (Lit.Set.of_list [ Lit.make a_ac true; Lit.make a_cb true ]));
  check
    "deferral: reason excludes the unrelated d=e"
    (not (Lit.Set.mem (Lit.make a_de true) prem))
;;

(* ================================================================== *)
let () =
  print_endline "euf adapter self-test:";
  test_textbook ();
  test_predicate_conflict ();
  test_bool_lit_conflict ();
  test_propagation ();
  test_predicate_propagation ();
  test_predicate_pushpop_restore ();
  test_predicate_late_binding ();
  test_predicate_latebind_pop_recurrence ();
  test_explain_precedence_eq_path_property ();
  test_explain_precedence_diseq_regression ();
  test_explain_euf_perf_deferral ();
  test_explain_cache_pushpop ();
  test_pushpop_restore ();
  test_pop_boundaries ();
  test_register_idempotent ();
  test_error_contracts ();
  test_register_non_owned ();
  test_eq_atom_model_currency ();
  test_determinism ();
  test_random ();
  Printf.printf
    "\neuf adapter self-test: %d checks, %d randomized assert-cases, %d failure(s)\n"
    !checks
    !rand_cases
    !failures;
  if !failures > 0 then exit 1
;;
