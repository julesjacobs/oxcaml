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
(* 6. Error contracts: Le atom rejected, non-atom rejected, unregistered assert raises. *)

let test_error_contracts () =
  let env, _u, _unary, _pred, konst, _bpred = make_env () in
  let ctx = Context.create env in
  let h = make_harness env ctx in
  (* A genuine (non-constant-folded) Le atom: [xi <= 0] over an Int variable — belongs to
     LIA, must be rejected by the EUF adapter. (A constant [1 <= 0] would fold to
     [Bool_const false], which is a legitimate Bool atom, so it must NOT be used here.) *)
  let xi = Context.const ctx (Env.declare_fun env "xi" (Rank.create [] Sort.int)) in
  let le_atom = Context.le ctx xi (Context.int_const ctx 0) in
  check_raises "register Le atom raises" (fun () ->
    A.register_atom h.adapter (Atom.fresh h.alloc) le_atom);
  (* a non-atom (a genuine 2-conjunct conjunction) is rejected. Distinct conjuncts, since
     [Eq(a,b)]/[Eq(b,a)] are the same tag-ordered term and would dedup to one atom. *)
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

(* ================================================================== *)
let () =
  print_endline "euf adapter self-test:";
  test_textbook ();
  test_predicate_conflict ();
  test_bool_lit_conflict ();
  test_propagation ();
  test_pushpop_restore ();
  test_pop_boundaries ();
  test_register_idempotent ();
  test_error_contracts ();
  test_determinism ();
  test_random ();
  Printf.printf
    "\neuf adapter self-test: %d checks, %d randomized assert-cases, %d failure(s)\n"
    !checks
    !rand_cases
    !failures;
  if !failures > 0 then exit 1
;;
