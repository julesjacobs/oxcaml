(* In-tree unit + property tests for smt/theories/euf (proof-producing congruence closure,
   Nieuwenhuis-Oliveras). Stdlib-only, deterministic (fixed-seed xorshift), no wall-clock.
   Run via [make euf-test]; also built by [make build].

   The main oracle is an INDEPENDENT, naive quadratic congruence closure written here from
   the EUF spec alone (a from-scratch union-find + brute-force O(n^2) congruence rule over
   a fixed term universe). It shares no code with the engine's own union-find /
   explanation forest / congruence table, so agreement is a genuine cross-check, not a
   tautology. The engine additionally self-checks every explanation it produces (DESIGN
   §7); this suite re-verifies the same soundness property from the test side.

   Coverage: the textbook (f x)=a / x=y / (f y)<>a refutation with exact conflict
   premises; deep congruence chains (a=f(a); f^3(x)=x /\ f^5(x)=x => f(x)=x);
   (dis)equality propagation with lazy explanations; thousands of randomized assert
   sequences cross-checked for equivalence classes + consistency verdict; explanation
   soundness (premises replayed into the naive closure suffice); randomized interleaved
   assert/push/pop/check vs recomputation-from-scratch; and determinism (same input twice
   => identical propagation order + explanations). *)

open Oxsmt_core
module Euf = Oxsmt_euf.Euf

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
(* Deterministic PRNG: xorshift64*, fixed seed (as in core_test). *)

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
(* Env / term-building helpers. *)

let usort_name = "U"

let make_env () =
  let env = Env.create () in
  let u = Env.declare_sort env usort_name in
  let usort = Sort.uninterpreted u in
  let unary name = Env.declare_fun env name (Rank.create [ usort ] usort) in
  let konst name = Env.declare_fun env name (Rank.create [] usort) in
  env, usort, unary, konst
;;

(* subterm closure of a seed list, deterministically ordered by Term tag. *)
let closure (seeds : Term.t list) : Term.t array =
  let tbl = Term.Table.create 64 in
  let rec go (t : Term.t) =
    if not (Term.Table.mem tbl t)
    then (
      Term.Table.replace tbl t ();
      let kids =
        Term.(
          match t.node with
          | Bool_const _ | Int_const _ | Real_const _ -> []
          | App (_, a) -> Iarr.to_list a
          | Arith { coeffs; _ } -> List.map fst (Iarr.to_list coeffs)
          | Real_arith { coeffs; _ } -> List.map fst (Iarr.to_list coeffs)
          | Le a -> [ a ]
          | Eq (a, b) -> [ a; b ]
          | Not a -> [ a ]
          | And a | Or a -> Iarr.to_list a
          | Ite (c, a, b) -> [ c; a; b ])
      in
      List.iter go kids)
  in
  List.iter go seeds;
  let all = Term.Table.fold (fun t () acc -> t :: acc) tbl [] in
  let arr = Array.of_list all in
  Array.sort Term.compare arr;
  arr
;;

(* ------------------------------------------------------------------ *)
(* Independent naive quadratic congruence closure over a fixed universe. *)

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

  (* congruence-close after seeding the asserted equalities (universe indices). *)
  let saturate t eqs =
    Array.iteri (fun i _ -> t.parent.(i) <- i) t.univ;
    List.iter (fun (i, j) -> union t i j) eqs;
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
                if find t (Term.Table.find t.idx arg) <> find t (Term.Table.find t.idx bj)
                then all := false)
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

  let equal t i j = find t i = find t j
  let index t term = Term.Table.find t.idx term
end

(* ------------------------------------------------------------------ *)
(* A fixed 10-node universe for the randomized cross-checks. *)

let build_universe () =
  let _env, _u, unary, konst = make_env () in
  let ctx = Context.create _env in
  let c0 = Context.const ctx (konst "c0") in
  let c1 = Context.const ctx (konst "c1") in
  let c2 = Context.const ctx (konst "c2") in
  let f = unary "f" in
  let g = unary "g" in
  let ap s x = Context.app ctx s [ x ] in
  let seeds =
    [ c0
    ; c1
    ; c2
    ; ap f c0
    ; ap f c1
    ; ap g c0
    ; ap g c1
    ; ap f (ap f c0)
    ; ap g (ap f c0)
    ; ap f (ap g c1)
    ]
  in
  ctx, closure seeds
;;

(* ------------------------------------------------------------------ *)
(* 1. Textbook refutation: (f x)=a, x=y, (f y)<>a  =>  conflict {1;2;3}. *)

let test_textbook () =
  let _env, _u, unary, konst = make_env () in
  let ctx = Context.create _env in
  let a = Context.const ctx (konst "a") in
  let x = Context.const ctx (konst "x") in
  let y = Context.const ctx (konst "y") in
  let f = unary "f" in
  let fx = Context.app ctx f [ x ] in
  let fy = Context.app ctx f [ y ] in
  let e = Euf.create ctx in
  Euf.register_term e fx;
  Euf.register_term e fy;
  Euf.assert_eq e ~premise:1 fx a;
  Euf.assert_eq e ~premise:2 x y;
  Euf.assert_neq e ~premise:3 fy a;
  (match Euf.check e with
   | Euf.Consistent -> check "textbook: expected conflict" false
   | Euf.Conflict prems ->
     check "textbook: conflict premises = {1;2;3}" (List.sort compare prems = [ 1; 2; 3 ]));
  check "textbook: fy ~ a" (Euf.are_equal e fy a);
  check "textbook: fx ~ fy (congruence)" (Euf.are_equal e fx fy)
;;

(* ------------------------------------------------------------------ *)
(* 2a. a = f(a): every f^k(a) collapses to a. *)

let test_chain_selfloop () =
  let _env, _u, unary, konst = make_env () in
  let ctx = Context.create _env in
  let a = Context.const ctx (konst "a") in
  let f = unary "f" in
  let rec app_n k t = if k = 0 then t else app_n (k - 1) (Context.app ctx f [ t ]) in
  let fa = app_n 1 a
  and ffa = app_n 2 a
  and fffa = app_n 3 a in
  let e = Euf.create ctx in
  List.iter (Euf.register_term e) [ fa; ffa; fffa ];
  Euf.assert_eq e ~premise:1 a fa;
  check "selfloop: a ~ f(a)" (Euf.are_equal e a fa);
  check "selfloop: a ~ f^2(a)" (Euf.are_equal e a ffa);
  check "selfloop: a ~ f^3(a)" (Euf.are_equal e a fffa);
  check "selfloop: f(a) ~ f^3(a)" (Euf.are_equal e fa fffa);
  (match Euf.check e with
   | Euf.Consistent -> ()
   | Euf.Conflict _ -> check "selfloop: consistent" false);
  check
    "selfloop: explain(a,f^3 a) = {1}"
    (List.sort compare (Euf.explain e a fffa) = [ 1 ])
;;

(* 2b. f^3(x)=x /\ f^5(x)=x => f(x)=x (pure congruence + transitivity). *)

let test_chain_orders () =
  let _env, _u, unary, konst = make_env () in
  let ctx = Context.create _env in
  let x = Context.const ctx (konst "x") in
  let f = unary "f" in
  let rec app_n k t = if k = 0 then t else app_n (k - 1) (Context.app ctx f [ t ]) in
  let pow = Array.init 8 (fun k -> app_n k x) in
  let e = Euf.create ctx in
  Array.iter (Euf.register_term e) pow;
  Euf.assert_eq e ~premise:1 pow.(3) pow.(0);
  Euf.assert_eq e ~premise:2 pow.(5) pow.(0);
  check "orders: f(x) ~ x" (Euf.are_equal e pow.(1) pow.(0));
  check "orders: f^2(x) ~ x" (Euf.are_equal e pow.(2) pow.(0));
  check "orders: f^4(x) ~ x" (Euf.are_equal e pow.(4) pow.(0));
  (* cross-check against the naive oracle over the same universe *)
  let univ = closure (Array.to_list pow) in
  let nz = Naive.build univ in
  Naive.saturate
    nz
    [ Naive.index nz pow.(3), Naive.index nz pow.(0)
    ; Naive.index nz pow.(5), Naive.index nz pow.(0)
    ];
  let ok = ref true in
  Array.iter
    (fun s ->
      Array.iter
        (fun t ->
          if Euf.are_equal e s t <> Naive.equal nz (Naive.index nz s) (Naive.index nz t)
          then ok := false)
        univ)
    univ;
  check "orders: classes match naive oracle" !ok
;;

(* ------------------------------------------------------------------ *)
(* 3. Propagation of watched equality atoms + lazy explanations. *)

let test_propagation () =
  let _env, _u, unary, konst = make_env () in
  let ctx = Context.create _env in
  let a = Context.const ctx (konst "a") in
  let b = Context.const ctx (konst "b") in
  let c1 = Context.const ctx (konst "c1") in
  let c2 = Context.const ctx (konst "c2") in
  let f = unary "f" in
  let ab = Context.eq ctx a b in
  (* positive: a=c1, c1=b => a=b entailed. *)
  let e = Euf.create ctx in
  Euf.register_term e ab;
  check "prop: nothing implied initially" (Euf.propagate e = []);
  Euf.assert_eq e ~premise:10 a c1;
  Euf.assert_eq e ~premise:11 c1 b;
  (match Euf.propagate e with
   | [ imp ] ->
     check "prop+: atom is (a=b)" (Term.equal imp.Euf.atom ab);
     check "prop+: value true" imp.Euf.value;
     check
       "prop+: explanation = {10;11}"
       (List.sort compare (Euf.explain_implied e imp) = [ 10; 11 ])
   | _ -> check "prop+: exactly one implied" false);
  check "prop+: nothing new on re-poll" (Euf.propagate e = []);
  (* congruence positive: watch (f a)=(f b); assert a=b. *)
  let fa = Context.app ctx f [ a ]
  and fb = Context.app ctx f [ b ] in
  let fab = Context.eq ctx fa fb in
  let e2 = Euf.create ctx in
  Euf.register_term e2 fab;
  Euf.assert_eq e2 ~premise:30 a b;
  (match Euf.propagate e2 with
   | [ imp ] ->
     check "prop-cong: atom is (f a=f b)" (Term.equal imp.Euf.atom fab);
     check "prop-cong: value true" imp.Euf.value;
     check "prop-cong: explanation = {30}" (Euf.explain_implied e2 imp = [ 30 ])
   | _ -> check "prop-cong: exactly one implied" false);
  (* negative: c1<>c2, a=c1, b=c2 => a<>b entailed. *)
  let e3 = Euf.create ctx in
  Euf.register_term e3 ab;
  Euf.assert_neq e3 ~premise:20 c1 c2;
  Euf.assert_eq e3 ~premise:21 a c1;
  Euf.assert_eq e3 ~premise:22 b c2;
  match Euf.propagate e3 with
  | [ imp ] ->
    check "prop-: atom is (a=b)" (Term.equal imp.Euf.atom ab);
    check "prop-: value false" (not imp.Euf.value);
    check
      "prop-: explanation = {20;21;22}"
      (List.sort compare (Euf.explain_implied e3 imp) = [ 20; 21; 22 ])
  | _ -> check "prop-: exactly one implied" false
;;

(* 3c. distinct_witness witness IDENTITY (task #33 O(1) witness index). When SEVERAL
   disequalities separate the same class pair, the cited witness must be the
   EARLIEST-asserted one — byte-identical to the old full assertion-order scan — so
   explanation premises (hence learned clauses / counted-metric identity) are unchanged by
   the cache. Here diseq [c1<>c2] (premise 20) is asserted first (then a=c1:21, b=c2:22)
   and a redundant [a<>b] (premise 40) later; both separate class(a) from class(b).
   [propagate] builds the witness index and the reported [a=b]-false explanation must cite
   the FIRST witness — premises 20,21,22 — NOT premise 40. DISCRIMINATION: a
   LAST-writer-wins index cites 40 and yields explanation [40]; this check goes RED
   (verified RED against a last-wins mutation before landing). It does NOT catch a dropped
   re-verify: with the merge/pop/count invalidation the cache is never consulted stale, so
   the re-verify is defense-in-depth, not load-bearing — that a public-API discrimination
   test for it cannot be built is itself the proof it is non-load-bearing (fable's review
   executed the drop-re-verify mutant: euf-test + counted-identity + a randomized push/pop
   oracle all stayed green). *)
let test_distinct_witness_first_wins () =
  let _env, _u, _unary, konst = make_env () in
  let ctx = Context.create _env in
  let a = Context.const ctx (konst "a") in
  let b = Context.const ctx (konst "b") in
  let c1 = Context.const ctx (konst "c1") in
  let c2 = Context.const ctx (konst "c2") in
  let ab = Context.eq ctx a b in
  let e = Euf.create ctx in
  Euf.register_term e ab;
  Euf.assert_neq e ~premise:20 c1 c2;
  Euf.assert_eq e ~premise:21 a c1;
  Euf.assert_eq e ~premise:22 b c2;
  Euf.assert_neq e ~premise:40 a b;
  match Euf.propagate e with
  | [ imp ] ->
    check "witness-first: atom is (a=b)" (Term.equal imp.Euf.atom ab);
    check "witness-first: value false" (not imp.Euf.value);
    check
      "witness-first: explanation = {20;21;22} (earliest witness, not {40})"
      (List.sort compare (Euf.explain_implied e imp) = [ 20; 21; 22 ])
  | _ -> check "witness-first: exactly one implied" false
;;

(* 3b. ⊤/⊥ bridge: a Bool-codomain predicate application is watched against [true_const],
   so a predicate truth entailed by congruence ([p(a), a = b |- p(b)]) surfaces as a
   {!propagate} flip — not merely as a reactive [true <> false] conflict on a wrong guess.
   DISCRIMINATION: before predicate watching, [register_term] watched only non-Bool [Eq]
   atoms, so [propagate] here returned [[]] and every check below failed. *)

let test_predicate_propagation () =
  let env, usort, _unary, konst = make_env () in
  let p = Env.declare_fun env "p" (Rank.create [ usort ] Sort.bool) in
  let ctx = Context.create env in
  let a = Context.const ctx (konst "a") in
  let b = Context.const ctx (konst "b") in
  let pa = Context.app ctx p [ a ]
  and pb = Context.app ctx p [ b ] in
  let tt = Context.bool_const ctx true
  and ff = Context.bool_const ctx false in
  (* positive: p(a)=true and a=b => p(b) entailed true by congruence. *)
  let e = Euf.create ctx in
  Euf.register_term e pa;
  Euf.register_term e pb;
  check "pred-prop: nothing implied initially" (Euf.propagate e = []);
  Euf.assert_eq e ~premise:1 pa tt;
  (* p(a) itself now equals true_const — a self-report, drained here. *)
  (match Euf.propagate e with
   | [ imp ] ->
     check
       "pred-prop: p(a) self-implied true"
       (Term.equal imp.Euf.atom pa && imp.Euf.value)
   | _ -> check "pred-prop: exactly one self-implied (p a)" false);
  Euf.assert_eq e ~premise:2 a b;
  (match Euf.propagate e with
   | [ imp ] ->
     check "pred-prop+: atom is p(b)" (Term.equal imp.Euf.atom pb);
     check "pred-prop+: value true" imp.Euf.value;
     check
       "pred-prop+: explanation = {1;2}"
       (List.sort compare (Euf.explain_implied e imp) = [ 1; 2 ])
   | _ -> check "pred-prop+: exactly one implied (p b)" false);
  check "pred-prop+: nothing new on re-poll" (Euf.propagate e = []);
  (* negative: with the [true <> false] axiom, p(a)=false and a=b => p(b) entailed false
     (provably distinct from true_const via the axiom). *)
  let e2 = Euf.create ctx in
  Euf.register_term e2 pa;
  Euf.register_term e2 pb;
  Euf.assert_neq e2 ~premise:0 tt ff;
  Euf.assert_eq e2 ~premise:1 pa ff;
  ignore (Euf.propagate e2 : Euf.implied list);
  Euf.assert_eq e2 ~premise:2 a b;
  (match
     List.filter (fun (i : Euf.implied) -> Term.equal i.Euf.atom pb) (Euf.propagate e2)
   with
   | [ imp ] ->
     check "pred-prop-: value false" (not imp.Euf.value);
     check
       "pred-prop-: explanation = {0;1;2}"
       (List.sort compare (Euf.explain_implied e2 imp) = [ 0; 1; 2 ])
   | _ -> check "pred-prop-: exactly one implied (p b)" false);
  (* a nullary Bool App (bare Bool variable) is NOT watched: it can only be merged with
     true/false by a direct assertion, so a watch would only echo it. *)
  let q = Env.declare_fun env "q" (Rank.create [] Sort.bool) in
  let e3 = Euf.create ctx in
  let qc = Context.const ctx q in
  Euf.register_term e3 qc;
  Euf.assert_eq e3 ~premise:5 qc tt;
  check "pred-prop: bare Bool var not watched (no self-report)" (Euf.propagate e3 = [])
;;

(* ------------------------------------------------------------------ *)
(* 4. Randomized cross-check: equivalence classes + consistency verdict. *)

let premctr = ref 0

let fresh_prem () =
  incr premctr;
  !premctr
;;

let rand_case_count = ref 0

let test_random_crosscheck () =
  set_seed 0xC0FFEE1234;
  let trials = 500 in
  for _ = 1 to trials do
    let ctx, univ = build_universe () in
    let n = Array.length univ in
    let nz = Naive.build univ in
    let e = Euf.create ctx in
    Array.iter (Euf.register_term e) univ;
    let eqs = ref [] in
    let diseqs = ref [] in
    let steps = 6 + rand_int 8 in
    for _ = 1 to steps do
      incr rand_case_count;
      let i = rand_int n
      and j = rand_int n in
      if rand_int 10 < 7
      then (
        Euf.assert_eq e ~premise:(fresh_prem ()) univ.(i) univ.(j);
        eqs := (i, j) :: !eqs)
      else (
        Euf.assert_neq e ~premise:(fresh_prem ()) univ.(i) univ.(j);
        diseqs := (i, j) :: !diseqs)
    done;
    Naive.saturate nz (List.rev !eqs);
    (* equivalence relation matches *)
    let classes_ok = ref true in
    for i = 0 to n - 1 do
      for j = i + 1 to n - 1 do
        if Euf.are_equal e univ.(i) univ.(j) <> Naive.equal nz i j
        then classes_ok := false
      done
    done;
    check "crosscheck: classes match" !classes_ok;
    (* consistency verdict matches *)
    let naive_consistent =
      List.for_all (fun (i, j) -> not (Naive.equal nz i j)) !diseqs
    in
    let euf_consistent =
      match Euf.check e with
      | Euf.Consistent -> true
      | Euf.Conflict _ -> false
    in
    check "crosscheck: consistency matches" (naive_consistent = euf_consistent);
    (* explanation soundness on a random equal pair (independent replay) *)
    if euf_consistent
    then (
      let i = rand_int n
      and j = rand_int n in
      if Euf.are_equal e univ.(i) univ.(j)
      then (
        let prems = Euf.explain e univ.(i) univ.(j) in
        (* soundness: premises are a subset of the asserted equalities (the dedicated
           replay-suffices check lives in [test_explanation_soundness]) *)
        check
          "crosscheck: explain premises are asserted"
          (List.for_all (fun p -> p >= 1 && p <= !premctr) prems)))
  done
;;

(* Dedicated explanation-soundness replay: rebuild the naive closure from ONLY the
   returned premises and confirm they suffice (mirrors the engine self-check, from the
   test side, over the shared universe). *)
let test_explanation_soundness () =
  set_seed 0x5EED0009;
  let trials = 300 in
  for _ = 1 to trials do
    let ctx, univ = build_universe () in
    let n = Array.length univ in
    let e = Euf.create ctx in
    Array.iter (Euf.register_term e) univ;
    (* premise id -> (i,j) so we can replay the exact equalities the explanation cites *)
    let prem_of = Hashtbl.create 32 in
    let steps = 6 + rand_int 8 in
    for _ = 1 to steps do
      let i = rand_int n
      and j = rand_int n in
      let p = fresh_prem () in
      Euf.assert_eq e ~premise:p univ.(i) univ.(j);
      Hashtbl.replace prem_of p (i, j)
    done;
    match Euf.check e with
    | Euf.Conflict _ -> ()
    | Euf.Consistent ->
      let i = rand_int n
      and j = rand_int n in
      if Euf.are_equal e univ.(i) univ.(j)
      then (
        let prems = Euf.explain e univ.(i) univ.(j) in
        let nz = Naive.build univ in
        let seed = List.map (fun p -> Hashtbl.find prem_of p) prems in
        Naive.saturate nz seed;
        check "explanation replay suffices" (Naive.equal nz i j))
  done
;;

(* ------------------------------------------------------------------ *)
(* 5. Randomized interleaved assert/push/pop/check vs scratch recomputation. *)

let test_pushpop () =
  set_seed 0xBEEF7777;
  let sequences = 300 in
  for _ = 1 to sequences do
    let ctx, univ = build_universe () in
    let n = Array.length univ in
    let nz = Naive.build univ in
    let e = Euf.create ctx in
    Array.iter (Euf.register_term e) univ;
    (* active assertions, most-recent-first, with the frame boundaries as counts. *)
    let active = ref [] in
    (* each: `Eq (i,j) | `Neq (i,j) *)
    let frames = ref [] in
    let verify () =
      let eqs =
        List.filter_map
          (function
            | `Eq (i, j) -> Some (i, j)
            | _ -> None)
          !active
      in
      let diseqs =
        List.filter_map
          (function
            | `Neq (i, j) -> Some (i, j)
            | _ -> None)
          !active
      in
      Naive.saturate nz eqs;
      let ok = ref true in
      for i = 0 to n - 1 do
        for j = i + 1 to n - 1 do
          if Euf.are_equal e univ.(i) univ.(j) <> Naive.equal nz i j then ok := false
        done
      done;
      check "pushpop: classes match scratch" !ok;
      let naive_ok = List.for_all (fun (i, j) -> not (Naive.equal nz i j)) diseqs in
      let euf_ok =
        match Euf.check e with
        | Euf.Consistent -> true
        | Euf.Conflict _ -> false
      in
      check "pushpop: consistency matches scratch" (naive_ok = euf_ok)
    in
    let steps = 12 + rand_int 12 in
    for _ = 1 to steps do
      match rand_int 10 with
      | 0 | 1 ->
        Euf.push e;
        frames := List.length !active :: !frames
      | 2 when !frames <> [] ->
        (* pop 1 or 2 frames *)
        let k = if List.length !frames >= 2 && rand_int 2 = 0 then 2 else 1 in
        let rec nth l k =
          match l, k with
          | x :: _, 1 -> x
          | _ :: tl, k -> nth tl (k - 1)
          | [], _ -> 0
        in
        let target = nth !frames k in
        Euf.pop e k;
        let rec drop_frames l k =
          if k = 0
          then l
          else (
            match l with
            | _ :: tl -> drop_frames tl (k - 1)
            | [] -> [])
        in
        frames := drop_frames !frames k;
        let rec take_last cnt l =
          if List.length l <= cnt
          then l
          else (
            match l with
            | _ :: tl -> take_last cnt tl
            | [] -> [])
        in
        active := take_last target !active
      | _ ->
        let i = rand_int n
        and j = rand_int n in
        if rand_int 10 < 7
        then (
          Euf.assert_eq e ~premise:(fresh_prem ()) univ.(i) univ.(j);
          active := `Eq (i, j) :: !active)
        else (
          Euf.assert_neq e ~premise:(fresh_prem ()) univ.(i) univ.(j);
          active := `Neq (i, j) :: !active)
    done;
    verify ()
  done
;;

(* 5a. Incremental [propagate] under push/pop equals a from-scratch full rescan
   (same-model H1 / codex R5). An INDEPENDENT full-scan reference — its own last-reported
   map, snapshotted on push and restored on pop exactly as the engine trails
   [w_reported] + the [prop_mark] watermark — is driven in lockstep with the engine over a
   randomized assert/neq/push/pop stream; the two per-step [propagate] outputs must be
   identical. This machine-checks the watermark trap the incremental delta introduces: a
   union propagated at a DEEPER level and then popped must be re-reported at the shallower
   level (the engine restores [prop_mark] to its push-time value on pop; a bug that didn't
   would MISS the re-report here, while the reference — restoring its map on pop — would
   still emit it, so the outputs diverge).

   This is ALSO the oracle for the per-call separated-root-pair hash set in
   [Euf.propagate] (#103): the reference's [status] computes distinct-ness by an
   independent full scan over [active_diseqs] (never touching the engine's set), so any
   divergence between the engine's O(1) membership test and the true scan — a
   lost/duplicated pair, a missed normalization, a stale (pre-merge) rep, a skipped
   rebuild — surfaces here as an output mismatch. Watched atoms and disequalities are
   deliberately dense (many pairs collapse onto shared roots under merging, the QG shape)
   so the set is non-trivially populated and both pair orientations are exercised. See the
   mutants registry [euf_propagate_sep_*] patches. *)
let test_propagate_pushpop_vs_full () =
  set_seed 0x9A7C0FFE;
  let sequences = 600 in
  for _ = 1 to sequences do
    let ctx, univ = build_universe () in
    let n = Array.length univ in
    let e = Euf.create ctx in
    Array.iter (Euf.register_term e) univ;
    (* distinct watched Eq atoms over random non-reflexive pairs (registering an Eq
       watches it); deduped by the hash-consed term, matching the engine's single watch
       per atom. *)
    let watch_tbl = Term.Table.create 16 in
    for _ = 1 to 6 + rand_int 8 do
      let i = rand_int n
      and j = rand_int n in
      if i <> j
      then (
        let atom = Context.eq ctx univ.(i) univ.(j) in
        Euf.register_term e atom;
        Term.Table.replace watch_tbl atom (i, j))
    done;
    let watch = Term.Table.fold (fun a p acc -> (a, p) :: acc) watch_tbl [] in
    (* reference last-reported value per watched atom (-1 unknown / 0 distinct / 1 equal) *)
    let reported = Term.Table.create 16 in
    List.iter (fun (a, _) -> Term.Table.replace reported a (-1)) watch;
    let active_diseqs = ref [] in
    (* per-level snapshot (active diseqs, reported copy); head = current level, like the
       engine's trail + level record. *)
    let frames = ref [] in
    (* ground-truth status of a watched pair at the current engine class structure —
       mirrors [distinct_witness]: distinct iff some active diseq separates exactly its
       two classes. *)
    let status (i, j) =
      if Euf.are_equal e univ.(i) univ.(j)
      then 1
      else if List.exists
                (fun (c, d) ->
                  (Euf.are_equal e univ.(i) univ.(c) && Euf.are_equal e univ.(j) univ.(d))
                  || (Euf.are_equal e univ.(i) univ.(d)
                      && Euf.are_equal e univ.(j) univ.(c)))
                !active_diseqs
      then 0
      else -1
    in
    let step_and_check () =
      let engine_out =
        List.filter_map
          (fun (imp : Euf.implied) ->
            match Term.Table.find_opt reported imp.Euf.atom with
            | Some _ -> Some (imp.Euf.atom.Term.tag, imp.Euf.value)
            | None -> None)
          (Euf.propagate e)
      in
      let ref_out = ref [] in
      List.iter
        (fun (a, p) ->
          let s = status p in
          if s <> -1 && s <> Term.Table.find reported a
          then (
            Term.Table.replace reported a s;
            ref_out := (a.Term.tag, s = 1) :: !ref_out))
        watch;
      let srt l = List.sort compare l in
      check
        "propagate incremental == full rescan (push/pop)"
        (srt engine_out = srt !ref_out)
    in
    let steps = 12 + rand_int 12 in
    for _ = 1 to steps do
      match rand_int 10 with
      | 0 | 1 ->
        Euf.push e;
        frames := (!active_diseqs, Term.Table.copy reported) :: !frames
      | 2 when !frames <> [] ->
        let k = if List.length !frames >= 2 && rand_int 2 = 0 then 2 else 1 in
        let rec nth l k =
          match l, k with
          | x :: _, 1 -> Some x
          | _ :: tl, k -> nth tl (k - 1)
          | [], _ -> None
        in
        (match nth !frames k with
         | None -> ()
         | Some (dsnap, rsnap) ->
           Euf.pop e k;
           let rec drop l k =
             if k = 0
             then l
             else (
               match l with
               | _ :: tl -> drop tl (k - 1)
               | [] -> [])
           in
           frames := drop !frames k;
           active_diseqs := dsnap;
           Term.Table.reset reported;
           Term.Table.iter (fun a v -> Term.Table.replace reported a v) rsnap)
      | 3 | 4 ->
        (* propagate + compare only INTERMITTENTLY — a propagate is NOT run after every
           assert, so a merge can be logged and then a push taken before any propagate
           (prop_mark < touched-length at push). That is exactly the gap the pop-restore
           of [prop_mark] must reopen: without it the union goes unre-reported at the
           shallower level. Propagating every step would keep the gap empty and hide the
           trap. *)
        step_and_check ()
      | _ ->
        let i = rand_int n
        and j = rand_int n in
        if rand_int 10 < 6
        then Euf.assert_eq e ~premise:(fresh_prem ()) univ.(i) univ.(j)
        else (
          Euf.assert_neq e ~premise:(fresh_prem ()) univ.(i) univ.(j);
          active_diseqs := (i, j) :: !active_diseqs)
    done;
    (* a final reconciliation so every sequence ends on a compared state *)
    step_and_check ()
  done
;;

(* 5a'. Same incremental-vs-full-rescan cross-check, but the watch set now includes
   PREDICATE atoms (Bool-codomain apps watched against true_const, the ⊤/⊥ bridge, #136)
   alongside Eq atoms, exercised through randomized push/pop. A predicate watch is a watch
   over the pair (pred, true_const), so its ground-truth [status] is the same
   distinct-witness scan as an Eq (equal-to-true ⇒ true; separated-from-true by a diseq —
   e.g. the level-0 true<>false axiom once pred~false — ⇒ false). Self-contained universe
   (own env with a predicate + true/false consts) so it neither perturbs {!build_universe}
   nor the Eq-only oracle above. Any divergence between the engine's incremental predicate
   propagation and the full scan (a lost/stale predicate watch across pop, a missed
   re-report) surfaces as a mismatch. *)
let test_predicate_propagate_pushpop_vs_full () =
  set_seed 0x5AFEB0075EED;
  let sequences = 400 in
  for _ = 1 to sequences do
    let env = Env.create () in
    let u = Env.declare_sort env "U" in
    let usort = Sort.uninterpreted u in
    let konst name = Env.declare_fun env name (Rank.create [] usort) in
    let p = Env.declare_fun env "p" (Rank.create [ usort ] Sort.bool) in
    let ctx = Context.create env in
    let c0 = Context.const ctx (konst "c0")
    and c1 = Context.const ctx (konst "c1")
    and c2 = Context.const ctx (konst "c2") in
    let pa = Context.app ctx p [ c0 ]
    and pb = Context.app ctx p [ c1 ] in
    let tt = Context.bool_const ctx true
    and ff = Context.bool_const ctx false in
    let univ = [| c0; c1; c2; pa; pb; tt; ff |] in
    let it = 5 in
    let iff = 6 in
    let n = Array.length univ in
    let e = Euf.create ctx in
    Array.iter (Euf.register_term e) univ;
    (* level-0 true<>false axiom (mirrors the adapter); asserted before any push, so it is
       in every frame's snapshot and never popped. *)
    Euf.assert_neq e ~premise:(fresh_prem ()) tt ff;
    (* watch set: both predicate atoms (each auto-watched vs true_const; status pair is
       (pred_idx, it)) + a few random Eq atoms, deduped by hash-consed term. *)
    let watch_tbl = Term.Table.create 16 in
    Term.Table.replace watch_tbl pa (3, it);
    Term.Table.replace watch_tbl pb (4, it);
    (* Eq watch atoms only among the three usort consts (indices 0..2): [Context.eq]
       sort-checks, so an Eq must join same-sorted terms. The random assert loop below is
       sort-agnostic (raw engine merges), so it still exercises pred/true/false classes. *)
    for _ = 1 to 2 + rand_int 4 do
      let i = rand_int 3
      and j = rand_int 3 in
      if i <> j
      then (
        let atom = Context.eq ctx univ.(i) univ.(j) in
        Euf.register_term e atom;
        Term.Table.replace watch_tbl atom (i, j))
    done;
    let watch = Term.Table.fold (fun a pr acc -> (a, pr) :: acc) watch_tbl [] in
    let reported = Term.Table.create 16 in
    List.iter (fun (a, _) -> Term.Table.replace reported a (-1)) watch;
    let active_diseqs = ref [ it, iff ] in
    let frames = ref [] in
    let status (i, j) =
      if Euf.are_equal e univ.(i) univ.(j)
      then 1
      else if List.exists
                (fun (c, d) ->
                  (Euf.are_equal e univ.(i) univ.(c) && Euf.are_equal e univ.(j) univ.(d))
                  || (Euf.are_equal e univ.(i) univ.(d)
                      && Euf.are_equal e univ.(j) univ.(c)))
                !active_diseqs
      then 0
      else -1
    in
    let step_and_check () =
      let engine_out =
        List.filter_map
          (fun (imp : Euf.implied) ->
            match Term.Table.find_opt reported imp.Euf.atom with
            | Some _ -> Some (imp.Euf.atom.Term.tag, imp.Euf.value)
            | None -> None)
          (Euf.propagate e)
      in
      let ref_out = ref [] in
      List.iter
        (fun (a, pr) ->
          let s = status pr in
          if s <> -1 && s <> Term.Table.find reported a
          then (
            Term.Table.replace reported a s;
            ref_out := (a.Term.tag, s = 1) :: !ref_out))
        watch;
      let srt l = List.sort compare l in
      check
        "pred propagate incremental == full rescan (push/pop)"
        (srt engine_out = srt !ref_out)
    in
    let steps = 12 + rand_int 12 in
    for _ = 1 to steps do
      match rand_int 10 with
      | 0 | 1 ->
        Euf.push e;
        frames := (!active_diseqs, Term.Table.copy reported) :: !frames
      | 2 when !frames <> [] ->
        let k = if List.length !frames >= 2 && rand_int 2 = 0 then 2 else 1 in
        let rec nth l k =
          match l, k with
          | x :: _, 1 -> Some x
          | _ :: tl, k -> nth tl (k - 1)
          | [], _ -> None
        in
        (match nth !frames k with
         | None -> ()
         | Some (dsnap, rsnap) ->
           Euf.pop e k;
           let rec drop l k =
             if k = 0
             then l
             else (
               match l with
               | _ :: tl -> drop tl (k - 1)
               | [] -> [])
           in
           frames := drop !frames k;
           active_diseqs := dsnap;
           Term.Table.reset reported;
           Term.Table.iter (fun a v -> Term.Table.replace reported a v) rsnap)
      | 3 | 4 -> step_and_check ()
      | _ ->
        let i = rand_int n
        and j = rand_int n in
        if rand_int 10 < 6
        then Euf.assert_eq e ~premise:(fresh_prem ()) univ.(i) univ.(j)
        else (
          Euf.assert_neq e ~premise:(fresh_prem ()) univ.(i) univ.(j);
          active_diseqs := (i, j) :: !active_diseqs)
    done;
    step_and_check ()
  done
;;

(* 5b. Registration INSIDE a frame is undone by pop (e-node truncation + use-list
   restore), and re-registration after pop rederives congruence. *)

let test_register_in_frame () =
  let _env, _u, unary, konst = make_env () in
  let ctx = Context.create _env in
  let a = Context.const ctx (konst "a") in
  let b = Context.const ctx (konst "b") in
  let d = Context.const ctx (konst "d") in
  let f = unary "f" in
  let fa = Context.app ctx f [ a ]
  and fb = Context.app ctx f [ b ] in
  let e = Euf.create ctx in
  Euf.register_term e a;
  Euf.register_term e b;
  Euf.register_term e d;
  Euf.assert_eq e ~premise:1 a b;
  let n0 = Euf.num_terms e in
  Euf.push e;
  Euf.register_term e fa;
  Euf.register_term e fb;
  check "frame-reg: f a ~ f b (a~b congruence)" (Euf.are_equal e fa fb);
  check "frame-reg: grew" (Euf.num_terms e > n0);
  Euf.pop e 1;
  check "frame-reg: e-nodes truncated on pop" (Euf.num_terms e = n0);
  (* use-list of a's root was restored: asserting into it must not crash *)
  Euf.assert_eq e ~premise:2 a d;
  check "frame-reg: a ~ d after pop" (Euf.are_equal e a d);
  (* re-registering rederives the congruence (a~b still holds at level 0) *)
  Euf.register_term e fa;
  Euf.register_term e fb;
  check "frame-reg: f a ~ f b rederived" (Euf.are_equal e fa fb);
  check "frame-reg: consistent" (Euf.check e = Euf.Consistent)
;;

(* ------------------------------------------------------------------ *)
(* 6. Determinism: identical input twice => identical propagation + explanation. *)

let scripted_run ctx univ watch script =
  let e = Euf.create ctx in
  Array.iter (Euf.register_term e) univ;
  List.iter (Euf.register_term e) watch;
  let props = ref [] in
  List.iter
    (fun (i, j, p) ->
      Euf.assert_eq e ~premise:p univ.(i) univ.(j);
      let step =
        List.map (fun imp -> imp.Euf.atom.Term.tag, imp.Euf.value) (Euf.propagate e)
      in
      props := step :: !props)
    script;
  (* also a fixed explanation, if the pair is equal *)
  let expl =
    if Euf.are_equal e univ.(0) univ.(1) then Euf.explain e univ.(0) univ.(1) else []
  in
  List.rev !props, expl
;;

let test_determinism () =
  let ctx, univ = build_universe () in
  let watch = [ Context.eq ctx univ.(0) univ.(1); Context.eq ctx univ.(3) univ.(4) ] in
  let script = [ 0, 3, 101; 1, 4, 102; 3, 4, 103; 0, 1, 104; 2, 5, 105 ] in
  let r1 = scripted_run ctx univ watch script in
  let r2 = scripted_run ctx univ watch script in
  check "determinism: propagation streams identical" (fst r1 = fst r2);
  check "determinism: explanation identical" (snd r1 = snd r2)
;;

(* ------------------------------------------------------------------ *)
(* Error-contract spot checks. *)

let test_errors () =
  let _env, _u, unary, konst = make_env () in
  ignore unary;
  let ctx = Context.create _env in
  let a = Context.const ctx (konst "a") in
  let b = Context.const ctx (konst "b") in
  let e = Euf.create ctx in
  Euf.register_term e a;
  Euf.register_term e b;
  check_raises "explain of unequal terms raises" (fun () -> Euf.explain e a b);
  check_raises "pop too many frames raises" (fun () -> Euf.pop e 1);
  Euf.push e;
  Euf.assert_eq e ~premise:1 a b;
  check "after push+assert, a~b" (Euf.are_equal e a b);
  Euf.pop e 1;
  check "after pop, a not~ b" (not (Euf.are_equal e a b));
  check "after pop, check consistent" (Euf.check e = Euf.Consistent)
;;

(* ------------------------------------------------------------------ *)
(* 7. Read-only query API (ADR-0012 L2 / R6): app_terms_by_symbol / find_class_opt /
   equal_if_registered / class_members are GENUINELY NON-MUTATING. A dense query workload
   over a rich e-graph must leave every observable — num_terms, the full are_equal matrix,
   the check verdict — byte-identical, and a query on an UNREGISTERED term must NOT
   register it. Discrimination (the mutation-detector has teeth): the REGISTERING
   accessors [are_equal]/[class_of] DO grow num_terms, so if the read-only accessors ever
   start registering (e.g. someone wires them to [register], or adds path compression that
   grows state), the invariance below breaks. This is the test the team-lead required (req
   A).

   RESIDUAL (documented, accepted by review): this snapshot (num_terms + are_equal
   matrix + check verdict) would NOT catch a BENIGN path compression added to [find] — one
   that rewrites parent pointers WITHOUT changing class roots or growing state. Such a
   rewrite is invisible to every observable here (roots and are_equal are preserved by
   definition). The guard against that is the engine's own push/pop/backtracking
   crosschecks ([test_pushpop]/[test_register_in_frame]/[test_propagate_pushpop_vs_full]):
   an untrailed pointer rewrite corrupts backtracking and breaks those. This test pins the
   REGISTRATION hazard (the R6 concern); the backtracking suite pins mutation-under-find. *)
let test_query_api_nonmutating () =
  let env, _u, unary, konst = make_env () in
  let ctx = Context.create env in
  let a = Context.const ctx (konst "a") in
  let b = Context.const ctx (konst "b") in
  let c = Context.const ctx (konst "c") in
  let f = unary "f"
  and g = unary "g" in
  let fa = Context.app ctx f [ a ]
  and fb = Context.app ctx f [ b ]
  and gc = Context.app ctx g [ c ] in
  let e = Euf.create ctx in
  List.iter (Euf.register_term e) [ a; b; c; fa; fb; gc ];
  Euf.assert_eq e ~premise:1 a b (* => fa ~ fb by congruence *);
  Euf.assert_eq e ~premise:2 c fa (* => c ~ fa ~ fb *);
  let terms = [ a; b; c; fa; fb; gc ] in
  (* observable-state snapshot: num_terms + full are_equal matrix + check verdict *)
  let snapshot () =
    ( Euf.num_terms e
    , List.map (fun t1 -> List.map (fun t2 -> Euf.are_equal e t1 t2) terms) terms
    , Euf.check e = Euf.Consistent )
  in
  let before = snapshot () in
  (* dense query workload through the read-only API only *)
  for _ = 1 to 1000 do
    List.iter
      (fun t ->
        ignore (Euf.app_terms_by_symbol e f : Term.t list);
        ignore (Euf.app_terms_by_symbol e g : Term.t list);
        ignore (Euf.find_class_opt e t : int option);
        ignore (Euf.class_members e t : Term.t list);
        List.iter (fun t2 -> ignore (Euf.equal_if_registered e t t2 : bool)) terms)
      terms
  done;
  check
    "query-api: observable state (num_terms, are_equal matrix, check) unchanged by dense \
     queries"
    (before = snapshot ());
  (* the read-only accessors agree with the engine's own are_equal on registered terms *)
  check
    "query-api: equal_if_registered agrees with are_equal on registered terms"
    (List.for_all
       (fun t1 ->
         List.for_all
           (fun t2 -> Euf.equal_if_registered e t1 t2 = Euf.are_equal e t1 t2)
           terms)
       terms);
  (* app_terms_by_symbol returns exactly the registered f-apps in id order *)
  check
    "query-api: app_terms_by_symbol f = [fa; fb]"
    (Euf.app_terms_by_symbol e f = [ fa; fb ]);
  check
    "query-api: class_members fa = its congruence class {c, fa, fb} (id order)"
    (Euf.class_members e fa = [ c; fa; fb ]);
  (* DISCRIMINATOR (part 1): a query on an UNREGISTERED term is inert — None / singleton /
     no growth. *)
  let d = Context.const ctx (konst "d") in
  let n = Euf.num_terms e in
  check "query-api: find_class_opt on unregistered -> None" (Euf.find_class_opt e d = None);
  check
    "query-api: class_members on unregistered -> singleton [d]"
    (Euf.class_members e d = [ d ]);
  ignore (Euf.equal_if_registered e d a : bool);
  check
    "query-api: unregistered queries did NOT register (num_terms constant)"
    (Euf.num_terms e = n);
  (* DISCRIMINATOR (part 2): the REGISTERING accessors DO grow num_terms — proving the
     invariance above is a live mutation detector, not vacuous. *)
  ignore (Euf.class_of e d : int);
  check
    "query-api: are_equal/class_of DO register (num_terms grows) — detector has teeth"
    (Euf.num_terms e > n)
;;

(* ================================================================== *)
(* ADR-0014 Stage 2 merge-notification log (multi-consumer cursor API) + Stage 3 per-class
   tag. Discriminating on the callback + data behaviours a datatypes client needs:
   fire-on-merge (asserted + congruence), correct reps, drain/cursor semantics, two
   independent consumers each seeing every merge, unwind-on-pop / re-fire-on-reassert. *)
let test_stage2_merge_log () =
  let _env, _u, unary, konst = make_env () in
  let ctx = Context.create _env in
  let a = Context.const ctx (konst "a") in
  let b = Context.const ctx (konst "b") in
  let f = unary "f" in
  let fa = Context.app ctx f [ a ]
  and fb = Context.app ctx f [ b ] in
  let has ms u v =
    List.exists
      (fun (m : Euf.merge_event) ->
        (Term.equal m.kept u && Term.equal m.merged v)
        || (Term.equal m.kept v && Term.equal m.merged u))
      ms
  in
  (* default OFF ⇒ no recording, zero cost, byte-identical to trunk. *)
  let e0 = Euf.create ctx in
  let c0 = Euf.add_merge_consumer e0 in
  Euf.register_term e0 fa;
  Euf.register_term e0 fb;
  Euf.assert_eq e0 ~premise:1 a b;
  check "stage2: merge log OFF by default ⇒ empty" (Euf.drain_merges e0 c0 = []);
  (* ON ⇒ fire on the asserted merge AND the congruence merge it triggers; TWO consumers
     each see every merge (multi-consumer cursors). *)
  let e = Euf.create ctx in
  Euf.set_record_merges e true;
  let c1 = Euf.add_merge_consumer e in
  let c2 = Euf.add_merge_consumer e in
  Euf.register_term e fa;
  Euf.register_term e fb;
  Euf.assert_eq e ~premise:1 a b;
  let ms = Euf.drain_merges e c1 in
  check "stage2: asserted merge (a,b) fires" (has ms a b);
  check "stage2: congruence merge (f a, f b) fires (correct reps)" (has ms fa fb);
  check "stage2: cursor drains (second drain empty)" (Euf.drain_merges e c1 = []);
  check
    "stage2: second consumer independently sees every merge"
    (has (Euf.drain_merges e c2) fa fb);
  (* unwind: a merge inside a frame must NOT survive its [pop] (no-fire-after-rewind). *)
  let e2 = Euf.create ctx in
  Euf.set_record_merges e2 true;
  let c3 = Euf.add_merge_consumer e2 in
  Euf.register_term e2 fa;
  Euf.register_term e2 fb;
  Euf.push e2;
  Euf.assert_eq e2 ~premise:1 a b;
  Euf.pop e2 1;
  check "stage2: pop clears the undrained log (unwind)" (Euf.drain_merges e2 c3 = []);
  check "stage2: pop undoes the union (a,b no longer equal)" (not (Euf.are_equal e2 a b));
  (* re-fire: re-asserting after the pop re-logs the merge. *)
  Euf.assert_eq e2 ~premise:1 a b;
  check
    "stage2: re-assert after pop re-fires the merge"
    (has (Euf.drain_merges e2 c3) a b)
;;

(* ADR-0014 Stage 3 per-class tag (datatypes-scoped): attach/read, inheritance on merge,
   collision surfaced via the merge event, pop restoration. *)
let test_stage3_class_tag () =
  let _env, _u, _unary, konst = make_env () in
  let ctx = Context.create _env in
  let a = Context.const ctx (konst "a")
  and b = Context.const ctx (konst "b")
  and c = Context.const ctx (konst "c") in
  let cA = Context.const ctx (konst "ctorA")
  and cB = Context.const ctx (konst "ctorB") in
  let e = Euf.create ctx in
  Euf.set_record_merges e true;
  let cur = Euf.add_merge_consumer e in
  Euf.set_class_tag e a cA;
  check "stage3: class_tag reads the attached witness" (Euf.class_tag e a = Some cA);
  check "stage3: an untagged class reads None" (Euf.class_tag e b = None);
  (* inheritance: b (untagged) merged into a (tagged) ⇒ the class carries a's tag. *)
  Euf.assert_eq e ~premise:1 a b;
  ignore (Euf.drain_merges e cur : Euf.merge_event list);
  check
    "stage3: untagged inherits the tagged class's witness on merge"
    (Euf.class_tag e b = Some cA);
  (* collision: c gets a DIFFERENT tag, then c merges the a/b class ⇒ the merge event
     surfaces BOTH tags (the ctor-clash signal a datatypes client refutes on). *)
  Euf.set_class_tag e c cB;
  Euf.assert_eq e ~premise:2 b c;
  let evs = Euf.drain_merges e cur in
  let saw_collision =
    List.exists
      (fun (m : Euf.merge_event) ->
        match m.kept_tag, m.merged_tag with
        | Some x, Some y ->
          (Term.equal x cA && Term.equal y cB) || (Term.equal x cB && Term.equal y cA)
        | _ -> false)
      evs
  in
  check
    "stage3: a merge of two tagged classes surfaces BOTH tags (clash signal)"
    saw_collision;
  (* pop restoration: a tag attached in a frame is gone after its pop. *)
  let e2 = Euf.create ctx in
  Euf.push e2;
  Euf.set_class_tag e2 a cA;
  check "stage3: tag present inside the frame" (Euf.class_tag e2 a = Some cA);
  Euf.pop e2 1;
  check "stage3: tag restored (gone) after pop" (Euf.class_tag e2 a = None)
;;

(* Task #47 COLLISION RED: the packed small-arity signature key must be INJECTIVE — two
   distinct signatures must never pack to the same key. These assertions PASS on the real
   packer and FAIL on a broken one (a narrowed arg/sym field, or a dropped arity tag); the
   mutant demonstration (narrow [sig_pack_arg_bits] by one) reddens the near-collision and
   arity-tag checks. See logs/euf-sigpack-log.md. *)
let test_sig_pack_injective () =
  let pack ~n ~s ~a0 ~a1 = Euf.Debug.pack_signature_fields ~n ~s ~a0 ~a1 in
  let argb = Euf.Debug.sig_pack_arg_bits in
  let symb = Euf.Debug.sig_pack_sym_bits in
  (* (1) NEAR-COLLISION on the top arg bit: two arity-2 sigs differing ONLY in bit
         [argb-1] of a0. In range for the real [argb]-bit field => distinct keys; a broken
         [argb-1]-bit packer truncates that bit => same key (RED). *)
  let top = 1 lsl (argb - 1) in
  let k_lo = pack ~n:2 ~s:7 ~a0:0 ~a1:3 in
  let k_hi = pack ~n:2 ~s:7 ~a0:top ~a1:3 in
  check
    "sigpack: near-collision top-arg-bit distinguished"
    (k_lo >= 0 && k_hi >= 0 && k_lo <> k_hi);
  (* same for a1's top bit *)
  let k1_lo = pack ~n:2 ~s:7 ~a0:5 ~a1:0 in
  let k1_hi = pack ~n:2 ~s:7 ~a0:5 ~a1:top in
  check
    "sigpack: near-collision top-a1-bit distinguished"
    (k1_lo >= 0 && k1_hi >= 0 && k1_lo <> k1_hi);
  (* top sym bit *)
  let stop = 1 lsl (symb - 1) in
  let ks_lo = pack ~n:2 ~s:1 ~a0:5 ~a1:3 in
  let ks_hi = pack ~n:2 ~s:(1 lor stop) ~a0:5 ~a1:3 in
  check
    "sigpack: near-collision top-sym-bit distinguished"
    (ks_lo >= 0 && ks_hi >= 0 && ks_lo <> ks_hi);
  (* (2) ARITY-TAG disjointness: (n=1,s,a0) vs (n=2,s,a0,a1=0) must differ even though the
     n=1 case ignores a1 (RED against a dropped tag). *)
  let k_ar1 = pack ~n:1 ~s:9 ~a0:4 ~a1:0 in
  let k_ar2 = pack ~n:2 ~s:9 ~a0:4 ~a1:0 in
  let k_ar0 = pack ~n:0 ~s:9 ~a0:0 ~a1:0 in
  check "sigpack: arity tags disjoint" (k_ar0 <> k_ar1 && k_ar1 <> k_ar2 && k_ar0 <> k_ar2);
  (* (3) RANGE FALLBACK: an out-of-range field returns -1 (never a truncated key). *)
  check "sigpack: sym overflow -> -1" (pack ~n:0 ~s:(1 lsl symb) ~a0:0 ~a1:0 = -1);
  check "sigpack: a0 overflow -> -1" (pack ~n:1 ~s:1 ~a0:(1 lsl argb) ~a1:0 = -1);
  check "sigpack: a1 overflow -> -1" (pack ~n:2 ~s:1 ~a0:1 ~a1:(1 lsl argb) = -1);
  check "sigpack: arity>2 -> -1" (pack ~n:3 ~s:1 ~a0:1 ~a1:1 = -1);
  (* (3b) OVERFLOW-ALIAS DISCRIMINATION (rider MEDIUM): a just-out-of-range arg (2^argb)
     overflows into the NEXT field, aliasing a distinct in-range signature. The range
     checks make the overflowing pack [-1] (distinct from the alias); DELETING the arity-2
     bound check (euf.ml n=2 arm) makes both pack to the same key — this must be RED then.
     - a0 overflow: (n=2,s=6,a0=2^argb,a1=3) [a0 spills into the sym field] vs
       (n=2,s=7,a0=0,a1=3): with the a0 field 20-bit and sym at bit 40, a0=2^20 lands on
       bit 40 = sym's low bit, so the broken packer reads it as sym=7,a0=0.
     - a1 overflow: (n=2,s=6,a0=0,a1=2^argb) [a1's bit 20 spills into a0's low bit] vs
       (n=2,s=6,a0=1,a1=0): a1=2^20 lands on bit 20 = a0=1's contribution. *)
  let no_alias lhs rhs = not (lhs >= 0 && lhs = rhs) in
  check
    "sigpack: a0 overflow does not alias (RED w/o n=2 bound check)"
    (no_alias (pack ~n:2 ~s:6 ~a0:(1 lsl argb) ~a1:3) (pack ~n:2 ~s:7 ~a0:0 ~a1:3));
  check
    "sigpack: a1 overflow does not alias (RED w/o n=2 bound check)"
    (no_alias (pack ~n:2 ~s:6 ~a0:0 ~a1:(1 lsl argb)) (pack ~n:2 ~s:6 ~a0:1 ~a1:0));
  (* (4) INJECTIVITY SWEEP over a grid of in-range tuples: all packed keys distinct, none
     negative. A collision (broken packer) trips the table. *)
  let seen = Hashtbl.create 4096 in
  let collision = ref false in
  let hi = 1 lsl (argb - 1) in
  let vals = [ 0; 1; 2; hi - 1; hi; hi + 1; (1 lsl argb) - 1 ] in
  let syms = [ 0; 1; 42; (1 lsl symb) - 1 ] in
  (* Vary only the arity's MEANINGFUL fields (n=0 ignores a0/a1; n=1 ignores a1), holding
     ignored fields at 0 — so a legitimate same-signature repeat is never mistaken for a
     collision. *)
  let record n s a0 a1 =
    let k = pack ~n ~s ~a0 ~a1 in
    if k >= 0
    then
      if Hashtbl.mem seen k then collision := true else Hashtbl.add seen k (n, s, a0, a1)
  in
  List.iter (fun s -> record 0 s 0 0) syms;
  List.iter (fun s -> List.iter (fun a0 -> record 1 s a0 0) vals) syms;
  List.iter
    (fun s -> List.iter (fun a0 -> List.iter (fun a1 -> record 2 s a0 a1) vals) vals)
    syms;
  check
    "sigpack: injective over in-range grid (no two tuples share a key)"
    (not !collision)
;;

(* H2 (fix #4 watch_index guard): [rearm_watch] looks up [watch_index]([term]) in O(1) and
   guards it with (idx in range AND [watched].(idx).w_atom = term), so a STALE entry — one
   left past a [pop] truncation, or pointing at a slot a later registration REUSED for a
   different atom — is a correct no-op, exactly as the old full [Dynarray.iteri] scan was.
   The adapter never reads a stale entry (it re-registers a term, refreshing the map,
   before re-arming), so this drives the ENGINE directly to manufacture both stale shapes
   and pin the guard. *)
let test_watch_index_stale_slot () =
  let env, usort, _unary, konst = make_env () in
  let p = Env.declare_fun env "p" (Rank.create [ usort ] Sort.bool) in
  let ctx = Context.create env in
  let a = Context.const ctx (konst "a") in
  let b = Context.const ctx (konst "b") in
  let c = Context.const ctx (konst "c") in
  let pa = Context.app ctx p [ a ]
  and pb = Context.app ctx p [ b ]
  and pc = Context.app ctx p [ c ] in
  let tt = Context.bool_const ctx true in
  let e = Euf.create ctx in
  (* p(a) at base -> watch slot 0, watch_index[p(a)] = 0. *)
  Euf.register_term e pa;
  Euf.push e;
  (* p(b) in a pushed frame -> watch slot 1, watch_index[p(b)] = 1. *)
  Euf.register_term e pb;
  Euf.pop e 1;
  (* (i) OUT-OF-RANGE: p(b)'s watch was truncated (watched length back to 1) but the
     watch_index[p(b)] = 1 entry survives (never cleaned on pop). rearm must reject it (1
     not < 1) — a safe no-op rather than an out-of-bounds read. *)
  Euf.rearm_watch e pb;
  check "stale-slot: out-of-range entry is a no-op (no crash)" true;
  (* (ii) REUSED-SLOT: registering p(c) reuses the freed slot 1, so watch_index[p(b)] = 1
     now points at p(c)'s watch. Make p(c) true and drain that self-report; then rearm the
     STALE p(b): the [w_atom] guard must NOT re-arm p(c)'s slot. *)
  Euf.register_term e pc;
  Euf.assert_eq e ~premise:1 pc tt;
  (match
     List.filter (fun (i : Euf.implied) -> Term.equal i.Euf.atom pc) (Euf.propagate e)
   with
   | [ imp ] -> check "stale-slot: p(c) self-implied true" imp.Euf.value
   | _ -> check "stale-slot: exactly one p(c) self-report" false);
  check "stale-slot: nothing new before rearm" (Euf.propagate e = []);
  Euf.rearm_watch e pb;
  check
    "stale-slot: rearm of truncated p(b) does not re-report the reused-slot p(c)"
    (Euf.propagate e = []);
  (* control: rearming the LIVE p(c) DOES re-report it, proving rearm is otherwise
     effective — the no-op above is the guard rejecting a stale entry, not a dead rearm. *)
  Euf.rearm_watch e pc;
  match
    List.filter (fun (i : Euf.implied) -> Term.equal i.Euf.atom pc) (Euf.propagate e)
  with
  | [ imp ] -> check "stale-slot: rearm of live p(c) re-reports it" imp.Euf.value
  | _ -> check "stale-slot: live rearm re-reports exactly p(c)" false
;;

(* ADR-0014 Stage 4.2: sub-frame checkpoint / rewind OBS-EQ. The earliest-removed
   incremental-undo oracle. Assert a PREFIX, [checkpoint], assert a SUFFIX, then
   [rewind_to_checkpoint]: the engine's classes + consistency must match a from-scratch
   closure of ONLY the prefix (rewind reverses exactly the suffix, sub-frame). Then replay
   a survivor subset of the suffix and re-check against prefix++survivors — i.e.
   rewind(checkpoint)+replay(survivors) == full rebuild of prefix++survivors, which is the
   OBS-EQ contract the SAT-core chrono incremental undo relies on. Cross-checked against
   the INDEPENDENT naive quadratic closure (no shared code with the engine's undo trail). *)
let test_checkpoint_obs_eq () =
  set_seed 0x5242C0DE;
  for _ = 1 to 300 do
    let ctx, univ = build_universe () in
    let n = Array.length univ in
    let e = Euf.create ctx in
    Array.iter (Euf.register_term e) univ;
    let rand_lit () =
      let i = rand_int n
      and j = rand_int n in
      if rand_int 10 < 7 then `Eq (i, j) else `Neq (i, j)
    in
    let assert_lit = function
      | `Eq (i, j) -> Euf.assert_eq e ~premise:(fresh_prem ()) univ.(i) univ.(j)
      | `Neq (i, j) -> Euf.assert_neq e ~premise:(fresh_prem ()) univ.(i) univ.(j)
    in
    let eqs_of =
      List.filter_map (function
        | `Eq (i, j) -> Some (i, j)
        | `Neq _ -> None)
    in
    let diseqs_of =
      List.filter_map (function
        | `Neq (i, j) -> Some (i, j)
        | `Eq _ -> None)
    in
    let verify_against label lits =
      let nz = Naive.build univ in
      Naive.saturate nz (eqs_of lits);
      let ok = ref true in
      for i = 0 to n - 1 do
        for j = i + 1 to n - 1 do
          if Euf.are_equal e univ.(i) univ.(j) <> Naive.equal nz i j then ok := false
        done
      done;
      check (label ^ ": classes match scratch") !ok;
      let naive_ok =
        List.for_all (fun (i, j) -> not (Naive.equal nz i j)) (diseqs_of lits)
      in
      let euf_ok =
        match Euf.check e with
        | Euf.Consistent -> true
        | Euf.Conflict _ -> false
      in
      check (label ^ ": consistency matches scratch") (naive_ok = euf_ok)
    in
    let prefix = List.init (rand_int 10) (fun _ -> rand_lit ()) in
    List.iter assert_lit prefix;
    let c = Euf.checkpoint e in
    let suffix = List.init (1 + rand_int 12) (fun _ -> rand_lit ()) in
    List.iter assert_lit suffix;
    Euf.rewind_to_checkpoint e c;
    verify_against "checkpoint-rewind" prefix;
    (* replay a survivor subset of the suffix; state must match prefix ++ survivors. *)
    let survivors = List.filter (fun _ -> rand_int 2 = 0) suffix in
    List.iter assert_lit survivors;
    verify_against "checkpoint-rewind+replay" (prefix @ survivors)
  done
;;

let () =
  print_endline "euf self-test:";
  test_checkpoint_obs_eq ();
  test_textbook ();
  test_chain_selfloop ();
  test_chain_orders ();
  test_propagation ();
  test_distinct_witness_first_wins ();
  test_predicate_propagation ();
  test_watch_index_stale_slot ();
  test_errors ();
  test_random_crosscheck ();
  test_explanation_soundness ();
  test_pushpop ();
  test_propagate_pushpop_vs_full ();
  test_predicate_propagate_pushpop_vs_full ();
  test_register_in_frame ();
  test_determinism ();
  test_query_api_nonmutating ();
  test_stage2_merge_log ();
  test_stage3_class_tag ();
  test_sig_pack_injective ();
  Printf.printf
    "\neuf self-test: %d checks, %d randomized assert-cases, %d failure(s)\n"
    !checks
    !rand_case_count
    !failures;
  if !failures > 0 then exit 1
;;
