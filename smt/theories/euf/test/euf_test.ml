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
                 if
                   find t (Term.Table.find t.idx arg) <> find t (Term.Table.find t.idx bj)
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
   still emit it, so the outputs diverge). *)
let test_propagate_pushpop_vs_full () =
  set_seed 0x9A7C0FFE;
  let sequences = 300 in
  for _ = 1 to sequences do
    let ctx, univ = build_universe () in
    let n = Array.length univ in
    let e = Euf.create ctx in
    Array.iter (Euf.register_term e) univ;
    (* distinct watched Eq atoms over random non-reflexive pairs (registering an Eq
       watches it); deduped by the hash-consed term, matching the engine's single watch
       per atom. *)
    let watch_tbl = Term.Table.create 16 in
    for _ = 1 to 3 + rand_int 4 do
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
      else if
        List.exists
          (fun (c, d) ->
             (Euf.are_equal e univ.(i) univ.(c) && Euf.are_equal e univ.(j) univ.(d))
             || (Euf.are_equal e univ.(i) univ.(d) && Euf.are_equal e univ.(j) univ.(c)))
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

(* ================================================================== *)
let () =
  print_endline "euf self-test:";
  test_textbook ();
  test_chain_selfloop ();
  test_chain_orders ();
  test_propagation ();
  test_errors ();
  test_random_crosscheck ();
  test_explanation_soundness ();
  test_pushpop ();
  test_propagate_pushpop_vs_full ();
  test_register_in_frame ();
  test_determinism ();
  Printf.printf
    "\neuf self-test: %d checks, %d randomized assert-cases, %d failure(s)\n"
    !checks
    !rand_case_count
    !failures;
  if !failures > 0 then exit 1
;;
