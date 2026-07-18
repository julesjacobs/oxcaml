(* Targeted RED-mutant unit test for the store-side occurrence index (stores_by_class)
   merge-cursor invalidation — perf audit #2, the OXSMT_AX_OCCIDX store-side twin.

   The corpus does NOT exercise the merge-only staleness window (store-class changes there
   are always accompanied by a pop or a new store registration, so any single invalidator
   catches them — see logs/theory-perf-fixes-log.md §Fix #3). This drives the arr theory
   through the frozen THEORY surface to FORCE that window deterministically:

   1. register a read [select a j] and a store [st = store b i v];
   2. Final #1 -> stores_by_class builds + caches the store index ([st] keyed by its own
      e-class; [st] is NOT congruent to [a] yet, so no split);
   3. assert [a = st] AT THE SAME LEVEL. This merges [st] into [a]'s class by an ordinary
      equality: it registers NO new store (no catalog invalidation) and needs NO pop.
      [a]'s class is padded ([a = a2 = a3]) so union-by-size keeps [a]'s root — the
      surviving class root is NOT [st]'s old key, so a stale index built at Final #1
      cannot find [st] under [class_of a];
   4. Final #2 -> row_split queries [find_all (class_of a)] for stores congruent to [a].
      Real code drains the merge cursor, drops the stale index, rebuilds -> [st] is found
      -> a ROW [Split] is emitted. With the merge cursor disabled the cache is frozen at
      the Final-#1 state -> [st] is missed -> [Sat] with no split (the M-storeidx wrong
      behaviour). So Final #2 = [Split] pins the invalidation; a merge-cursor-off build
      turns it into [Sat] (RED).

   Run under OXSMT_AX_OCCIDX=1 (the store cache is only live then); with the flag off the
   theory rebuilds every call and the test still passes (the cache is simply not
   exercised). Stdlib-only + oxsmt_core + oxsmt_arr (dependency firewall I3). *)

open Oxsmt_core
module Arr = Oxsmt_arr.Arr
module Defs = Array_defs

let checks = ref 0
let failures = ref 0

let check name cond =
  incr checks;
  if not cond
  then (
    incr failures;
    Printf.printf "  FAIL %s\n" name)
;;

let () =
  let env, cap = Env.create_with_cap () in
  let ctx = Context.create env in
  let isort = Sort.uninterpreted (Env.declare_sort env "I") in
  let esort = Sort.uninterpreted (Env.declare_sort env "E") in
  let asort = Sort.array_ ~index:isort ~element:esort in
  (* store + select op symbols via the reserved door (board #58), registered so [role_of]
     classifies them; the same canonical names the parser/theory use, so terms hash-cons. *)
  let store_name = Defs.op_symbol_name Defs.Store ~index:isort ~element:esort in
  let store_sym =
    Env.declare_reserved cap env store_name (Rank.create [ asort; isort; esort ] asort)
  in
  let select_name = Defs.op_symbol_name Defs.Select ~index:isort ~element:esort in
  let select_sym =
    Env.declare_reserved cap env select_name (Rank.create [ asort; isort ] esort)
  in
  let reg = Defs.add Defs.empty store_sym Defs.Store ~index:isort ~element:esort in
  let reg = Defs.add reg select_sym Defs.Select ~index:isort ~element:esort in
  let konst name sort =
    Context.app ctx (Env.declare_fun env name (Rank.create [] sort)) []
  in
  let a = konst "a" asort in
  let a2 = konst "a2" asort in
  let a3 = konst "a3" asort in
  let b = konst "b" asort in
  let i = konst "i" isort in
  let j = konst "j" isort in
  let v = konst "v" esort in
  let st = Context.app ctx store_sym [ b; i; v ] in
  let sel = Context.app ctx select_sym [ a; j ] in
  let th = Arr.create ctx env cap reg in
  let alloc = Atom.create_allocator () in
  let reg_atom term =
    let atom = Atom.fresh alloc in
    Arr.register_atom th atom term;
    atom
  in
  let assert_eq x y =
    let atom = reg_atom (Context.eq ctx x y) in
    Arr.assert_lit th (Lit.make atom true)
  in
  let w = konst "w" esort in
  let cdummy = konst "cdummy" asort in
  (* Catalog the read [sel] and the store [st] the way they enter the theory in a real
     solve: as subterms of equality atoms. We only REGISTER these atoms (never assert
     them), so [sel]/[st] are cataloged without merging anything. *)
  ignore (reg_atom (Context.eq ctx sel w) : Atom.t);
  ignore (reg_atom (Context.eq ctx st cdummy) : Atom.t);
  (* pad [a]'s class so union-by-size keeps [a] as the surviving root when it later merges
     with the size-1 [st] class *)
  assert_eq a a2;
  assert_eq a2 a3;
  (* Drive [check Final] to a terminal (Sat/Split/Conflict), re-calling on Propagations
     the way the CDCL(T) loop does; the theory does not re-report an already-reported
     propagation, so this quiesces. Bounded to catch a runaway. *)
  let settle_final () =
    let rec go fuel =
      if fuel = 0 then failwith "settle_final: propagation did not converge";
      match Arr.check th Theory.Final with
      | Theory.Propagations (_ :: _) -> go (fuel - 1)
      | other -> other
    in
    go 1000
  in
  let is_split = function
    | Theory.Split _ -> true
    | _ -> false
  in
  let describe = function
    | Theory.Sat -> "Sat"
    | Theory.Split _ -> "Split"
    | Theory.Conflict _ -> "Conflict"
    | Theory.Propagations _ -> "Propagations"
    | Theory.Lemma _ -> "Lemma"
  in
  (* Final #1: builds + caches the store index. [st] is not congruent to [a] yet, so there
     is no ROW obligation for [select a j] -> Sat (no split). *)
  let r1 = settle_final () in
  check
    (Printf.sprintf
       "Final#1 is Sat (cache built, no store congruent to a) [got %s]"
       (describe r1))
    (match r1 with
     | Theory.Sat -> true
     | _ -> false);
  (* Same-level merge: [a = st]. No new store registered, no pop. Only the merge cursor
     can detect that [st] now sits in [a]'s class. *)
  assert_eq a st;
  (* Final #2: row_split must now find [st] congruent to [a] and emit the ROW split. A
     stale (merge-cursor-off) store index misses [st] -> Sat (RED). *)
  let r2 = settle_final () in
  check
    (Printf.sprintf
       "Final#2 emits a ROW Split for the merge-reclassed store (merge-cursor \
        invalidation load-bearing) [got %s]"
       (describe r2))
    (is_split r2);
  if !failures = 0
  then Printf.printf "arr store-idx merge-window test: %d checks, 0 failures\n" !checks
  else (
    Printf.printf
      "arr store-idx merge-window test: %d checks, %d FAILURES\n"
      !checks
      !failures;
    exit 1)
;;
