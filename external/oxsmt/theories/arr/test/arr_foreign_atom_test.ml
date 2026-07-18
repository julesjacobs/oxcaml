(* Regression test for the poison-47 robustness bug (logs/poison47-report.md).

   An AUFLIA problem (arrays + UF + LIA, quantified set programs) is routed WHOLE to the
   standalone arrays theory ({!Oxsmt_interface.Cdclt.ensure_theory} selects [TArr] as soon
   as the array registry is non-empty), so its arithmetic [Le_zero] literals ride along.
   The arrays theory registers such an atom as [K_foreign] (it does not own it) but the
   CDCL(T) seam still forwards its assignment through [assert_lit]. The old code answered
   a [K_foreign] assertion with
   [invalid_arg "... a foreign ... atom must not be asserted"], which escaped [Sat.solve]
   and hit the session's CONTRACT-POISON firewall — degrading 47 AUFLIA files to an opaque
   [poison-solve:Invalid_argument] unknown.

   The fix makes [assert_lit] IGNORE a foreign atom (the arrays theory reasons only over
   the array/EUF fragment). This test pins BOTH halves of that contract through the frozen
   THEORY surface:

   1. asserting a foreign [Le_zero] literal (either sign) does NOT raise — the crash that
      poisoned all 47 files (RED: the pre-fix [invalid_arg] fails here);
   2. after ignoring the foreign literal, the theory is still internally consistent — a
      [check Final] over a fresh (no array conflict) state returns [Sat], NOT a bogus
      [Conflict] (soundness of the array/EUF fragment is undisturbed).

   Stdlib-only + oxsmt_core + oxsmt_arr (dependency firewall I3). *)

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
  (* A non-empty array registry is what routes an AUFLIA problem to [TArr] in the first
     place; build one so this is a genuine arrays theory, not inert congruence closure. *)
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
  let th = Arr.create ctx env cap reg in
  let alloc = Atom.create_allocator () in
  let konst name sort =
    Context.app ctx (Env.declare_fun env name (Rank.create [] sort)) []
  in
  (* A LIA [Le_zero] atom [n - 1 <= 0]: [Context.le] lowers to a single gcd-normalized
     [Le] atom, which [Theory_view.atom] classifies as [Le_zero] -> the arrays theory's
     [classify] returns [K_foreign]. This is exactly the kind of atom the 47 poison files
     carry alongside their arrays. *)
  let n = konst "n" Sort.int in
  let foreign_atom_term = Context.le ctx n (Context.int_const ctx 1) in
  let assert_foreign ~positive =
    let atom = Atom.fresh alloc in
    Arr.register_atom th atom foreign_atom_term;
    Arr.assert_lit th (Lit.make atom positive)
  in
  (* (1) The crash regression: neither sign may raise. *)
  (match assert_foreign ~positive:true with
   | () -> check "assert positive foreign Le_zero literal does not raise" true
   | exception e ->
     check
       (Printf.sprintf
          "assert positive foreign Le_zero literal does not raise [raised %s]"
          (Printexc.to_string e))
       false);
  (match assert_foreign ~positive:false with
   | () -> check "assert negative foreign Le_zero literal does not raise" true
   | exception e ->
     check
       (Printf.sprintf
          "assert negative foreign Le_zero literal does not raise [raised %s]"
          (Printexc.to_string e))
       false);
  (* (2) Soundness of the array/EUF fragment is undisturbed: with no array conflict the
     theory saturates to [Sat] (the foreign atom is simply not enforced here; the
     session's array model check re-evaluates the arithmetic and fail-closes any SAT). A
     bogus [Conflict] here would be a wrong-UNSAT vector. *)
  let settle_final () =
    let rec go fuel =
      if fuel = 0 then failwith "settle_final: propagation did not converge";
      match Arr.check th Theory.Final with
      | Theory.Propagations (_ :: _) -> go (fuel - 1)
      | other -> other
    in
    go 1000
  in
  let describe = function
    | Theory.Sat -> "Sat"
    | Theory.Split _ -> "Split"
    | Theory.Conflict _ -> "Conflict"
    | Theory.Propagations _ -> "Propagations"
    | Theory.Lemma _ -> "Lemma"
  in
  let r = settle_final () in
  check
    (Printf.sprintf
       "check Final is Sat after ignoring a foreign atom (no bogus conflict) [got %s]"
       (describe r))
    (match r with
     | Theory.Sat -> true
     | _ -> false);
  if !failures = 0
  then Printf.printf "arr foreign-atom (poison-47) test: %d checks, 0 failures\n" !checks
  else (
    Printf.printf
      "arr foreign-atom (poison-47) test: %d checks, %d FAILURES\n"
      !checks
      !failures;
    exit 1)
;;
