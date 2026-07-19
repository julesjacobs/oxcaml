(* The cross-theory half of OXSMT_LIA_EQ_PROP: LIA's bound-implied shared equality must
   leave the arithmetic child through Combined, carry its sparse reason, and reach EUF
   through normal trail re-assertion. Run only with the dark flag enabled. *)

open Oxsmt_core

module Combined =
  Oxsmt_combine.Combine.Combine (Oxsmt_combine.Uflia_router) (Oxsmt_euf.Euf_adapter)
    (Oxsmt_lia.Lia_adapter)

let checks = ref 0
let failures = ref 0

let check name condition =
  incr checks;
  if not condition
  then (
    incr failures;
    Printf.printf "  FAIL %s\n" name)
;;

let () =
  let env = Env.create () in
  let ctx = Context.create env in
  let x_sym = Env.declare_fun env "x" (Rank.create [] Sort.int) in
  let y_sym = Env.declare_fun env "y" (Rank.create [] Sort.int) in
  let f_sym = Env.declare_fun env "f" (Rank.create [ Sort.int ] Sort.int) in
  let x = Context.const ctx x_sym in
  let y = Context.const ctx y_sym in
  let fx = Context.app ctx f_sym [ x ] in
  let fy = Context.app ctx f_sym [ y ] in
  let equality = Context.eq ctx x y in
  let x_le_y = Context.le ctx x y in
  let y_le_x = Context.le ctx y x in
  let f_diseq = Context.eq ctx fx fy in
  let theory = Combined.create ctx env in
  let alloc = Atom.create_allocator () in
  let register term =
    let atom = Atom.fresh alloc in
    Combined.register_atom theory atom term;
    Lit.make atom true
  in
  let equality_lit = register equality in
  let x_le_y_lit = register x_le_y in
  let y_le_x_lit = register y_le_x in
  let f_diseq_lit = register f_diseq |> Lit.negate in
  Combined.assert_lit theory x_le_y_lit;
  Combined.assert_lit theory y_le_x_lit;
  Combined.assert_lit theory f_diseq_lit;
  let propagated =
    match Combined.check theory Theory.Propagate with
    | Theory.Propagations lits -> lits
    | Theory.Conflict _ | Theory.Sat | Theory.Split _ | Theory.Lemma _ -> []
  in
  check
    "LIA shared equality is forwarded through Combined"
    (List.exists (Lit.equal equality_lit) propagated);
  check
    "Combined routes the equality explanation back to LIA's two bounds"
    (List.sort_uniq Lit.compare (Combined.explain theory equality_lit).Explanation.premises
     = List.sort_uniq Lit.compare [ x_le_y_lit; y_le_x_lit ]);
  (* This is the SAT seam's normal next action for a theory propagation. EUF now merges
     x/y, congruence merges f(x)/f(y), and the asserted f-disequality conflicts. *)
  Combined.assert_lit theory equality_lit;
  check
    "re-asserting the propagated shared equality triggers EUF congruence conflict"
    (match Combined.check theory Theory.Propagate with
     | Theory.Conflict _ -> true
     | Theory.Sat | Theory.Propagations _ | Theory.Split _ | Theory.Lemma _ -> false);
  Printf.printf "lia_eq_prop_combine_test: %d checks, %d failures\n" !checks !failures;
  if !failures <> 0 then exit 1
;;
