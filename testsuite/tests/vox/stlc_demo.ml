(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml unifier_spec.ml unifier_proofs.ml unifier.ml unifier_finite_spec.ml unifier_finite_proofs.ml unifier_mgu_spec.ml unifier_mgu_proofs.ml stlc_spec.ml stlc_graph_proofs.ml stlc_model_proofs.ml stlc_complete_proofs.ml stlc_solve_proofs.ml stlc_inference_proofs.ml stlc_generate.ml stlc_solve.ml stlc_infer.ml stlc_demo.ml";
 { bytecode; }
 { native; }
*)
open Unifier_spec
open Unifier_finite_spec
open Stlc_spec
open Stlc_inference_proofs

let run e expected =
  if scoped_term Z e then (
    let e : {e : term | scoped_term Z e} = refine_ e in
    let refine_ result = Stlc_infer.infer e in let refine_ e = e in
    let ok = result.#ok in assert (ok = expected);
    let h = ghost_ (Pref.own (borrow_ result.#state)) in
    let _proof = ghost_ (
      if ok then (
        let u = () in
        let refine_ typing = inference_sound e result.#generated_heap result.#graph h result.#solving result.#tree (refine_ u) in
        ()) else ()) in
    let p = result.#value in let t = result.#state in
    let t : {t : Pref.token | H.mem (Pref.own t) p} = refine_ t in
    let refine_ _payload = Pref.read p (borrow_ t) in ())
  else assert false

let identity_instance () =
  let e = Lambda (Bound Z) in
  ghost_ (let b = Bound Z in let zero = Z in let one = S zero in
    scoped_term_def one b; present_def one zero; scoped_term_def zero e;
    let u = () in let proof : {u : unit | scoped_term Z e} = refine_ u in proof);
  let e : {e : term | scoped_term Z e} = refine_ e in
  let refine_ result = Stlc_infer.infer e in let refine_ e = e in
  assert result.#ok;
  let _proof = ghost_ (
    let after = Pref.own (borrow_ result.#state) in
    if result.#ok then (
      let target = TArrow (TBool, TBool) in
      let d = Abstraction (TBool, Variable) in
      let ctx = Type (TBool, No_types) in let b = Bound Z in
      let zero = Z in let boolean = TBool in let variable = Variable in let empty = No_types in
      lookup_type_def ctx zero; typed_def ctx b boolean variable;
      typed_def empty e target d;
      let claim = true in
      let use : ((delta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
          {u : unit | target === Unifier_mgu_spec.substitute delta (readback result.#tree)} ->
          {u : unit | claim}) @ total = fun _delta factor ->
        let refine_ factor = factor in let u = () in refine_ u in
      let u = () in
      let refine_ u = with_typing_factor e result.#generated_heap result.#graph after result.#solving
        result.#tree target d (refine_ u) claim use in ()) else ()) in
  ()

let recursive_instance () =
  let e = Recursive (Bound Z) in
  if scoped_term Z e then (
    let e : {e : term | scoped_term Z e} = refine_ e in
    let refine_ result = Stlc_infer.infer e in let refine_ e = e in
    assert result.#ok;
    let _proof = ghost_ (
      let after = Pref.own (borrow_ result.#state) in
      if result.#ok then (
        let boolean = TBool in let target = TArrow (boolean, boolean) in
        let variable = Variable in let d = Recursion (boolean, boolean, variable) in
        let ctx = Type (boolean, Type (target, No_types)) in
        let body = Bound Z in let zero = Z in let empty = No_types in
        lookup_type_def ctx zero; typed_def ctx body boolean variable;
        typed_def empty e target d;
        let claim = true in
        let use : ((delta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
            {u : unit | target === Unifier_mgu_spec.substitute delta (readback result.#tree)} ->
            {u : unit | claim}) @ total = fun _delta factor ->
          let refine_ factor = factor in let u = () in refine_ u in
        let u = () in
        let refine_ u = with_typing_factor e result.#generated_heap result.#graph after result.#solving
          result.#tree target d (refine_ u) claim use in ()) else ()) in
    ()) else assert false

let () =
  run Boolean true;
  run (Lambda (Bound Z)) true;
  run (Lambda (Lambda (Bound (S Z)))) true;
  run (Lambda (Lambda (Apply (Bound (S Z), Bound Z)))) true;
  run (Apply (Lambda (Bound Z), Boolean)) true;
  run (Apply (Lambda (Lambda (Bound (S Z))), Boolean)) true;
  run (Lambda (Apply (Bound Z, Bound Z))) false;
  run (Apply (Boolean, Boolean)) false;
  run (Apply (Lambda (Apply (Bound Z, Boolean)), Boolean)) false;
  run (Recursive (Bound Z)) true;
  run (Recursive (Apply (Bound (S Z), Bound Z))) true;
  run (Apply (Recursive (Bound Z), Boolean)) true;
  run (Lambda (Recursive (Bound (S (S Z))))) true;
  run (Recursive (Bound (S Z))) false;
  run (Recursive (Apply (Bound Z, Bound Z))) false;
  run (Recursive (Apply (Lambda (Apply (Bound (S (S Z)), Lambda (Bound Z))),
    Apply (Bound (S Z), Boolean)))) false;
  identity_instance ();
  recursive_instance ()
