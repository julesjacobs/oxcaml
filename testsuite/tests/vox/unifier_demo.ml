(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml unifier_spec.ml unifier_proofs.ml unifier.ml unifier_demo.ml";
 { bytecode; }
 { native; }
*)

open Unifier_spec
open Unifier_proofs

let run mode =
  let t = Pref.empty () in
  let var = Var in
  let allocated = Pref.alloc var t in
  let a = allocated.value in let t = allocated.state in
  let boolean = Bool in
  let allocated = Pref.alloc boolean t in
  let b = allocated.value in let t = allocated.state in
  let arrow = Arrow (a, a) in
  let allocated = Pref.alloc arrow t in
  let r = allocated.value in let t = allocated.state in
  let arrow = Arrow (b, b) in
  let allocated = Pref.alloc arrow t in
  let s = allocated.value in let t = allocated.state in
  let arrow = Arrow (b, r) in
  let allocated = Pref.alloc arrow t in
  let clash = allocated.value in let t = allocated.state in
  let link = Link a in
  let allocated = Pref.alloc link t in
  let alias = allocated.value in let t = allocated.state in
  let link = Link alias in
  let allocated = Pref.alloc link t in
  let alias2 = allocated.value in let t = allocated.state in
  let var = Var in
  let allocated = Pref.alloc var t in
  let other = allocated.value in let t = allocated.state in
  let h = ghost_ (Pref.own (borrow_ t)) in
  let scope : ((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h x) || scoped h x}) @ total ghost =
    ghost_ (fun x ->
      scoped_def h x;
      let u = () in u) in
  let p, q = match mode with
    | 0 -> r, s
    | 1 -> a, r
    | 2 -> r, clash
    | 3 -> alias2, b
    | 5 -> a, other
    | 6 -> b, r
    | 7 -> a, a
    | 8 -> alias2, clash
    | 9 -> a, s
    | _ -> r, r in
  let t : {t : Pref.token | Pref.own t === h && H.mem h p && H.mem h q} = t in
  let result = Unifier.unify h scope p q t in
  let ok = result.#ok in
  let d = ghost_ result.#derivation in
  let t = result.#state in
  let after = ghost_ (Pref.own (borrow_ t)) in
  let _proof = ghost_ (
    let u = () in
    unified_frame h p q ok after d a (u);
    unified_frame h p q ok after d r (u);
    let proof : {u : unit | H.mem after a && H.mem after r} = u in proof) in
  let t : {t : Pref.token | H.mem (Pref.own t) a} = t in
  let av = Pref.read a (borrow_ t) in
  let t : {t : Pref.token | H.mem (Pref.own t) r} = t in
  let rv = Pref.read r (borrow_ t) in
  assert (match rv with Arrow (x, y) -> x == a && y == a | _ -> false);
  match mode with
  | 0 | 3 ->
    assert ok;
    assert (match av with Link q -> q == b | _ -> false)
  | 1 -> assert (not ok); assert (match av with Var -> true | _ -> false)
  | 2 ->
    assert (not ok);
    assert (match av with Link q -> q == b | _ -> false)
  | 5 -> assert ok; assert (match av with Link q -> q == other | _ -> false)
  | 6 | 8 -> assert (not ok); assert (match av with Var -> true | _ -> false)
  | 9 -> assert ok; assert (match av with Link q -> q == s | _ -> false)
  | _ -> assert ok; assert (match av with Var -> true | _ -> false)

let () = List.iter run [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]
