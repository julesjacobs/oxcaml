(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml unifier_spec.ml unifier_proofs.ml unifier.ml unifier_finite_spec.ml unifier_finite_proofs.ml unifier_mgu_spec.ml unifier_mgu_proofs.ml unifier_finite_demo.ml";
 { bytecode; }
 { native; }
*)

open Unifier_spec
open Unifier_proofs
open Unifier_finite_spec
open Unifier_finite_proofs
open Unifier_mgu_spec
open Unifier_mgu_proofs

let run mode =
  let t = Pref.empty () in
  let h0 = ghost_ (Pref.own (borrow_ t)) in
  let trees0 : ((x : node Pref.t) @ immutable ->
      {t : tree | root t === x &&
        (if H.mem h0 x then finite h0 t else H.at h0 x === None)}
      @ immutable) @ total ghost = ghost_ (fun x ->
    let t = Free x in root_def t; t) in
  let var = Var in
  let allocated = Pref.alloc var t in
  let a = allocated.value in let t = allocated.state in
  let h1 = ghost_ (Pref.own (borrow_ t)) in
  let trees1 : ((x : node Pref.t) @ immutable ->
      {t : tree | root t === x &&
        (if H.mem h1 x then finite h1 t else H.at h1 x === None)}
      @ immutable) @ total ghost = ghost_ (fun x ->
    allocatable_def h0 var;
    let u = () in
    let tree = allocation_finite_at h0 trees0 a var x (u) in
    tree) in
  let boolean = Bool in
  let allocated = Pref.alloc boolean t in
  let b = allocated.value in let t = allocated.state in
  let h2 = ghost_ (Pref.own (borrow_ t)) in
  let trees2 : ((x : node Pref.t) @ immutable ->
      {t : tree | root t === x &&
        (if H.mem h2 x then finite h2 t else H.at h2 x === None)}
      @ immutable) @ total ghost = ghost_ (fun x ->
    allocatable_def h1 boolean;
    let u = () in
    let tree = allocation_finite_at h1 trees1 b boolean x (u) in
    tree) in
  let arrow = Arrow (a, a) in
  let allocated = Pref.alloc arrow t in
  let r = allocated.value in let t = allocated.state in
  let h3 = ghost_ (Pref.own (borrow_ t)) in
  let trees3 : ((x : node Pref.t) @ immutable ->
      {t : tree | root t === x &&
        (if H.mem h3 x then finite h3 t else H.at h3 x === None)}
      @ immutable) @ total ghost = ghost_ (fun x ->
    allocatable_def h2 arrow;
    let u = () in
    let tree = allocation_finite_at h2 trees2 r arrow x (u) in
    tree) in
  let arrow = Arrow (b, b) in
  let allocated = Pref.alloc arrow t in
  let s = allocated.value in let t = allocated.state in
  let h4 = ghost_ (Pref.own (borrow_ t)) in
  let trees4 : ((x : node Pref.t) @ immutable ->
      {t : tree | root t === x &&
        (if H.mem h4 x then finite h4 t else H.at h4 x === None)}
      @ immutable) @ total ghost = ghost_ (fun x ->
    allocatable_def h3 arrow;
    let u = () in
    let tree = allocation_finite_at h3 trees3 s arrow x (u) in
    tree) in
  let arrow = Arrow (b, r) in
  let allocated = Pref.alloc arrow t in
  let clash = allocated.value in let t = allocated.state in
  let h5 = ghost_ (Pref.own (borrow_ t)) in
  let trees5 : ((x : node Pref.t) @ immutable ->
      {t : tree | root t === x &&
        (if H.mem h5 x then finite h5 t else H.at h5 x === None)}
      @ immutable) @ total ghost = ghost_ (fun x ->
    allocatable_def h4 arrow;
    let u = () in
    let tree = allocation_finite_at h4 trees4 clash arrow x (u) in
    tree) in
  let link = Link a in
  let allocated = Pref.alloc link t in
  let alias = allocated.value in let t = allocated.state in
  let h6 = ghost_ (Pref.own (borrow_ t)) in
  let trees6 : ((x : node Pref.t) @ immutable ->
      {t : tree | root t === x &&
        (if H.mem h6 x then finite h6 t else H.at h6 x === None)}
      @ immutable) @ total ghost = ghost_ (fun x ->
    allocatable_def h5 link;
    let u = () in
    let tree = allocation_finite_at h5 trees5 alias link x (u) in
    tree) in
  let link = Link alias in
  let allocated = Pref.alloc link t in
  let alias2 = allocated.value in let t = allocated.state in
  let h7 = ghost_ (Pref.own (borrow_ t)) in
  let trees7 : ((x : node Pref.t) @ immutable ->
      {t : tree | root t === x &&
        (if H.mem h7 x then finite h7 t else H.at h7 x === None)}
      @ immutable) @ total ghost = ghost_ (fun x ->
    allocatable_def h6 link;
    let u = () in
    let tree = allocation_finite_at h6 trees6 alias2 link x (u) in
    tree) in
  let var = Var in
  let allocated = Pref.alloc var t in
  let other = allocated.value in let t = allocated.state in
  let h8 = ghost_ (Pref.own (borrow_ t)) in
  let trees8 : ((x : node Pref.t) @ immutable ->
      {t : tree | root t === x &&
        (if H.mem h8 x then finite h8 t else H.at h8 x === None)}
      @ immutable) @ total ghost = ghost_ (fun x ->
    allocatable_def h7 var;
    let u = () in
    let tree = allocation_finite_at h7 trees7 other var x (u) in
    tree) in
  let h = ghost_ (Pref.own (borrow_ t)) in
  let trees : ((x : node Pref.t) @ immutable ->
      {t : tree | root t === x &&
        (if H.mem h x then finite h t else H.at h x === None)}
      @ immutable) @ total ghost = ghost_ (trees8) in
  let scope : ((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h x) || scoped h x}) @ total ghost =
    ghost_ (fun x ->
      let t = trees x in let u = () in
      if H.mem h x then (
        finite_scope_at h t (u); u)
      else u) in
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
  let _finite_proof = ghost_ (
    let after_trees : (x : node Pref.t) @ immutable ->
        {t : tree | root t === x &&
          (if H.mem after x then finite after t else H.at after x === None)}
        @ immutable total = fun x ->
      let u = () in
      let t = unified_finite_at h trees p q ok after d x (u) in
      t in
    let claim = true in
    let use : ((rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (model : ((x : node Pref.t) @ immutable -> {u : unit | equation after rho x}))
        @ total -> {u : unit | claim}) @ total = fun rho model ->
      model a; model r;
      let u = () in
      if ok then (
        success_forward_at h rho p q after d model r (u); u)
      else u in
    let u = with_finite_model after after_trees claim use in u) in
  let _mgu_proof = ghost_ (
    if ok then (
      let claim = true in
      let use : ((sigma : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
          (solution : ((x : node Pref.t) @ immutable ->
            {u : unit | equation h sigma x && sigma p === sigma q
              && sigma x === substitute sigma (sigma x)
              && (H.mem h x || sigma x === TVar x)})) @ total ->
          (factor : ((rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
            (model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x})) @ total ->
            (x : node Pref.t) @ immutable -> {u : unit | rho p === rho q} ->
            {u : unit | rho x === substitute rho (sigma x)})) @ total ->
          {u : unit | claim}) @ total = fun sigma solution factor ->
        solution a; solution r; solution alias2;
        let model : (x : node Pref.t) @ immutable -> {u : unit | equation h sigma x}
            @ total = fun x -> solution x; let u = () in u in
        let[@def] delta : node Pref.t @ immutable total -> ty @ immutable total =
          fun x -> TArrow (TVar x, TBool) in
        let[@def] rho : node Pref.t @ immutable total -> ty @ immutable total =
          fun x -> substitute delta (sigma x) in
        let instance : (x : node Pref.t) @ immutable ->
            {u : unit | rho x === substitute delta (sigma x)} @ total = fun x ->
          rho_def x; let u = () in u in
        let old_model : (x : node Pref.t) @ immutable -> {u : unit | equation h rho x}
            @ total = fun x ->
          solution x; let u = () in
          instance_solution_at h sigma model delta rho instance p q x (u);
          u in
        let u = () in
        instance_solution_at h sigma model delta rho instance p q a (u);
        factor rho old_model a (u);
        factor rho old_model r (u);
        u in
      let u = () in let u = with_mgu h trees p q after d (u) claim use in u)
    else ()) in
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
