open Unifier_spec
open Unifier_finite_spec
open Unifier_finite_proofs
open Unifier_mgu_proofs
open Stlc_spec
open Stlc_graph_proofs
open Stlc_solve_proofs
open Stlc_complete_proofs

let (empty_tree @ total) : (x : node Pref.t) @ immutable ->
    {t : tree | Unifier_finite_spec.root t === x &&
      (if H.mem (H.empty ()) x then finite (H.empty ()) t else H.at (H.empty ()) x === None)}
    @ immutable ghost = fun x -> ghost_ (
  let t = Free x in Unifier_finite_spec.root_def t; refine_ t)

let (inference_finite_at @ total) : (e : term) @ immutable -> (middle : Pref.heap) @ immutable ->
    (g : graph) @ immutable -> (ok : bool) -> (after : Pref.heap) @ immutable -> (d : solving) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | inferred e middle g ok after d} ->
    {t : tree | Unifier_finite_spec.root t === x &&
      (if H.mem after x then finite after t else H.at after x === None)} @ immutable ghost =
  fun e middle g ok after d x premise -> ghost_ (
    let refine_ premise = premise in inferred_def e middle g ok after d;
    let h = H.empty () in let env = Empty in
    let start : (x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
        (if H.mem h x then finite h t else H.at h x === None)} @ immutable total = fun x ->
      let refine_ t = empty_tree x in refine_ t in
    let generated : (x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
        (if H.mem middle x then finite middle t else H.at middle x === None)} @ immutable total = fun x ->
      let u = () in let refine_ t = built_finite_at h start env g middle x (refine_ u) in refine_ t in
    let cs = constraints g in let u = () in
    solved_finite_at middle generated cs ok after d x (refine_ u))

let (inference_sound @ total) : (e : term) @ immutable -> (middle : Pref.heap) @ immutable ->
    (g : graph) @ immutable -> (after : Pref.heap) @ immutable -> (d : solving) @ immutable ->
    (t : tree) @ immutable ->
    {u : unit | inferred e middle g true after d && finite after t && Unifier_finite_spec.root t === root g} ->
    {proof : typing | typed No_types e (readback t) proof} @ immutable ghost =
  fun e middle g after d t premise -> ghost_ (
    let refine_ premise = premise in inferred_def e middle g true after d;
    let trees : (x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
        (if H.mem after x then finite after t else H.at after x === None)} @ immutable total = fun x ->
      let u = () in let refine_ t = inference_finite_at e middle g true after d x (refine_ u) in refine_ t in
    let[@def] rho : node Pref.t @ immutable total -> ty @ immutable total = fun x ->
      let refine_ t = trees x in readback t in
    let agrees : (x : node Pref.t) @ immutable ->
        {u : unit | let refine_ t = trees x in not (H.mem after x) || rho x === readback t}
        @ total = fun x -> rho_def x; let u = () in refine_ u in
    let model : (x : node Pref.t) @ immutable -> {u : unit | equation after rho x}
        @ total = fun x -> let refine_ u = readback_model_at after trees rho agrees x in refine_ u in
    let cs = constraints g in
    let generated_model : (x : node Pref.t) @ immutable ->
        {u : unit | not (H.mem middle x) || equation middle rho x} @ total = fun x ->
      let u = () in solved_forward_at middle rho cs after d model x (refine_ u); refine_ u in
    let r = root g in let u = () in
    solved_forward_at middle rho cs after d model r (refine_ u);
    let h = H.empty () in let env = Empty in
    generation_sound h env g middle rho generated_model (refine_ u);
    context_of_def rho env; finite_def after t;
    let refine_ actual = trees r in finite_unique after actual t (refine_ u); rho_def r;
    let proof = derive rho g in refine_ proof)

let (with_typing_factor @ total) : (e : term) @ immutable -> (middle : Pref.heap) @ immutable ->
    (g : graph) @ immutable -> (after : Pref.heap) @ immutable -> (d : solving) @ immutable ->
    (t : tree) @ immutable -> (target : ty) @ immutable -> (typing : typing) @ immutable ->
    {u : unit | inferred e middle g true after d && finite after t && Unifier_finite_spec.root t === root g
      && typed No_types e target typing} -> (claim : bool) ->
    (use : ((delta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | target === Unifier_mgu_spec.substitute delta (readback t)} -> {u : unit | claim})) @ total ->
    {u : unit | claim} @ ghost = fun e middle g after d t target typing premise claim use -> ghost_ (
  let refine_ premise = premise in inferred_def e middle g true after d;
  let h = H.empty () in let env = Empty in env_allocated_def h env;
  let trees : (x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
      (if H.mem h x then finite h t else H.at h x === None)} @ immutable total = fun x ->
    let refine_ t = empty_tree x in refine_ t in
  let rho : node Pref.t @ immutable total -> ty @ immutable total = fun x -> TVar x in
  let model : (x : node Pref.t) @ immutable -> {u : unit | equation h rho x}
      @ total = fun x -> equation_def h rho x; let u = () in refine_ u in
  context_of_def rho env;
  let consume : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (generated_model : ((x : node Pref.t) @ immutable -> {u : unit | equation middle tau x})) @ total ->
      (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
      {u : unit | satisfies tau (constraints g) && tau (root g) === target} -> {u : unit | claim}) @ total =
    fun tau generated_model _equal fit ->
      let refine_ fit = fit in let cs = constraints g in
      let final_model : (x : node Pref.t) @ immutable -> {u : unit | equation after tau x}
          @ total = fun x -> let u = () in
        let refine_ u = solved_backward_at middle tau generated_model cs after d x (refine_ u) in refine_ u in
      let u = () in readback_factor after tau final_model t (refine_ u);
      let refine_ u = use tau (refine_ u) in refine_ u in
  let u = () in let refine_ u = with_generation_model h trees env g middle rho model target typing
    (refine_ u) claim consume in refine_ u)

let (inference_rejects @ total) : (e : term) @ immutable -> (middle : Pref.heap) @ immutable ->
    (g : graph) @ immutable -> (after : Pref.heap) @ immutable -> (d : solving) @ immutable ->
    (target : ty) @ immutable -> (typing : typing) @ immutable ->
    {u : unit | inferred e middle g false after d && typed No_types e target typing} ->
    {u : unit | false} @ ghost = fun e middle g after d target typing premise -> ghost_ (
  let refine_ premise = premise in inferred_def e middle g false after d;
  let h = H.empty () in let env = Empty in env_allocated_def h env;
  let trees : (x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
      (if H.mem h x then finite h t else H.at h x === None)} @ immutable total = fun x ->
    let refine_ t = empty_tree x in refine_ t in
  let rho : node Pref.t @ immutable total -> ty @ immutable total = fun x -> TVar x in
  let model : (x : node Pref.t) @ immutable -> {u : unit | equation h rho x}
      @ total = fun x -> equation_def h rho x; let u = () in refine_ u in
  context_of_def rho env; let claim = false in
  let consume : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (generated_model : ((x : node Pref.t) @ immutable -> {u : unit | equation middle tau x})) @ total ->
      (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
      {u : unit | satisfies tau (constraints g) && tau (root g) === target} -> {u : unit | claim}) @ total =
    fun tau generated_model _equal fit ->
      let refine_ fit = fit in let cs = constraints g in let u = () in
      let refine_ u = solved_refutes middle tau generated_model cs after d (refine_ u) in refine_ u in
  let u = () in let refine_ u = with_generation_model h trees env g middle rho model target typing
    (refine_ u) claim consume in refine_ u)
