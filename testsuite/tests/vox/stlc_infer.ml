open Unifier_spec
open Unifier_finite_spec
open Unifier_finite_proofs
open Stlc_spec
open Stlc_graph_proofs
open Stlc_solve_proofs
open Stlc_inference_proofs

let infer : (e : {e : term | scoped_term Z e}) @ immutable ->
    {r : inference | let refine_ e = e in inferred e r.#generated_heap r.#graph r.#ok (Pref.own r.#state) r.#solving
      && r.#value === root r.#graph && H.mem (Pref.own r.#state) r.#value
      && finite (Pref.own r.#state) r.#tree && Unifier_finite_spec.root r.#tree === r.#value} @ unique = fun e ->
  let refine_ e = e in
  let refine_ t = Pref.empty () in let h = ghost_ (Pref.own (borrow_ t)) in
  let env = Empty in ghost_ (env_allocated_def h env; depth_def env);
  let env : {env : env | env_allocated h env} = refine_ env in
  let e : {e : term | let refine_ env = env in scoped_term (depth env) e} = refine_ e in
  let t : {t : Pref.token | Pref.own t === h} = refine_ t in
  let refine_ generated = Stlc_generate.generate h env e t in
  let refine_ env = env in let refine_ e = e in
  let middle = ghost_ (Pref.own (borrow_ generated.#state)) in
  let graph = ghost_ generated.#graph in let value = generated.#value in
  let trees : ((x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
      (if H.mem middle x then finite middle t else H.at middle x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
    let start : (x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
        (if H.mem h x then finite h t else H.at h x === None)} @ immutable total = fun x ->
      let refine_ t = empty_tree x in refine_ t in
    let u = () in let refine_ t = built_finite_at h start env graph middle x (refine_ u) in refine_ t) in
  let eqs = generated.#equations in
  let eqs : {eqs : equations | equations_allocated middle eqs} = refine_ eqs in
  let t = generated.#state in let t : {t : Pref.token | Pref.own t === middle} = refine_ t in
  let refine_ solved = Stlc_solve.solve middle trees eqs t in let refine_ eqs = eqs in
  let ok = solved.#ok in let solving = ghost_ solved.#solving in
  let t = solved.#state in let after = ghost_ (Pref.own (borrow_ t)) in
  ghost_ (inferred_def e middle graph ok after solving);
  ghost_ (let u = () in solved_frame middle eqs ok after solving value (refine_ u));
  let refine_ tree = ghost_ (let u = () in inference_finite_at e middle graph ok after solving value (refine_ u)) in
  let r = #{value; ok; state = t; graph; generated_heap = middle; solving; tree} in refine_ r
