open Unifier_spec
open Stlc_spec
open Stlc_graph_proofs

let rec generate : (h : node Pref.heap) @ immutable ghost ->
    (env : {env : env | env_allocated h env}) @ immutable ->
    (e : {e : term | let refine_ env = env in scoped_term (depth env) e}) @ immutable ->
    (t : {t : node Pref.token | Pref.own t === h}) @ unique ->
    {r : generated | let refine_ env = env in let refine_ e = e in
      built h env r.#graph (Pref.own r.#state) && source r.#graph === e
      && root r.#graph === r.#value && constraints r.#graph === r.#equations
      && H.mem (Pref.own r.#state) r.#value
      && equations_allocated (Pref.own r.#state) r.#equations} @ unique =
  fun h env e t ->
    let refine_ env = env in let refine_ e = e in let refine_ t = t in
    let n = ghost_ (depth env) in ghost_ (scoped_term_def n e);
    match e with
    | Bound i ->
      let value = lookup env i in
      ghost_ (let u = () in lookup_exists h env i (refine_ u));
      (match value with
      | None -> assert false
      | Some value ->
        let graph = ghost_ (GVar (i, value)) in
        let equations = Nothing in
        ghost_ (built_def h env graph h; source_def graph; root_def graph;
          constraints_def graph; equations_allocated_def h equations);
        let r = #{value; equations; state = t; graph} in refine_ r)
    | Boolean ->
      let v = Bool in let refine_ step = Pref.alloc v t in
      let value = step.value in let t = step.state in
      let after = ghost_ (Pref.own (borrow_ t)) in
      let graph = ghost_ (GBool value) in let equations = Nothing in
      ghost_ (built_def h env graph after; source_def graph; root_def graph;
        constraints_def graph; equations_allocated_def after equations);
      let r = #{value; equations; state = t; graph} in refine_ r
    | Lambda body ->
      let v = Var in let refine_ step = Pref.alloc v t in
      let arg = step.value in let t = step.state in
      let h1 = ghost_ (Pref.own (borrow_ t)) in
      ghost_ (allocation_mem h arg v);
      let env1 = Bind (arg, env) in
      ghost_ (
        let keep : (x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || H.mem h1 x}
            @ total = fun x -> let u = () in refine_ u in
        let u = () in env_frame h h1 keep env (refine_ u);
        env_allocated_def h1 env1; depth_def env1;
        let proof : {u : unit | env_allocated h1 env1 && scoped_term (depth env1) body} = refine_ u in proof);
      let env1 : {v : env | env_allocated h1 v} = refine_ env1 in
      let body : {b : term | let refine_ env1 = env1 in scoped_term (depth env1) b} = refine_ body in
      let t : {t : node Pref.token | Pref.own t === h1} = refine_ t in
      let refine_ child = generate h1 env1 body t in
      let refine_ env1 = env1 in let refine_ body = body in
      let middle = ghost_ (Pref.own (borrow_ child.#state)) in
      let v = Arrow (arg, child.#value) in
      let refine_ step = Pref.alloc v child.#state in
      let value = step.value in let t = step.state in let after = ghost_ (Pref.own (borrow_ t)) in
      let graph = ghost_ (GLam (arg, child.#graph, value, middle)) in
      let equations = child.#equations in
      ghost_ (built_def h env graph after; source_def graph; root_def graph; constraints_def graph;
        let u = () in built_frame h env graph after value (refine_ u);
        built_equations h env graph after (refine_ u));
      let r = #{value; equations; state = t; graph} in refine_ r
    | Apply (f, a) ->
      let env : {v : env | env_allocated h v} = refine_ env in
      let f : {f : term | let refine_ env = env in scoped_term (depth env) f} = refine_ f in
      let t : {t : node Pref.token | Pref.own t === h} = refine_ t in
      let refine_ left = generate h env f t in
      let refine_ env = env in let refine_ f = f in
      let h1 = ghost_ (Pref.own (borrow_ left.#state)) in
      ghost_ (
        let keep : (x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || H.mem h1 x}
            @ total = fun x -> let u = () in
          built_frame h env left.#graph h1 x (refine_ u); refine_ u in
        let u = () in env_frame h h1 keep env (refine_ u);
        let proof : {u : unit | env_allocated h1 env} = refine_ u in proof);
      let env1 : {v : env | env_allocated h1 v} = refine_ env in
      let a : {a : term | let refine_ env1 = env1 in scoped_term (depth env1) a} = refine_ a in
      let t = left.#state in
      let t : {t : node Pref.token | Pref.own t === h1} = refine_ t in
      let refine_ right = generate h1 env1 a t in
      let refine_ a = a in
      let h2 = ghost_ (Pref.own (borrow_ right.#state)) in
      let v = Var in let refine_ step = Pref.alloc v right.#state in
      let value = step.value in let t = step.state in
      let v = Arrow (right.#value, value) in let refine_ step = Pref.alloc v t in
      let arrow = step.value in let t = step.state in let after = ghost_ (Pref.own (borrow_ t)) in
      let graph = ghost_ (GApp (left.#graph, right.#graph, value, arrow, h1, h2)) in
      let equations = And (And (left.#equations, right.#equations), Equal (left.#value, arrow)) in
      ghost_ (built_def h env graph after; source_def graph; root_def graph; constraints_def graph;
        let u = () in built_frame h env graph after value (refine_ u);
        built_equations h env graph after (refine_ u));
      let r = #{value; equations; state = t; graph} in refine_ r
