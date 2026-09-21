open Unifier_spec
open Stlc_spec
open Stlc_graph_proofs

let rec generate : (h : Pref.heap) @ immutable ghost ->
    (env : {env : env | env_allocated h env}) @ immutable ->
    (e : {e : term | scoped_term (depth env) e}) @ immutable ->
    (t : {t : Pref.token | Pref.own t === h}) @ unique ->
    {r : generated | built h env r.#graph (Pref.own r.#state) && source r.#graph === e
      && root r.#graph === r.#value && constraints r.#graph === r.#equations
      && H.mem (Pref.own r.#state) r.#value
      && equations_allocated (Pref.own r.#state) r.#equations} @ unique =
  fun h env e t ->
    let n = ghost_ (depth env) in ghost_ (scoped_term_def n e);
    match e with
    | Bound i ->
      let value = lookup env i in
      ghost_ (let u = () in lookup_exists h env i (u));
      (match value with
      | None -> assert false
      | Some value ->
        let graph = ghost_ (GVar (i, value)) in
        let equations = Nothing in
        ghost_ (built_def h env graph h; source_def graph; root_def graph;
          constraints_def graph; equations_allocated_def h equations);
        let r = #{value; equations; state = t; graph} in r)
    | Boolean ->
      let v = Bool in let step = Pref.alloc v t in
      let value = step.value in let t = step.state in
      let after = ghost_ (Pref.own (borrow_ t)) in
      let graph = ghost_ (GBool value) in let equations = Nothing in
      ghost_ (built_def h env graph after; source_def graph; root_def graph;
        constraints_def graph; equations_allocated_def after equations);
      let r = #{value; equations; state = t; graph} in r
    | Lambda body ->
      let v = Var in let step = Pref.alloc v t in
      let arg = step.value in let t = step.state in
      let h1 = ghost_ (Pref.own (borrow_ t)) in
      ghost_ (allocation_mem h arg v);
      let env1 = Bind (arg, env) in
      ghost_ (
        let keep : (x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || H.mem h1 x}
            @ total = fun x -> let u = () in u in
        let u = () in env_frame h h1 keep env (u);
        env_allocated_def h1 env1; depth_def env1;
        let proof : {u : unit | env_allocated h1 env1 && scoped_term (depth env1) body} = u in proof);
      let env1 : {v : env | env_allocated h1 v} = env1 in
      let body : {b : term | scoped_term (depth env1) b} = body in
      let t : {t : Pref.token | Pref.own t === h1} = t in
      let child = generate h1 env1 body t in
      let middle = ghost_ (Pref.own (borrow_ child.#state)) in
      let v = Arrow (arg, child.#value) in
      let step = Pref.alloc v child.#state in
      let value = step.value in let t = step.state in let after = ghost_ (Pref.own (borrow_ t)) in
      let graph = ghost_ (GLam (arg, child.#graph, value, middle)) in
      let equations = child.#equations in
      ghost_ (built_def h env graph after; source_def graph; root_def graph; constraints_def graph;
        let u = () in built_frame h env graph after value (u);
        built_equations h env graph after (u));
      let r = #{value; equations; state = t; graph} in r
    | Recursive body ->
      let v = Var in let step = Pref.alloc v t in
      let arg = step.value in let t = step.state in
      let h1 = ghost_ (Pref.own (borrow_ t)) in
      ghost_ (allocation_mem h arg v);
      let step = Pref.alloc v t in
      let result = step.value in let t = step.state in
      let h2 = ghost_ (Pref.own (borrow_ t)) in
      ghost_ (allocation_mem h1 result v);
      let arrow = Arrow (arg, result) in let step = Pref.alloc arrow t in
      let value = step.value in let t = step.state in
      let h3 = ghost_ (Pref.own (borrow_ t)) in
      ghost_ (allocation_mem h2 value arrow;
        allocation_keeps_mem h1 result v arg; allocation_keeps_mem h2 value arrow arg);
      let rest = Bind (value, env) in let env1 = Bind (arg, rest) in
      ghost_ (
        let keep : (x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || H.mem h3 x}
            @ total = fun x -> let u = () in u in
        let u = () in env_frame h h3 keep env (u);
        env_allocated_def h3 rest; env_allocated_def h3 env1;
        depth_def rest; depth_def env1;

        let proof : {u : unit | env_allocated h3 env1 && scoped_term (depth env1) body} = u in proof);
      let env1 : {v : env | env_allocated h3 v} = env1 in
      let body : {b : term | scoped_term (depth env1) b} = body in
      let t : {t : Pref.token | Pref.own t === h3} = t in
      let child = generate h3 env1 body t in
      let after = ghost_ (Pref.own (borrow_ child.#state)) in
      let graph = ghost_ (GRec (arg, result, value, child.#graph)) in
      let equations = And (child.#equations, Equal (child.#value, result)) in
      ghost_ (built_def h env graph after; source_def graph; root_def graph; constraints_def graph;
        let u = () in built_frame h env graph after value (u);
        built_equations h env graph after (u));
      let r = #{value; equations; state = child.#state; graph} in r
    | Apply (f, a) ->
      let env : {v : env | env_allocated h v} = env in
      let f : {f : term | scoped_term (depth env) f} = f in
      let t : {t : Pref.token | Pref.own t === h} = t in
      let left = generate h env f t in
      let h1 = ghost_ (Pref.own (borrow_ left.#state)) in
      ghost_ (
        let keep : (x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || H.mem h1 x}
            @ total = fun x -> let u = () in
          built_frame h env left.#graph h1 x (u); u in
        let u = () in env_frame h h1 keep env (u);
        let proof : {u : unit | env_allocated h1 env} = u in proof);
      let env1 : {v : env | env_allocated h1 v} = env in
      let a : {a : term | scoped_term (depth env1) a} = a in
      let t = left.#state in
      let t : {t : Pref.token | Pref.own t === h1} = t in
      let right = generate h1 env1 a t in
      let h2 = ghost_ (Pref.own (borrow_ right.#state)) in
      let v = Var in let step = Pref.alloc v right.#state in
      let value = step.value in let t = step.state in
      let v = Arrow (right.#value, value) in let step = Pref.alloc v t in
      let arrow = step.value in let t = step.state in let after = ghost_ (Pref.own (borrow_ t)) in
      let graph = ghost_ (GApp (left.#graph, right.#graph, value, arrow, h1, h2)) in
      let equations = And (And (left.#equations, right.#equations), Equal (left.#value, arrow)) in
      ghost_ (built_def h env graph after; source_def graph; root_def graph; constraints_def graph;
        let u = () in built_frame h env graph after value (u);
        built_equations h env graph after (u));
      let r = #{value; equations; state = t; graph} in r
