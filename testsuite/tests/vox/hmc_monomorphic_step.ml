module D = Hm_declarative
module C = Hmc_monomorphic
module M = Hmc_manifest
module T = Hmc_templates
module I = Hmc_instance
module B = Hmc_specialized_body
module L = Hmc_monomorphic_links
module E = Hmc_catalog_semantics
module W = Hmc_monomorphic_values
module S = Hmc_source_semantics
module Q = Hmc_monomorphic_semantics
module H = Hmc_monomorphic_states
open H

let (evaluate @ total) : (p : C.program) @ immutable ->
    (definitions : {d : C.definitions | C.origins d}) @ immutable ->
    (catalog : T.catalog) @ immutable -> (env : W.t) @ immutable -> (term : C.term) @ immutable ->
    (k : H.continuation) @ immutable ->
    {u : unit | p.C.definitions === definitions && H.valid (C.manifest definitions) (Running (Evaluate (catalog, env, term), k))} ->
    {out : H.state | H.valid (C.manifest definitions) out
      && H.source out === S.step (H.source (Running (Evaluate (catalog, env, term), k)))
      && H.target out === Q.step definitions (H.target (Running (Evaluate (catalog, env, term), k)))} @ immutable =
  fun p definitions catalog env term k premise ->
  let state = Running (Evaluate (catalog, env, term), k) in
  ghost_ (C.ready_def p);
  let table = C.manifest definitions in
  ghost_ (H.source_def state; H.target_def state; H.valid_def table state;
    let control = Evaluate (catalog, env, term) in
    H.source_control_def control; H.target_control_def control; H.control_valid_def table control;
    H.environment_valid_def table env; H.source_environment_def catalog env;
    C.erase_def term;
    S.step_def (H.source state); Q.step_def definitions (H.target state));
  let out = ghost_ (L.linked_def table catalog (W.length env) term);
    (match term with
    | C.Local index ->
      let v = W.local table env (E.environment catalog) index () in
      Running (Return v, k)
    | C.Global (i, j, id) ->
      (match C.lookup definitions id with
      | None -> unreachable_ ()
      | Some d ->
        let origin = d.C.body.B.origin in
        ghost_ (
          let entry = {M.body = d.C.body; dependencies = C.links d.C.code} in
          M.lookup_closed table id entry ();
          L.from_records table origin.I.earlier D.Empty_context d.C.code d.C.body.B.derivation ();
          L.depth_def D.Empty_context; L.linked_def table origin.I.earlier D.Z d.C.code;
          I.valid_def origin; T.definition_valid_def origin.I.earlier origin.I.definition;
          Hmc_admission.callable_def origin.I.definition.T.source; C.erase_def d.C.code;
          W.global env (E.environment catalog) i j ();
          E.lookup catalog j {T.definition = origin.I.definition; earlier = origin.I.earlier} ();
          E.close_def (E.environment origin.I.earlier) origin.I.definition.T.source;
          W.source_def (W.Empty);
            W.target_def (W.Empty);
            W.valid_def table (W.Empty);
            W.environment_def (W.Empty);
            W.length_def (W.Empty);
            E.append_def (W.source W.Empty) (E.environment origin.I.earlier));
        (match d.C.code with
        | C.Lambda body -> Running (Return (W.Closure (origin.I.earlier, body, W.Empty)), k)
        | C.Recursive body -> Running (Return (W.Recursive_closure (origin.I.earlier, body, W.Empty)), k)
        | _ -> unreachable_ ()))
    | C.Truth -> Running (Return W.True, k)
    | C.False -> Running (Return W.False, k)
    | C.Word w -> Running (Return (W.Word w), k)
    | C.Nil -> Running (Return W.Nil, k)
    | C.Lambda body -> Running (Return (W.Closure (catalog, body, env)), k)
    | C.Recursive body -> Running (Return (W.Recursive_closure (catalog, body, env)), k)
    | C.Apply (f, a) -> Running (Evaluate (catalog, env, f), Apply_function (catalog, env, a, k))
    | C.Let (rhs, body) -> Running (Evaluate (catalog, env, rhs), Let_body (catalog, env, body, k))
    | C.Cons (head, tail) -> Running (Evaluate (catalog, env, head), Cons_head (catalog, env, tail, k))
    | C.CaseList (s, l, r) -> Running (Evaluate (catalog, env, s), List_cases (catalog, env, l, r, k))
    | C.If (c, a, b) -> Running (Evaluate (catalog, env, c), Conditional (catalog, env, a, b, k))
    | C.Primitive (op, a, b) -> Running (Evaluate (catalog, env, a), Primitive_left (op, catalog, env, b, k))) in
  ghost_ (H.source_def out; H.target_def out; H.valid_def table out;
    (match out with
    | Running (control, kont) ->
      H.source_control_def control; H.target_control_def control; H.control_valid_def table control;
      (match control with
      | Evaluate (catalog, env, code) ->
        H.environment_valid_def table env; H.source_environment_def catalog env;
        W.source_def env;
        W.target_def env;
        W.valid_def table env;
        W.environment_def env;
        W.length_def env;
        E.append_def (W.source env) (E.environment catalog); C.erase_def code
      | Return v -> W.source_def v;
        W.target_def v;
        W.valid_def table v;
        W.environment_def v;
        W.length_def v);
      H.source_continuation_def kont; H.target_continuation_def kont; H.continuation_valid_def table kont;
      (match kont with
      | Apply_function (catalog, env, _, _) | Let_body (catalog, env, _, _)
      | Cons_head (catalog, env, _, _) | List_cases (catalog, env, _, _, _)
      | Conditional (catalog, env, _, _, _) | Primitive_left (_, catalog, env, _, _) ->
        H.environment_valid_def table env; H.source_environment_def catalog env;
        W.source_def env;
        W.target_def env;
        W.valid_def table env;
        W.environment_def env;
        W.length_def env;
        E.append_def (W.source env) (E.environment catalog)
      | Apply_argument (v, _) | Cons_tail (v, _) | Primitive_right (_, v, _) ->
        W.source_def v;
        W.target_def v;
        W.valid_def table v;
        W.environment_def v;
        W.length_def v
      | Halt -> ())
    | Done v -> W.source_def v;
      W.target_def v;
      W.valid_def table v;
      W.environment_def v;
      W.length_def v
    | Stuck -> ()));
  out

let (return_value @ total) : (p : C.program) @ immutable ->
    (definitions : {d : C.definitions | C.origins d}) @ immutable ->
    (v : W.t) @ immutable -> (k : H.continuation) @ immutable ->
    {u : unit | p.C.definitions === definitions && H.valid (C.manifest definitions) (Running (Return v, k))} ->
    {out : H.state | H.valid (C.manifest definitions) out
      && H.source out === S.step (H.source (Running (Return v, k)))
      && H.target out === Q.step definitions (H.target (Running (Return v, k)))} @ immutable =
  fun p definitions v k premise ->
  let state = Running (Return v, k) in
  ghost_ (C.ready_def p);
  let table = C.manifest definitions in
  ghost_ (H.source_def state; H.target_def state; H.valid_def table state;
    let control = Return v in
    H.source_control_def control; H.target_control_def control; H.control_valid_def table control;
    W.source_def v; W.target_def v; W.valid_def table v;
    H.source_continuation_def k; H.target_continuation_def k; H.continuation_valid_def table k;
    (match k with
    | Apply_function (catalog, env, _, _) | Let_body (catalog, env, _, _)
    | Cons_head (catalog, env, _, _) | List_cases (catalog, env, _, _, _)
    | Conditional (catalog, env, _, _, _) | Primitive_left (_, catalog, env, _, _) ->
      H.environment_valid_def table env; H.source_environment_def catalog env
    | Apply_argument (f, _) | Cons_tail (f, _) | Primitive_right (_, f, _) ->
      W.source_def f; W.target_def f; W.valid_def table f
    | Halt -> ());
    S.step_def (H.source state); Q.step_def definitions (H.target state));
  let out = (match k with
    | Halt -> Done v
    | Apply_function (catalog, env, arg, rest) -> Running (Evaluate (catalog, env, arg), Apply_argument (v, rest))
    | Apply_argument (f, rest) -> (match f with
      | W.Closure (catalog, body, env) -> Running (Evaluate (catalog, W.Bind (v, env), body), rest)
      | W.Recursive_closure (catalog, body, env) ->
        ghost_ (H.environment_valid_def table (env);
          H.source_environment_def catalog (env);
          W.source_def (env);
          W.target_def (env);
          W.valid_def table (env);
          W.environment_def (env);
          W.length_def (env);
          E.append_def (W.source (env)) (E.environment catalog);
          H.environment_valid_def table (W.Bind (f, env));
          H.source_environment_def catalog (W.Bind (f, env));
          W.source_def (W.Bind (f, env));
          W.target_def (W.Bind (f, env));
          W.valid_def table (W.Bind (f, env));
          W.environment_def (W.Bind (f, env));
          W.length_def (W.Bind (f, env));
          E.append_def (W.source (W.Bind (f, env))) (E.environment catalog));
        Running (Evaluate (catalog, W.Bind (v, W.Bind (f, env)), body), rest)
      | _ -> Stuck)
    | Let_body (catalog, env, body, rest) -> Running (Evaluate (catalog, W.Bind (v, env), body), rest)
    | Cons_head (catalog, env, tail, rest) -> Running (Evaluate (catalog, env, tail), Cons_tail (v, rest))
    | Cons_tail (head, rest) -> Running (Return (W.Cons (head, v)), rest)
    | List_cases (catalog, env, empty, nonempty, rest) -> (match v with
      | W.Nil -> Running (Evaluate (catalog, env, empty), rest)
      | W.Cons (head, tail) ->
        ghost_ (H.environment_valid_def table (W.Bind (tail, env));
          H.source_environment_def catalog (W.Bind (tail, env));
          W.source_def (W.Bind (tail, env));
          W.target_def (W.Bind (tail, env));
          W.valid_def table (W.Bind (tail, env));
          W.environment_def (W.Bind (tail, env));
          W.length_def (W.Bind (tail, env));
          E.append_def (W.source (W.Bind (tail, env))) (E.environment catalog));
        Running (Evaluate (catalog, W.Bind (head, W.Bind (tail, env)), nonempty), rest)
      | _ -> Stuck)
    | Conditional (catalog, env, yes, no, rest) -> (match v with
      | W.True -> Running (Evaluate (catalog, env, yes), rest)
      | W.False -> Running (Evaluate (catalog, env, no), rest)
      | _ -> Stuck)
    | Primitive_left (op, catalog, env, right, rest) ->
      Running (Evaluate (catalog, env, right), Primitive_right (op, v, rest))
    | Primitive_right (op, left, rest) -> (match left, v with
      | W.Word a, W.Word b -> Running (Return (W.primitive table op a b), rest)
      | _ -> Stuck)) in
  ghost_ (H.source_def out; H.target_def out; H.valid_def table out;
    (match out with
    | Running (control, kont) ->
      H.source_control_def control; H.target_control_def control; H.control_valid_def table control;
      (match control with
      | Evaluate (catalog, env, code) ->
        H.environment_valid_def table env; H.source_environment_def catalog env;
        W.source_def env;
        W.target_def env;
        W.valid_def table env;
        W.environment_def env;
        W.length_def env;
        E.append_def (W.source env) (E.environment catalog); C.erase_def code
      | Return v -> W.source_def v;
        W.target_def v;
        W.valid_def table v;
        W.environment_def v;
        W.length_def v);
      H.source_continuation_def kont; H.target_continuation_def kont; H.continuation_valid_def table kont;
      (match kont with
      | Apply_function (catalog, env, _, _) | Let_body (catalog, env, _, _)
      | Cons_head (catalog, env, _, _) | List_cases (catalog, env, _, _, _)
      | Conditional (catalog, env, _, _, _) | Primitive_left (_, catalog, env, _, _) ->
        H.environment_valid_def table env; H.source_environment_def catalog env;
        W.source_def env;
        W.target_def env;
        W.valid_def table env;
        W.environment_def env;
        W.length_def env;
        E.append_def (W.source env) (E.environment catalog)
      | Apply_argument (v, _) | Cons_tail (v, _) | Primitive_right (_, v, _) ->
        W.source_def v;
        W.target_def v;
        W.valid_def table v;
        W.environment_def v;
        W.length_def v
      | Halt -> ())
    | Done v -> W.source_def v;
      W.target_def v;
      W.valid_def table v;
      W.environment_def v;
      W.length_def v
    | Stuck -> ()));
  out

let (step @ total) : (p : C.program) @ immutable -> (definitions : {d : C.definitions | C.origins d}) @ immutable -> (state : H.state) @ immutable ->
    {u : unit | p.C.definitions === definitions && H.valid (C.manifest p.C.definitions) state} ->
    {out : H.state | H.valid (C.manifest p.C.definitions) out
      && H.source out === S.step (H.source state)
      && H.target out === Q.step definitions (H.target state)} @ immutable = fun p definitions state premise ->
  ghost_ (H.valid_def (C.manifest p.C.definitions) state;
    H.source_def state; H.target_def state; S.step_def (H.source state); Q.step_def definitions (H.target state));
  match state with
  | Done _ | Stuck -> state
  | Running (Evaluate (catalog, env, term), k) -> evaluate p definitions catalog env term k ()
  | Running (Return v, k) -> return_value p definitions v k ()
