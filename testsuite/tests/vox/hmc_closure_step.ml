module D = Hm_declarative
module C = Hmc_monomorphic
module M = Hmc_manifest
module K = Hmc_closure_ir
module E = Hmc_closure_extension
module P = Hmc_closure_program
module Q = Hmc_monomorphic_semantics
module R = Hmc_closure_semantics
module W = Hmc_closure_values
module H = Hmc_closure_states
open H

let (evaluate @ total) : (p : P.program) @ immutable ->
    (definitions : {d : C.definitions | C.origins d}) @ immutable ->
    (env : R.V.value) @ immutable -> (source : C.term) @ immutable -> (code : K.term) @ immutable ->
    (k : H.continuation) @ immutable ->
    {u : unit | p.P.origin.C.definitions === definitions && H.valid p.P.table (Running (Evaluate (env, source, code), k))} ->
    {out : H.state | H.valid p.P.table out
      && H.source p.P.table out === Q.step definitions (H.source p.P.table (Running (Evaluate (env, source, code), k)))
      && H.target out === R.step p.P.table p.P.globals (H.target (Running (Evaluate (env, source, code), k)))} @ immutable =
  fun p definitions env source code k premise ->
  let table = p.P.table in let interface = C.manifest definitions in
  let state = Running (Evaluate (env, source, code), k) in
  ghost_ (P.valid_def p; H.source_def table state; H.target_def state; H.valid_def table state);
  ghost_ (let control = Evaluate (env, source, code) in
    H.source_control_def table control; H.target_control_def control; H.control_valid_def table control;
    K.related_def table source code;
    Q.step_def definitions (H.source table state); R.step_def table p.P.globals (H.target state));
  let out = match source, code with
  | C.Local i, K.Local _ -> (match R.lookup env i with
    | None -> ghost_ (W.absent table env i ()); Stuck
    | Some v -> ghost_ (W.local table env i v ()); Running (Return v, k))
  | C.Global (_, _, id), K.Global _ -> (match C.lookup definitions id with
    | None -> ghost_ (P.lookup_absent interface table definitions p.P.globals id ()); Stuck
    | Some d ->
      let code_id = P.lookup_origin interface table definitions p.P.globals id d () in
      ghost_ (K.related_def table d.C.code (K.Closure code_id);
        W.valid_def table R.V.Empty; W.environment_def R.V.Empty; W.source_def table R.V.Empty);
      Running (Return (R.V.Closure (code_id, R.V.Empty)), k))
  | C.Lambda _, K.Closure id | C.Recursive _, K.Closure id -> Running (Return (R.V.Closure (id, env)), k)
  | C.Truth, K.Truth -> Running (Return R.V.True, k)
  | C.False, K.False -> Running (Return R.V.False, k)
  | C.Word w, K.Word _ -> Running (Return (R.V.Word w), k)
  | C.Nil, K.Nil -> Running (Return R.V.Nil, k)
  | C.Apply (f, a), K.Apply (fc, ac) -> Running (Evaluate (env, f, fc), Apply_function (env, a, ac, k))
  | C.Let (rhs, body), K.Let (rc, bc) -> Running (Evaluate (env, rhs, rc), Let_body (env, body, bc, k))
  | C.Cons (head, tail), K.Cons (hc, tc) -> Running (Evaluate (env, head, hc), Cons_head (env, tail, tc, k))
  | C.CaseList (scr, empty, full), K.CaseList (sc, ec, fc) ->
    Running (Evaluate (env, scr, sc), List_cases (env, empty, ec, full, fc, k))
  | C.If (condition, yes, no), K.If (cc, yc, nc) ->
    Running (Evaluate (env, condition, cc), Conditional (env, yes, yc, no, nc, k))
  | C.Primitive (op, a, b), K.Primitive (_, ac, bc) ->
    Running (Evaluate (env, a, ac), Primitive_left (op, env, b, bc, k))
  | _ -> unreachable_ () in
  ghost_ (H.source_def table out; H.target_def out; H.valid_def table out;
    match out with
    | Running (control, kont) ->
      H.source_control_def table control; H.target_control_def control; H.control_valid_def table control;
      (match control with
      | Evaluate (env, _, _) -> W.source_def table env; W.valid_def table env; W.environment_def env
      | Return v -> W.source_def table v; W.valid_def table v);
      H.source_continuation_def table kont; H.target_continuation_def kont; H.continuation_valid_def table kont
    | Done v -> W.source_def table v; W.valid_def table v
    | Stuck -> ());
  out

let (return_value @ total) : (p : P.program) @ immutable ->
    (definitions : {d : C.definitions | C.origins d}) @ immutable ->
    (v : R.V.value) @ immutable -> (k : H.continuation) @ immutable ->
    {u : unit | p.P.origin.C.definitions === definitions && H.valid p.P.table (Running (Return v, k))} ->
    {out : H.state | H.valid p.P.table out
      && H.source p.P.table out === Q.step definitions (H.source p.P.table (Running (Return v, k)))
      && H.target out === R.step p.P.table p.P.globals (H.target (Running (Return v, k)))} @ immutable =
  fun p definitions v k premise ->
  let table = p.P.table in let interface = C.manifest definitions in
  let state = Running (Return v, k) in
  ghost_ (P.valid_def p; H.source_def table state; H.target_def state; H.valid_def table state);
  ghost_ (let control = Return v in
    H.source_control_def table control; H.target_control_def control; H.control_valid_def table control;
    W.source_def table v; W.valid_def table v;
    H.source_continuation_def table k; H.target_continuation_def k; H.continuation_valid_def table k;
    (match k with Apply_argument (f, _) | Cons_tail (f, _) | Primitive_right (_, f, _) ->
      W.source_def table f; W.valid_def table f | _ -> ());
    Q.step_def definitions (H.source table state); R.step_def table p.P.globals (H.target state));
  let out = match k with
  | Halt -> Done v
  | Apply_function (env, arg, code, rest) -> Running (Evaluate (env, arg, code), Apply_argument (v, rest))
  | Apply_argument (f, rest) -> (match f with
    | R.V.Closure (id, env) -> (match K.lookup table id with
      | None -> unreachable_ ()
      | Some entry ->
        ghost_ (E.lookup_valid interface table id entry ());
        let captured = if entry.K.recursive then R.V.Bind (f, env) else env in
        ghost_ (W.source_def table captured; W.valid_def table captured; W.environment_def captured);
        Running (Evaluate (R.V.Bind (v, captured), entry.K.source, entry.K.body), rest))
    | _ -> Stuck)
  | Let_body (env, body, code, rest) -> Running (Evaluate (R.V.Bind (v, env), body, code), rest)
  | Cons_head (env, tail, code, rest) -> Running (Evaluate (env, tail, code), Cons_tail (v, rest))
  | Cons_tail (head, rest) -> Running (Return (R.V.Cons (head, v)), rest)
  | List_cases (env, empty, ec, full, fc, rest) -> (match v with
    | R.V.Nil -> Running (Evaluate (env, empty, ec), rest)
    | R.V.Cons (head, tail) ->
      let captured = R.V.Bind (tail, env) in
      ghost_ (W.source_def table captured; W.valid_def table captured; W.environment_def captured);
      Running (Evaluate (R.V.Bind (head, captured), full, fc), rest)
    | _ -> Stuck)
  | Conditional (env, yes, yc, no, nc, rest) -> (match v with
    | R.V.True -> Running (Evaluate (env, yes, yc), rest)
    | R.V.False -> Running (Evaluate (env, no, nc), rest)
    | _ -> Stuck)
  | Primitive_left (op, env, right, code, rest) -> Running (Evaluate (env, right, code), Primitive_right (op, v, rest))
  | Primitive_right (op, left, rest) -> (match left, v with
    | R.V.Word a, R.V.Word b ->
      ghost_ (W.primitive table op a b);
      Running (Return (R.primitive op a b), rest)
    | _ -> Stuck) in
  ghost_ (H.source_def table out; H.target_def out; H.valid_def table out;
    match out with
    | Running (control, kont) ->
      H.source_control_def table control; H.target_control_def control; H.control_valid_def table control;
      (match control with
      | Evaluate (env, _, _) -> W.source_def table env; W.valid_def table env; W.environment_def env
      | Return v -> W.source_def table v; W.valid_def table v);
      H.source_continuation_def table kont; H.target_continuation_def kont; H.continuation_valid_def table kont
    | Done v -> W.source_def table v; W.valid_def table v
    | Stuck -> ());
  out

let (step @ total) : (p : P.program) @ immutable ->
    (definitions : {d : C.definitions | C.origins d}) @ immutable -> (state : H.state) @ immutable ->
    {u : unit | p.P.origin.C.definitions === definitions && H.valid p.P.table state} ->
    {out : H.state | H.valid p.P.table out
      && H.source p.P.table out === Q.step definitions (H.source p.P.table state)
      && H.target out === R.step p.P.table p.P.globals (H.target state)} @ immutable = fun p definitions state premise ->
  ghost_ (H.source_def p.P.table state; H.target_def state;
    Q.step_def definitions (H.source p.P.table state); R.step_def p.P.table p.P.globals (H.target state));
  match state with
  | Done _ | Stuck -> state
  | Running (Evaluate (env, source, code), k) -> evaluate p definitions env source code k ()
  | Running (Return v, k) -> return_value p definitions v k ()
