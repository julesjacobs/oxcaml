module D = Hm_declarative
module V = Hm_interpreter_typing
module S = Hmc_source_semantics
module E = Hm_evaluation

let (one @ total) : (before : S.state) @ immutable -> (after : S.state) @ immutable ->
    {u : unit | S.step before === after} ->
    {n : D.index | S.advance n before === after} @ immutable ghost =
  fun before after premise -> ghost_ (
    S.advance_def (D.S D.Z) before; S.advance_def D.Z after; D.S D.Z)

let (join @ total) : (a : D.index) @ immutable -> (b : D.index) @ immutable ->
    (before : S.state) @ immutable -> (middle : S.state) @ immutable ->
    (after : S.state) @ immutable ->
    {u : unit | S.advance a before === middle && S.advance b middle === after} ->
    {n : D.index | S.advance n before === after} @ immutable ghost =
  fun a b before middle after premise -> ghost_ (
    S.advance_add a b before; D.add a b)

let (atomic @ total) : (env : V.value) @ immutable -> (term : D.term) @ immutable ->
    (value : V.value) @ immutable -> (k : S.continuation) @ immutable ->
    {u : unit | E.evaluates env term value E.Atomic} ->
    {n : D.index | S.advance n (S.Running (S.Evaluate (env, term), k))
      === S.Running (S.Return value, k)} @ immutable ghost =
  fun env term value k premise -> ghost_ (
    E.evaluates_def env term value E.Atomic;
    let before = S.Running (S.Evaluate (env, term), k) in
    let after = S.Running (S.Return value, k) in
    S.step_def before; one before after ())

let rec (run @ total) : (env : V.value) @ immutable -> (term : D.term) @ immutable ->
    (value : V.value) @ immutable -> (k : S.continuation) @ immutable ->
    (execution : E.execution) @ immutable ->
    {u : unit | E.evaluates env term value execution} ->
    {n : D.index | S.advance n (S.Running (S.Evaluate (env, term), k))
      === S.Running (S.Return value, k)} @ immutable ghost =
  fun env term value k execution premise -> ghost_ (
    E.evaluates_def env term value execution;
    match term, execution with
    | _, E.Atomic -> atomic env term value k ()
    | D.Cons (head, tail), E.Cons (h, a, b) ->
      (match value with
      | V.Cons (_, rest) ->
        let s0 = S.Running (S.Evaluate (env, term), k) in
        let s1 = S.Running (S.Evaluate (env, head), S.Cons_head (env, tail, k)) in
        let s2 = S.Running (S.Return h, (S.Cons_head (env, tail, k))) in
        let s3 = S.Running (S.Evaluate (env, tail), S.Cons_tail (h, k)) in
        let s4 = S.Running (S.Return rest, (S.Cons_tail (h, k))) in
        let s5 = S.Running (S.Return value, k) in
        S.step_def s0;
        let n0 = one s0 s1 () in
        let n1 = run env head h (S.Cons_head (env, tail, k)) a () in
        S.step_def s2;
        let n2 = one s2 s3 () in
        let n3 = run env tail rest (S.Cons_tail (h, k)) b () in
        S.step_def s4;
        let n4 = one s4 s5 () in
        let t1 = join n0 n1 s0 s1 s2 () in
        let t2 = join t1 n2 s0 s2 s3 () in
        let t3 = join t2 n3 s0 s3 s4 () in
        let t4 = join t3 n4 s0 s4 s5 () in
        t4
      | _ -> unreachable_ ())
    | D.Let (rhs, body), E.Let (v, a, b) ->
      let s0 = S.Running (S.Evaluate (env, term), k) in
      let s1 = S.Running (S.Evaluate (env, rhs), S.Let_body (env, body, k)) in
      let s2 = S.Running (S.Return v, (S.Let_body (env, body, k))) in
      let s3 = S.Running (S.Evaluate (V.Bind (v, env), body), k) in
      let s4 = S.Running (S.Return value, k) in
      S.step_def s0;
      let n0 = one s0 s1 () in
      let n1 = run env rhs v (S.Let_body (env, body, k)) a () in
      S.step_def s2;
      let n2 = one s2 s3 () in
      let n3 = run (V.Bind (v, env)) body value k b () in
      let t1 = join n0 n1 s0 s1 s2 () in
      let t2 = join t1 n2 s0 s2 s3 () in
      let t3 = join t2 n3 s0 s3 s4 () in
      t3
    | D.If (condition, yes, no), E.Branch (v, a, b) ->
      (match v with
      | V.True ->
        let s0 = S.Running (S.Evaluate (env, term), k) in
        let s1 = S.Running (S.Evaluate (env, condition), S.Conditional (env, yes, no, k)) in
        let s2 = S.Running (S.Return v, (S.Conditional (env, yes, no, k))) in
        let s3 = S.Running (S.Evaluate (env, yes), k) in
        let s4 = S.Running (S.Return value, k) in
        S.step_def s0;
        let n0 = one s0 s1 () in
        let n1 = run env condition v (S.Conditional (env, yes, no, k)) a () in
        S.step_def s2;
        let n2 = one s2 s3 () in
        let n3 = run env yes value k b () in
        let t1 = join n0 n1 s0 s1 s2 () in
        let t2 = join t1 n2 s0 s2 s3 () in
        let t3 = join t2 n3 s0 s3 s4 () in
        t3
      | V.False ->
        let s0 = S.Running (S.Evaluate (env, term), k) in
        let s1 = S.Running (S.Evaluate (env, condition), S.Conditional (env, yes, no, k)) in
        let s2 = S.Running (S.Return v, (S.Conditional (env, yes, no, k))) in
        let s3 = S.Running (S.Evaluate (env, no), k) in
        let s4 = S.Running (S.Return value, k) in
        S.step_def s0;
        let n0 = one s0 s1 () in
        let n1 = run env condition v (S.Conditional (env, yes, no, k)) a () in
        S.step_def s2;
        let n2 = one s2 s3 () in
        let n3 = run env no value k b () in
        let t1 = join n0 n1 s0 s1 s2 () in
        let t2 = join t1 n2 s0 s2 s3 () in
        let t3 = join t2 n3 s0 s3 s4 () in
        t3
      | _ -> unreachable_ ())
    | D.CaseList (condition, yes, no), E.Branch (v, a, b) ->
      (match v with
      | V.Nil ->
        let s0 = S.Running (S.Evaluate (env, term), k) in
        let s1 = S.Running (S.Evaluate (env, condition), S.List_cases (env, yes, no, k)) in
        let s2 = S.Running (S.Return v, (S.List_cases (env, yes, no, k))) in
        let s3 = S.Running (S.Evaluate (env, yes), k) in
        let s4 = S.Running (S.Return value, k) in
        S.step_def s0;
        let n0 = one s0 s1 () in
        let n1 = run env condition v (S.List_cases (env, yes, no, k)) a () in
        S.step_def s2;
        let n2 = one s2 s3 () in
        let n3 = run env yes value k b () in
        let t1 = join n0 n1 s0 s1 s2 () in
        let t2 = join t1 n2 s0 s2 s3 () in
        let t3 = join t2 n3 s0 s3 s4 () in
        t3
      | V.Cons (head, tail) ->
        let s0 = S.Running (S.Evaluate (env, term), k) in
        let s1 = S.Running (S.Evaluate (env, condition), S.List_cases (env, yes, no, k)) in
        let s2 = S.Running (S.Return v, (S.List_cases (env, yes, no, k))) in
        let s3 = S.Running (S.Evaluate ((V.Bind (head, V.Bind (tail, env))), no), k) in
        let s4 = S.Running (S.Return value, k) in
        S.step_def s0;
        let n0 = one s0 s1 () in
        let n1 = run env condition v (S.List_cases (env, yes, no, k)) a () in
        S.step_def s2;
        let n2 = one s2 s3 () in
        let n3 = run (V.Bind (head, V.Bind (tail, env))) no value k b () in
        let t1 = join n0 n1 s0 s1 s2 () in
        let t2 = join t1 n2 s0 s2 s3 () in
        let t3 = join t2 n3 s0 s3 s4 () in
        t3
      | _ -> unreachable_ ())
    | D.Primitive (op, left, right), E.Primitive (l, r, a, b) ->
      (match l, r with
      | V.Word x, V.Word y ->
        let s0 = S.Running (S.Evaluate (env, term), k) in
        let s1 = S.Running (S.Evaluate (env, left), S.Primitive_left (op, env, right, k)) in
        let s2 = S.Running (S.Return l, (S.Primitive_left (op, env, right, k))) in
        let s3 = S.Running (S.Evaluate (env, right), S.Primitive_right (op, l, k)) in
        let s4 = S.Running (S.Return r, (S.Primitive_right (op, l, k))) in
        let s5 = S.Running (S.Return value, k) in
        S.step_def s0;
        let n0 = one s0 s1 () in
        let n1 = run env left l (S.Primitive_left (op, env, right, k)) a () in
        S.step_def s2;
        let n2 = one s2 s3 () in
        let n3 = run env right r (S.Primitive_right (op, l, k)) b () in
        S.step_def s4;
        let n4 = one s4 s5 () in
        let t1 = join n0 n1 s0 s1 s2 () in
        let t2 = join t1 n2 s0 s2 s3 () in
        let t3 = join t2 n3 s0 s3 s4 () in
        let t4 = join t3 n4 s0 s4 s5 () in
        t4
      | _ -> unreachable_ ())
    | D.Apply (fn, arg), E.Apply (f, v, a, b, c) ->
      (match f with
      | V.Closure (body, captured) ->
        let s0 = S.Running (S.Evaluate (env, term), k) in
        let s1 = S.Running (S.Evaluate (env, fn), S.Apply_function (env, arg, k)) in
        let s2 = S.Running (S.Return f, (S.Apply_function (env, arg, k))) in
        let s3 = S.Running (S.Evaluate (env, arg), S.Apply_argument (f, k)) in
        let s4 = S.Running (S.Return v, (S.Apply_argument (f, k))) in
        let s5 = S.Running (S.Evaluate ((V.Bind (v, captured)), body), k) in
        let s6 = S.Running (S.Return value, k) in
        S.step_def s0;
        let n0 = one s0 s1 () in
        let n1 = run env fn f (S.Apply_function (env, arg, k)) a () in
        S.step_def s2;
        let n2 = one s2 s3 () in
        let n3 = run env arg v (S.Apply_argument (f, k)) b () in
        S.step_def s4;
        let n4 = one s4 s5 () in
        let n5 = run (V.Bind (v, captured)) body value k c () in
        let t1 = join n0 n1 s0 s1 s2 () in
        let t2 = join t1 n2 s0 s2 s3 () in
        let t3 = join t2 n3 s0 s3 s4 () in
        let t4 = join t3 n4 s0 s4 s5 () in
        let t5 = join t4 n5 s0 s5 s6 () in
        t5
      | V.Recursive_closure (body, captured) ->
        let s0 = S.Running (S.Evaluate (env, term), k) in
        let s1 = S.Running (S.Evaluate (env, fn), S.Apply_function (env, arg, k)) in
        let s2 = S.Running (S.Return f, (S.Apply_function (env, arg, k))) in
        let s3 = S.Running (S.Evaluate (env, arg), S.Apply_argument (f, k)) in
        let s4 = S.Running (S.Return v, (S.Apply_argument (f, k))) in
        let s5 = S.Running (S.Evaluate ((V.Bind (v, V.Bind (f, captured))), body), k) in
        let s6 = S.Running (S.Return value, k) in
        S.step_def s0;
        let n0 = one s0 s1 () in
        let n1 = run env fn f (S.Apply_function (env, arg, k)) a () in
        S.step_def s2;
        let n2 = one s2 s3 () in
        let n3 = run env arg v (S.Apply_argument (f, k)) b () in
        S.step_def s4;
        let n4 = one s4 s5 () in
        let n5 = run (V.Bind (v, V.Bind (f, captured))) body value k c () in
        let t1 = join n0 n1 s0 s1 s2 () in
        let t2 = join t1 n2 s0 s2 s3 () in
        let t3 = join t2 n3 s0 s3 s4 () in
        let t4 = join t3 n4 s0 s4 s5 () in
        let t5 = join t4 n5 s0 s5 s6 () in
        t5
      | _ -> unreachable_ ())
    | _ -> unreachable_ ())

let (closed @ total) : (term : D.term) @ immutable -> (value : V.value) @ immutable ->
    (execution : E.execution) @ immutable ->
    {u : unit | E.evaluates V.Empty term value execution} ->
    {n : D.index | S.advance n (S.initial term) === S.Done value} @ immutable ghost =
  fun term value execution premise -> ghost_ (
    let n = run V.Empty term value S.Halt execution () in
    let start = S.Running (S.Evaluate (V.Empty, term), S.Halt) in
    let returned = S.Running (S.Return value, S.Halt) in
    S.initial_def term; S.step_def returned;
    let last = one returned (S.Done value) () in
    join n last start returned (S.Done value) ())
