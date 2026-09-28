module D = Hm_declarative
module V = Hm_interpreter_typing
module S = Hmc_source_semantics
module E = Hm_evaluation
module C = Hm_evaluation_continuation

let (evaluated @ total) : (env : V.value) @ immutable -> (term : D.term) @ immutable ->
    (value : V.value) @ immutable -> (execution : E.execution) @ immutable ->
    (k : S.continuation) @ immutable -> (result : V.value) @ immutable ->
    (rest : C.completion) @ immutable ->
    {u : unit | E.evaluates env term value execution && C.completes k value result rest} ->
    {p : C.execution | C.finishes (S.Running (S.Evaluate (env, term), k)) result p}
      @ immutable ghost = fun env term value execution k result rest premise -> ghost_ (
    let p = C.Evaluate_term (value, execution, rest) in
    C.finishes_def (S.Running (S.Evaluate (env, term), k)) result p; p)

let (returned @ total) : (value : V.value) @ immutable -> (k : S.continuation) @ immutable ->
    (result : V.value) @ immutable -> (rest : C.completion) @ immutable ->
    {u : unit | C.completes k value result rest} ->
    {p : C.execution | C.finishes (S.Running (S.Return value, k)) result p}
      @ immutable ghost = fun value k result rest premise -> ghost_ (
    let p = C.Return rest in C.finishes_def (S.Running (S.Return value, k)) result p; p)

let (previous @ total) : (state : S.state) @ immutable -> (result : V.value) @ immutable ->
    (proof : C.execution) @ immutable ->
    {u : unit | C.finishes (S.step state) result proof} ->
    {p : C.execution | C.finishes state result p} @ immutable ghost =
  fun state result proof premise -> ghost_ (
    S.step_def state; C.finishes_def (S.step state) result proof;
    match state with
    | S.Done _ | S.Stuck -> proof
    | S.Running (S.Evaluate (env, term), k) ->
      (match term, proof with
      | D.Truth, C.Return rest ->
        let execution = E.Atomic in
        E.evaluates_def env term V.True execution;
        evaluated env term V.True execution k result rest ()
      | D.False, C.Return rest ->
        let execution = E.Atomic in
        E.evaluates_def env term V.False execution;
        evaluated env term V.False execution k result rest ()
      | D.Word word, C.Return rest ->
        let execution = E.Atomic in
        E.evaluates_def env term (V.Word word) execution;
        evaluated env term (V.Word word) execution k result rest ()
      | D.Nil, C.Return rest ->
        let execution = E.Atomic in
        E.evaluates_def env term V.Nil execution;
        evaluated env term V.Nil execution k result rest ()
      | D.Lambda body, C.Return rest ->
        let execution = E.Atomic in
        E.evaluates_def env term (V.Closure (body, env)) execution;
        evaluated env term (V.Closure (body, env)) execution k result rest ()
      | D.Recursive body, C.Return rest ->
        let execution = E.Atomic in
        E.evaluates_def env term (V.Recursive_closure (body, env)) execution;
        evaluated env term (V.Recursive_closure (body, env)) execution k result rest ()
      | D.Bound index, C.Return rest ->
        (match S.lookup env index with
        | None -> unreachable_ ()
        | Some value ->
        let execution = E.Atomic in
        E.evaluates_def env term value execution;
        evaluated env term value execution k result rest ())
      | D.Let (rhs, body), C.Evaluate_term (v, a, next) ->
        C.completes_def (S.Let_body (env, body, k)) v result next;
        (match next with
        | C.Evaluate (value, b, rest) ->
        let execution = E.Let (v, a, b) in
        E.evaluates_def env term value execution;
        evaluated env term value execution k result rest ()
        | _ -> unreachable_ ())
      | D.If (condition, yes, no), C.Evaluate_term (v, a, next) ->
        C.completes_def (S.Conditional (env, yes, no, k)) v result next;
        (match next with
        | C.Evaluate (value, b, rest) ->
        let execution = E.Branch (v, a, b) in
        E.evaluates_def env term value execution;
        evaluated env term value execution k result rest ()
        | _ -> unreachable_ ())
      | D.CaseList (condition, yes, no), C.Evaluate_term (v, a, next) ->
        C.completes_def (S.List_cases (env, yes, no, k)) v result next;
        (match next with
        | C.Evaluate (value, b, rest) ->
        let execution = E.Branch (v, a, b) in
        E.evaluates_def env term value execution;
        evaluated env term value execution k result rest ()
        | _ -> unreachable_ ())
      | D.Cons (head, tail), C.Evaluate_term (h, a, next) ->
        C.completes_def (S.Cons_head (env, tail, k)) h result next;
        (match next with
        | C.Evaluate (t, b, more) ->
          C.completes_def (S.Cons_tail (h, k)) t result more;
          (match more with
          | C.Pass rest ->
        let execution = E.Cons (h, a, b) in
        E.evaluates_def env term (V.Cons (h, t)) execution;
        evaluated env term (V.Cons (h, t)) execution k result rest ()
          | _ -> unreachable_ ())
        | _ -> unreachable_ ())
      | D.Primitive (op, left, right), C.Evaluate_term (l, a, next) ->
        C.completes_def (S.Primitive_left (op, env, right, k)) l result next;
        (match next with
        | C.Evaluate (r, b, more) ->
          C.completes_def (S.Primitive_right (op, l, k)) r result more;
          (match more, l, r with
          | C.Pass rest, V.Word x, V.Word y ->
        let execution = E.Primitive (l, r, a, b) in
        E.evaluates_def env term (S.primitive op x y) execution;
        evaluated env term (S.primitive op x y) execution k result rest ()
          | _ -> unreachable_ ())
        | _ -> unreachable_ ())
      | D.Apply (fn, arg), C.Evaluate_term (f, a, next) ->
        C.completes_def (S.Apply_function (env, arg, k)) f result next;
        (match next with
        | C.Evaluate (v, b, more) ->
          C.completes_def (S.Apply_argument (f, k)) v result more;
          (match more with
          | C.Evaluate (value, c, rest) ->
        let execution = E.Apply (f, v, a, b, c) in
        E.evaluates_def env term value execution;
        evaluated env term value execution k result rest ()
          | _ -> unreachable_ ())
        | _ -> unreachable_ ())
      | _ -> unreachable_ ())
    | S.Running (S.Return value, k) ->
      (match k, proof with
      | S.Halt, C.Done ->
        C.completes_def k value result C.Halt;
        returned value k result C.Halt ()
      | S.Apply_function _, C.Evaluate_term (v, e, rest) ->
        let next = C.Evaluate (v, e, rest) in
        C.completes_def k value result next;
        returned value k result next ()
      | S.Apply_argument _, C.Evaluate_term (v, e, rest) ->
        let next = C.Evaluate (v, e, rest) in
        C.completes_def k value result next;
        returned value k result next ()
      | S.Let_body _, C.Evaluate_term (v, e, rest) ->
        let next = C.Evaluate (v, e, rest) in
        C.completes_def k value result next;
        returned value k result next ()
      | S.Cons_head _, C.Evaluate_term (v, e, rest) ->
        let next = C.Evaluate (v, e, rest) in
        C.completes_def k value result next;
        returned value k result next ()
      | S.List_cases _, C.Evaluate_term (v, e, rest) ->
        let next = C.Evaluate (v, e, rest) in
        C.completes_def k value result next;
        returned value k result next ()
      | S.Conditional _, C.Evaluate_term (v, e, rest) ->
        let next = C.Evaluate (v, e, rest) in
        C.completes_def k value result next;
        returned value k result next ()
      | S.Primitive_left _, C.Evaluate_term (v, e, rest) ->
        let next = C.Evaluate (v, e, rest) in
        C.completes_def k value result next;
        returned value k result next ()
      | S.Cons_tail _, C.Return rest ->
        let next = C.Pass rest in
        C.completes_def k value result next;
        returned value k result next ()
      | S.Primitive_right _, C.Return rest ->
        let next = C.Pass rest in
        C.completes_def k value result next;
        returned value k result next ()
      | _ -> unreachable_ ()))

let rec (prefix @ total) : (fuel : D.index) @ immutable -> (state : S.state) @ immutable ->
    (result : V.value) @ immutable ->
    {u : unit | S.advance fuel state === S.Done result} ->
    {p : C.execution | C.finishes state result p} @ immutable ghost =
  fun fuel state result premise -> ghost_ (
    S.advance_def fuel state;
    match fuel with
    | D.Z -> C.finishes_def state result C.Done; C.Done
    | D.S n ->
      let p = prefix n (S.step state) result () in previous state result p ())

let (closed @ total) : (fuel : D.index) @ immutable -> (term : D.term) @ immutable ->
    (value : V.value) @ immutable ->
    {u : unit | S.advance fuel (S.initial term) === S.Done value} ->
    {e : E.execution | E.evaluates V.Empty term value e} @ immutable ghost =
  fun fuel term value premise -> ghost_ (
    let p = prefix fuel (S.initial term) value () in
    S.initial_def term; C.finishes_def (S.initial term) value p;
    match p with
    | C.Evaluate_term (v, e, rest) -> C.completes_def S.Halt v value rest; e
    | _ -> unreachable_ ())
