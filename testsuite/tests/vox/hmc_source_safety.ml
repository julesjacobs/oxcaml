module D = Hm_declarative
module V = Hm_interpreter_typing
module S = Hmc_source_semantics
module Sub = Hm_interpreter_substitution_proofs

type control_evidence = Term of D.context * D.typing * V.evidence | Value of V.evidence [@@inductive]
type continuation_evidence =
  | Stop
  | Function_argument of D.mono * D.mono * D.context * D.typing * V.evidence * continuation_evidence
  | Apply_saved of D.mono * D.mono * V.evidence * continuation_evidence
  | Let_saved of D.scheme * D.context * D.mono * D.typing * V.evidence * continuation_evidence
  | Head of D.mono * D.context * D.typing * V.evidence * continuation_evidence
  | Tail of D.mono * V.evidence * continuation_evidence
  | List_branch of D.mono * D.mono * D.context * D.typing * D.typing * V.evidence * continuation_evidence
  | Choice of D.mono * D.context * D.typing * D.typing * V.evidence * continuation_evidence
  | Prim_left of D.context * D.typing * V.evidence * continuation_evidence
  | Prim_right of V.evidence * continuation_evidence
  [@@inductive]

let[@def] (control_typed @ total) (c : S.control @ immutable)
    (t : D.mono @ immutable) (p : control_evidence @ immutable) = ghost_ (
  match c, p with
  | S.Evaluate (env, term), Term (g, d, ep) ->
    V.typed g term t d && V.valid ep (V.Environment (env, g))
  | S.Return v, Value vp -> V.valid vp (V.Value (v, t))
  | _ -> false)

let[@def] rec (continuation_typed @ total) (k : S.continuation @ immutable)
    (input : D.mono @ immutable) (output : D.mono @ immutable)
    (p : continuation_evidence @ immutable) = ghost_ (
  match k, p with
  | S.Halt, Stop -> input === output
  | S.Apply_function (env, arg, rest), Function_argument (a, b, g, d, ep, rp) ->
    input === D.Function (a, b) && V.typed g arg a d
    && V.valid ep (V.Environment (env, g)) && continuation_typed rest b output rp
  | S.Apply_argument (f, rest), Apply_saved (a, b, fp, rp) -> input === a
    && V.valid fp (V.Value (f, D.Function (a, b))) && continuation_typed rest b output rp
  | S.Let_body (env, body, rest), Let_saved (scheme, g, b, d, ep, rp) ->
    (match scheme with D.Forall (_, a) -> input === a)
    && V.valid ep (V.Environment (env, g))
    && V.typed (D.Binding (scheme, g)) body b d && continuation_typed rest b output rp
  | S.Cons_head (env, tail, rest), Head (a, g, d, ep, rp) -> input === a
    && V.typed g tail (D.List_type a) d && V.valid ep (V.Environment (env, g))
    && continuation_typed rest (D.List_type a) output rp
  | S.Cons_tail (head, rest), Tail (a, hp, rp) -> input === D.List_type a
    && V.valid hp (V.Value (head, a)) && continuation_typed rest (D.List_type a) output rp
  | S.List_cases (env, empty, nonempty, rest), List_branch (a, b, g, l, r, ep, rp) ->
    input === D.List_type a && V.valid ep (V.Environment (env, g))
    && V.typed g empty b l
    && V.typed (D.Binding (D.Forall (D.Z, a),
      D.Binding (D.Forall (D.Z, D.List_type a), g))) nonempty b r
    && continuation_typed rest b output rp
  | S.Conditional (env, yes, no, rest), Choice (a, g, l, r, ep, rp) ->
    input === D.Boolean && V.valid ep (V.Environment (env, g))
    && V.typed g yes a l && V.typed g no a r && continuation_typed rest a output rp
  | S.Primitive_left (op, env, right, rest), Prim_left (g, d, ep, rp) ->
    input === D.Word64 && V.valid ep (V.Environment (env, g))
    && V.typed g right D.Word64 d && continuation_typed rest (D.operation_type op) output rp
  | S.Primitive_right (op, left, rest), Prim_right (lp, rp) ->
    input === D.Word64 && V.valid lp (V.Value (left, D.Word64))
    && continuation_typed rest (D.operation_type op) output rp
  | _ -> false)

type evidence = Active of D.mono * control_evidence * continuation_evidence | Complete of V.evidence [@@inductive]

let[@def] (valid @ total) (state : S.state @ immutable)
    (output : D.mono @ immutable) (p : evidence @ immutable) = ghost_ (
  match state, p with
  | S.Running (c, k), Active (input, cp, kp) ->
    control_typed c input cp && continuation_typed k input output kp
  | S.Done v, Complete vp -> V.valid vp (V.Value (v, output))
  | _ -> false)

let rec (lookup_typed @ total) : (env : V.value) @ immutable ->
    (g : D.context) @ immutable -> (index : D.index) @ immutable ->
    (scheme : D.scheme) @ immutable -> (args : D.arguments) @ immutable ->
    (ep : V.evidence) @ immutable ->
    {u : unit | V.valid ep (V.Environment (env, g)) && D.lookup g index === Some scheme} ->
    {vp : V.evidence | match S.lookup env index with
      | None -> false | Some v -> V.valid vp (V.Value (v, D.open_scheme scheme args))}
      @ immutable ghost = fun env g index scheme args ep premise -> ghost_ (
  V.valid_def ep (V.Environment (env, g)); D.lookup_def g index; S.lookup_def env index;
  match env, g, ep with
  | V.Bind (head, tail), D.Binding (D.Forall (_, a), rest), V.Extend (hp, tp) ->
    (match index with
    | D.Z -> D.open_scheme_def scheme args; Sub.instantiate args head a hp ()
    | D.S i -> lookup_typed tail rest i scheme args tp ())
  | _ -> unreachable_ ())

let (primitive_typed @ total) : (op : D.word_operation) @ immutable ->
    (a : Hmc_word64.t) @ immutable -> (b : Hmc_word64.t) @ immutable ->
    {u : unit | V.valid V.Leaf (V.Value (S.primitive op a b, D.operation_type op))}
      @ ghost = fun op a b -> ghost_ (
  S.primitive_def op a b; D.operation_type_def op;
  V.valid_def V.Leaf (V.Value (S.primitive op a b, D.operation_type op)); ())

let (initial_typed @ total) : (term : D.term) @ immutable ->
    (ty : D.mono) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | D.typed D.Z D.Empty_context term ty d} ->
    {p : evidence | valid (S.initial term) ty p} @ immutable ghost = fun term ty d premise -> ghost_ (
  Hm_interpreter_proofs.erase_typing D.Z D.Empty_context term ty d ();
  V.valid_def V.Leaf (V.Environment (V.Empty, D.Empty_context));
  let cp = Term (D.Empty_context, d, V.Leaf) in
  control_typed_def (S.Evaluate (V.Empty, term)) ty cp;
  continuation_typed_def S.Halt ty ty Stop; S.initial_def term;
  let p = Active (ty, cp, Stop) in valid_def (S.initial term) ty p; p)

let (step_preserves @ total) : (state : S.state) @ immutable ->
    (output : D.mono) @ immutable -> (p : evidence) @ immutable ->
    {u : unit | valid state output p} ->
    {q : evidence | valid (S.step state) output q} @ immutable ghost =
  fun state output p premise -> ghost_ (
  valid_def state output p; S.step_def state;
  let q = match state, p with
  | S.Done _, Complete _ -> p
  | S.Running (S.Evaluate (env, term), k), Active (t, Term (g, d, ep), kp) ->
    control_typed_def (S.Evaluate (env, term)) t (Term (g, d, ep));
    V.typed_def g term t d;
    (match term, d with
    | D.Bound index, D.Variable args ->
      (match D.lookup g index with
      | None -> unreachable_ ()
      | Some scheme ->
        let vp = lookup_typed env g index scheme args ep () in
        Active (t, Value vp, kp))
    | D.Truth, D.Constant | D.False, D.Constant | D.Word _, D.Word_constant
    | D.Nil, D.Empty_list _ ->
      (match S.step state with
      | S.Running (S.Return v, _) -> V.valid_def V.Leaf (V.Value (v, t))
      | _ -> ());
      Active (t, Value V.Leaf, kp)
    | D.Lambda _, D.Abstraction (_, body)
    | D.Recursive _, D.Recursion (_, _, body) ->
      let vp = V.Capture (g, body, ep) in
      (match S.step state with
      | S.Running (S.Return v, _) -> V.valid_def vp (V.Value (v, t))
      | _ -> ());
      Active (t, Value vp, kp)
    | D.Apply (_, _), D.Application (a, left, right) ->
      Active (D.Function (a, t), Term (g, left, ep), Function_argument (a, t, g, right, ep, kp))
    | D.Let (_, _), D.Let_binding (scheme, rhs, body) ->
      (match scheme with D.Forall (n, a) ->
        let wp = Sub.weaken_environment n env g ep () in
        Active (a, Term (D.weaken_context n g, rhs, wp), Let_saved (scheme, g, t, body, ep, kp)))
    | D.Cons (_, _), D.List_cons (a, head, tail) ->
      Active (a, Term (g, head, ep), Head (a, g, tail, ep, kp))
    | D.CaseList (_, _, _), D.List_case (a, scrutinee, empty, nonempty) ->
      Active (D.List_type a, Term (g, scrutinee, ep), List_branch (a, t, g, empty, nonempty, ep, kp))
    | D.If (_, _, _), D.Conditional (condition, yes, no) ->
      Active (D.Boolean, Term (g, condition, ep), Choice (t, g, yes, no, ep, kp))
    | D.Primitive (_, _, _), D.Word_primitive (left, right) ->
      Active (D.Word64, Term (g, left, ep), Prim_left (g, right, ep, kp))
    | _ -> unreachable_ ())
  | S.Running (S.Return v, k), Active (t, Value vp, kp) ->
    control_typed_def (S.Return v) t (Value vp);
    continuation_typed_def k t output kp;
    V.valid_def vp (V.Value (v, t));
    (match k, kp with
    | S.Halt, Stop -> Complete vp
    | S.Apply_function (_, _, _), Function_argument (a, b, g, d, ep, rp) ->
      Active (a, Term (g, d, ep), Apply_saved (a, b, vp, rp))
    | S.Apply_argument (f, _), Apply_saved (a, b, fp, rp) ->
      V.valid_def fp (V.Value (f, D.Function (a, b)));
      (match f, fp with
      | V.Closure (_, captured), V.Capture (g, d, ep) ->
        let bg = D.Binding (D.Forall (D.Z, a), g) in
        let bp = V.Extend (vp, ep) in
        V.valid_def bp (V.Environment (V.Bind (v, captured), bg));
        Active (b, Term (bg, d, bp), rp)
      | V.Recursive_closure (_, captured), V.Capture (g, d, ep) ->
        let sg = D.Binding (D.Forall (D.Z, D.Function (a, b)), g) in
        let sp = V.Extend (fp, ep) in
        V.valid_def sp (V.Environment (V.Bind (f, captured), sg));
        let bg = D.Binding (D.Forall (D.Z, a), sg) in
        let bp = V.Extend (vp, sp) in
        V.valid_def bp (V.Environment (V.Bind (v, V.Bind (f, captured)), bg));
        Active (b, Term (bg, d, bp), rp)
      | _ -> unreachable_ ())
    | S.Let_body (env, _, _), Let_saved (scheme, g, b, d, ep, rp) ->
      let bg = D.Binding (scheme, g) in let bp = V.Extend (vp, ep) in
      V.valid_def bp (V.Environment (V.Bind (v, env), bg));
      Active (b, Term (bg, d, bp), rp)
    | S.Cons_head (_, _, _), Head (a, g, d, ep, rp) ->
      Active (D.List_type a, Term (g, d, ep), Tail (a, vp, rp))
    | S.Cons_tail (head, _), Tail (a, hp, rp) ->
      let proof = V.Elements (hp, vp) in
      V.valid_def proof (V.Value (V.Cons (head, v), D.List_type a));
      Active (D.List_type a, Value proof, rp)
    | S.List_cases (env, _, _, _), List_branch (a, b, g, l, r, ep, rp) ->
      (match v, vp with
      | V.Nil, V.Leaf -> Active (b, Term (g, l, ep), rp)
      | V.Cons (head, tail), V.Elements (hp, tp) ->
        let tg = D.Binding (D.Forall (D.Z, D.List_type a), g) in
        let te = V.Extend (tp, ep) in
        V.valid_def te (V.Environment (V.Bind (tail, env), tg));
        let bg = D.Binding (D.Forall (D.Z, a), tg) in let bp = V.Extend (hp, te) in
        V.valid_def bp (V.Environment (V.Bind (head, V.Bind (tail, env)), bg));
        Active (b, Term (bg, r, bp), rp)
      | _ -> unreachable_ ())
    | S.Conditional (_, _, _, _), Choice (a, g, l, r, ep, rp) ->
      (match v with
      | V.True -> Active (a, Term (g, l, ep), rp)
      | V.False -> Active (a, Term (g, r, ep), rp)
      | _ -> unreachable_ ())
    | S.Primitive_left (_, _, _, _), Prim_left (g, d, ep, rp) ->
      Active (D.Word64, Term (g, d, ep), Prim_right (vp, rp))
    | S.Primitive_right (op, left, _), Prim_right (lp, rp) ->
      V.valid_def lp (V.Value (left, D.Word64));
      (match left, v with
      | V.Word a, V.Word b -> primitive_typed op a b;
        Active (D.operation_type op, Value V.Leaf, rp)
      | _ -> unreachable_ ())
    | _ -> unreachable_ ())
  | S.Running (c, _), Active (t, cp, _) ->
    control_typed_def c t cp; unreachable_ ()
  | _ -> unreachable_ () in
  let after = S.step state in
  valid_def after output q;
  (match after, q with
  | S.Running (c, k), Active (t, cp, kp) ->
    control_typed_def c t cp; continuation_typed_def k t output kp
  | _ -> ());
  q)

let rec (advance_preserves @ total) : (fuel : D.index) @ immutable ->
    (state : S.state) @ immutable -> (output : D.mono) @ immutable ->
    (p : evidence) @ immutable -> {u : unit | valid state output p} ->
    {q : evidence | valid (S.advance fuel state) output q} @ immutable ghost =
  fun fuel state output p premise -> ghost_ (
  S.advance_def fuel state;
  match fuel with
  | D.Z -> p
  | D.S n -> let q = step_preserves state output p () in
    advance_preserves n (S.step state) output q ())

let (source_safe @ total) : (fuel : D.index) @ immutable ->
    (term : D.term) @ immutable -> (ty : D.mono) @ immutable ->
    (d : D.typing) @ immutable -> {u : unit | D.typed D.Z D.Empty_context term ty d} ->
    {u : unit | not (S.advance fuel (S.initial term) === S.Stuck)} @ ghost =
  fun fuel term ty d premise -> ghost_ (
    let p = initial_typed term ty d () in
    let q = advance_preserves fuel (S.initial term) ty p () in
    valid_def (S.advance fuel (S.initial term)) ty q; ())

let (source_return_typed @ total) : (fuel : D.index) @ immutable ->
    (term : D.term) @ immutable -> (ty : D.mono) @ immutable ->
    (d : D.typing) @ immutable -> (value : V.value) @ immutable ->
    {u : unit | D.typed D.Z D.Empty_context term ty d
      && S.advance fuel (S.initial term) === S.Done value} ->
    {p : V.evidence | V.valid p (V.Value (value, ty))} @ immutable ghost =
  fun fuel term ty d value premise -> ghost_ (
    let p = initial_typed term ty d () in
    let q = advance_preserves fuel (S.initial term) ty p () in
    valid_def (S.advance fuel (S.initial term)) ty q;
    match q with Complete vp -> vp | Active _ -> unreachable_ ())
