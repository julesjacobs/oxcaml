module D = Hm_declarative
module E = Hm_evaluation
module Machine = Hmc_source_semantics
module S = Hm_interpreter_substitution_proofs
module Spec = Hm_interpreter_typing
open Hm_interpreter_typing

type certificate = {typing : evidence; execution : E.execution}
type result = #{value : value; evidence : certificate @@ ghost}
type request = #{context : D.context @@ ghost; ty : D.mono @@ ghost;
  derivation : D.typing @@ ghost; environment : evidence @@ ghost}
type lookup_request = #{context : D.context @@ ghost; scheme : D.scheme @@ ghost;
  arguments : D.arguments @@ ghost; environment : evidence @@ ghost}
type binding = #{scheme : D.scheme @@ ghost; context : D.context @@ ghost;
  head : evidence @@ ghost; rest : evidence @@ ghost}

let rec lookup : (env : value) @ immutable -> (index : D.index) @ immutable ->
    (p : {p : lookup_request |
      valid p.#environment (Environment (env, p.#context))
      && D.lookup p.#context index === Some p.#scheme}) @ immutable ->
    (target_env : value) @ immutable ghost -> (target_term : D.term) @ immutable ghost ->
    (finish : ((value : value) @ immutable ->
      {u : unit | Machine.lookup env index === Some value} ->
      {e : E.execution | E.evaluates target_env target_term value e} @ immutable)) @ total ghost ->
    {r : result | valid r.#evidence.typing
      (Value (r.#value, D.open_scheme p.#scheme p.#arguments))
      && E.evaluates target_env target_term r.#value r.#evidence.execution} @ immutable =
  fun env index p target_env target_term finish ->
  ghost_ (valid_def p.#environment (Environment (env, p.#context));
    D.lookup_def p.#context index; Machine.lookup_def env index);
  match env with
  | Bind (v, rest) ->
    let b = ghost_ (match p.#environment, p.#context with
      | Extend (head, tail), D.Binding (scheme, context) ->
        #{scheme; context; head; rest = tail}
      | _ -> unreachable_ ()) in
    (match index with
     | D.Z ->
       let proof = ghost_ (
         D.open_scheme_def p.#scheme p.#arguments;
         match b.#scheme with D.Forall (_, ty) ->
           S.instantiate p.#arguments v ty b.#head ()) in
       #{value = v; evidence = ghost_ {typing = proof; execution = finish v ()}}
     | D.S index -> lookup rest index
       #{context = ghost_ b.#context; scheme = ghost_ p.#scheme;
         arguments = ghost_ p.#arguments; environment = ghost_ b.#rest}
       (ghost_ target_env) (ghost_ target_term)
       (ghost_ (fun value premise -> finish value ())))
  | _ -> unreachable_ ()

let[@def] (primitive_value @ total) (op : D.word_operation @ immutable)
    (a : Hmc_word64.t @ immutable) (b : Hmc_word64.t @ immutable) =
  match op with
  | D.Add -> Word (Hmc_word64.add a b)
  | D.Subtract -> Word (Hmc_word64.subtract a b)
  | D.Equal_word -> if Hmc_word64.equal a b then True else False
  | D.Unsigned_less -> if Hmc_word64.unsigned_less a b then True else False

let (primitive_typed @ total) : (op : D.word_operation) @ immutable ->
    (a : Hmc_word64.t) @ immutable -> (b : Hmc_word64.t) @ immutable ->
    {u : unit | valid Leaf (Value (primitive_value op a b, D.operation_type op))}
      @ ghost = fun op a b -> ghost_ (
  primitive_value_def op a b; D.operation_type_def op;
  valid_def Leaf (Value (primitive_value op a b, D.operation_type op)); ())

let rec eval : (env : value) @ immutable -> (term : D.term) @ immutable ->
    (p : {p : request | typed p.#context term p.#ty p.#derivation
      && valid p.#environment (Environment (env, p.#context))}) @ immutable ->
    (target_env : value) @ immutable ghost -> (target_term : D.term) @ immutable ghost ->
    (finish : ((value : value) @ immutable ->
      (execution : {e : E.execution | E.evaluates env term value e}) @ immutable ->
      {e : E.execution | E.evaluates target_env target_term value e} @ immutable)) @ total ghost ->
    {r : result | valid r.#evidence.typing (Value (r.#value, p.#ty))
      && E.evaluates target_env target_term r.#value r.#evidence.execution} @ immutable =
  fun env term p target_env target_term finish ->
  ghost_ (typed_def p.#context term p.#ty p.#derivation);
  match term with
  | D.Truth ->
    ghost_ (valid_def Leaf (Value (True, p.#ty)));
    ghost_ (E.evaluates_def env term True E.Atomic);
    #{value = True; evidence = ghost_ {typing = Leaf; execution = finish True E.Atomic}}
  | D.False ->
    ghost_ (valid_def Leaf (Value (False, p.#ty)));
    ghost_ (E.evaluates_def env term False E.Atomic);
    #{value = False; evidence = ghost_ {typing = Leaf; execution = finish False E.Atomic}}
  | D.Word word ->
    let value = Word word in
    ghost_ (valid_def Leaf (Value (value, p.#ty)));
    ghost_ (E.evaluates_def env term value E.Atomic);
    #{value = value; evidence = ghost_ {typing = Leaf; execution = finish value E.Atomic}}
  | D.Nil ->
    ghost_ (valid_def Leaf (Value (Nil, p.#ty)));
    ghost_ (E.evaluates_def env term Nil E.Atomic);
    #{value = Nil; evidence = ghost_ {typing = Leaf; execution = finish Nil E.Atomic}}
  | D.Cons (head, tail) ->
    let hp = ghost_ (match p.#derivation with
      | D.List_cons (a, d, _) ->
        #{context = p.#context; ty = a; derivation = d; environment = p.#environment}
      | _ -> unreachable_ ()) in
    let h = eval env head #{context = ghost_ hp.#context; ty = ghost_ hp.#ty;
      derivation = ghost_ hp.#derivation; environment = ghost_ hp.#environment}
      (ghost_ env) (ghost_ head) (ghost_ (fun value execution -> execution)) in
    let tp = ghost_ (match p.#derivation with
      | D.List_cons (_, _, d) ->
        #{context = p.#context; ty = p.#ty; derivation = d; environment = p.#environment}
      | _ -> unreachable_ ()) in
    let t = eval env tail #{context = ghost_ tp.#context; ty = ghost_ tp.#ty;
      derivation = ghost_ tp.#derivation; environment = ghost_ tp.#environment}
      (ghost_ env) (ghost_ tail) (ghost_ (fun value execution -> execution)) in
    let value = Cons (h.#value, t.#value) in
    let evidence = ghost_ (Elements (h.#evidence.typing, t.#evidence.typing)) in
    ghost_ (valid_def evidence (Value (value, p.#ty)));
    let execution = ghost_ (E.Cons (h.#value, h.#evidence.execution, t.#evidence.execution)) in
    ghost_ (E.evaluates_def env term value execution);
    #{value = value; evidence = ghost_ {typing = evidence; execution = finish value execution}}
  | D.CaseList (scrutinee, empty, nonempty) ->
    let sp = ghost_ (match p.#derivation with
      | D.List_case (a, d, _, _) ->
        #{context = p.#context; ty = D.List_type a; derivation = d; environment = p.#environment}
      | _ -> unreachable_ ()) in
    let v = eval env scrutinee #{context = ghost_ sp.#context; ty = ghost_ sp.#ty;
      derivation = ghost_ sp.#derivation; environment = ghost_ sp.#environment}
      (ghost_ env) (ghost_ scrutinee) (ghost_ (fun value execution -> execution)) in
    ghost_ (valid_def v.#evidence.typing (Value (v.#value, sp.#ty)));
    (match v.#value with
    | Nil ->
      let d = ghost_ (match p.#derivation with
        | D.List_case (_, _, d, _) -> d | _ -> unreachable_ ()) in
      eval env empty #{context = ghost_ p.#context; ty = ghost_ p.#ty;
        derivation = ghost_ d; environment = ghost_ p.#environment}
      (ghost_ target_env) (ghost_ target_term)
        (ghost_ (fun value child ->
          let execution = E.Branch (v.#value, v.#evidence.execution, child) in
          E.evaluates_def env term value execution;
          finish value execution))
    | Cons (head, tail) ->
      let tail_env = Bind (tail, env) in
      let body_env = Bind (head, tail_env) in
      let bp = ghost_ (match p.#derivation, v.#evidence.typing with
        | D.List_case (a, _, _, d), Elements (head_proof, tail_proof) ->
          let tail_context = D.Binding (D.Forall (D.Z, D.List_type a), p.#context) in
          let tail_environment = Extend (tail_proof, p.#environment) in
          valid_def tail_environment (Environment (tail_env, tail_context));
          let context = D.Binding (D.Forall (D.Z, a), tail_context) in
          let environment = Extend (head_proof, tail_environment) in
          valid_def environment (Environment (body_env, context));
          #{context; ty = p.#ty; derivation = d; environment}
        | _ -> unreachable_ ()) in
      eval body_env nonempty #{context = ghost_ bp.#context; ty = ghost_ bp.#ty;
        derivation = ghost_ bp.#derivation; environment = ghost_ bp.#environment}
      (ghost_ target_env) (ghost_ target_term)
        (ghost_ (fun value child ->
          let execution = E.Branch (v.#value, v.#evidence.execution, child) in
          E.evaluates_def env term value execution;
          finish value execution))
    | _ -> unreachable_ ())
  | D.If (condition, yes, no) ->
    let d = ghost_ (match p.#derivation with
      | D.Conditional (d, _, _) -> d | _ -> unreachable_ ()) in
    let c = eval env condition #{context = ghost_ p.#context; ty = D.Boolean;
      derivation = ghost_ d; environment = ghost_ p.#environment}
      (ghost_ env) (ghost_ condition) (ghost_ (fun value execution -> execution)) in
    ghost_ (valid_def c.#evidence.typing (Value (c.#value, D.Boolean)));
    (match c.#value with
    | True ->
      let d = ghost_ (match p.#derivation with
        | D.Conditional (_, d, _) -> d | _ -> unreachable_ ()) in
      eval env yes #{context = ghost_ p.#context; ty = ghost_ p.#ty;
        derivation = ghost_ d; environment = ghost_ p.#environment}
      (ghost_ target_env) (ghost_ target_term)
        (ghost_ (fun value child ->
          let execution = E.Branch (c.#value, c.#evidence.execution, child) in
          E.evaluates_def env term value execution;
          finish value execution))
    | False ->
      let d = ghost_ (match p.#derivation with
        | D.Conditional (_, _, d) -> d | _ -> unreachable_ ()) in
      eval env no #{context = ghost_ p.#context; ty = ghost_ p.#ty;
        derivation = ghost_ d; environment = ghost_ p.#environment}
      (ghost_ target_env) (ghost_ target_term)
        (ghost_ (fun value child ->
          let execution = E.Branch (c.#value, c.#evidence.execution, child) in
          E.evaluates_def env term value execution;
          finish value execution))
    | _ -> unreachable_ ())
  | D.Primitive (op, left, right) ->
    let ld = ghost_ (match p.#derivation with
      | D.Word_primitive (d, _) -> d | _ -> unreachable_ ()) in
    let l = eval env left #{context = ghost_ p.#context; ty = D.Word64;
      derivation = ghost_ ld; environment = ghost_ p.#environment}
      (ghost_ env) (ghost_ left) (ghost_ (fun value execution -> execution)) in
    let rd = ghost_ (match p.#derivation with
      | D.Word_primitive (_, d) -> d | _ -> unreachable_ ()) in
    let r = eval env right #{context = ghost_ p.#context; ty = D.Word64;
      derivation = ghost_ rd; environment = ghost_ p.#environment}
      (ghost_ env) (ghost_ right) (ghost_ (fun value execution -> execution)) in
    ghost_ (valid_def l.#evidence.typing (Value (l.#value, D.Word64));
      valid_def r.#evidence.typing (Value (r.#value, D.Word64)));
    (match l.#value, r.#value with
    | Word a, Word b ->
      let value = primitive_value op a b in
      ghost_ (primitive_typed op a b);
      let execution = ghost_ (E.Primitive (l.#value, r.#value, l.#evidence.execution, r.#evidence.execution)) in
      ghost_ (primitive_value_def op a b; Machine.primitive_def op a b;
        E.evaluates_def env term value execution);
      #{value = value; evidence = ghost_ {typing = Leaf; execution = finish value execution}}
    | _ -> unreachable_ ())
  | D.Bound index ->
    let actuals = ghost_ (match p.#derivation with
      | D.Variable args -> args | _ -> unreachable_ ()) in
    let scheme = ghost_ (match D.lookup p.#context index with
      | Some s -> s | None -> unreachable_ ()) in
    lookup env index #{context = ghost_ p.#context; scheme = ghost_ scheme;
      arguments = ghost_ actuals; environment = ghost_ p.#environment}
      (ghost_ target_env) (ghost_ target_term)
      (ghost_ (fun value premise ->
        E.evaluates_def env term value E.Atomic; finish value E.Atomic))
  | D.Lambda body ->
    let proof = ghost_ (match p.#derivation with
      | D.Abstraction (_, d) -> Capture (p.#context, d, p.#environment)
      | _ -> unreachable_ ()) in
    let v = Closure (body, env) in
    ghost_ (valid_def proof (Value (v, p.#ty)));
    ghost_ (E.evaluates_def env term v E.Atomic);
    #{value = v; evidence = ghost_ {typing = proof; execution = finish v E.Atomic}}
  | D.Recursive body ->
    let proof = ghost_ (match p.#derivation with
      | D.Recursion (_, _, d) -> Capture (p.#context, d, p.#environment)
      | _ -> unreachable_ ()) in
    let v = Recursive_closure (body, env) in
    ghost_ (valid_def proof (Value (v, p.#ty)));
    ghost_ (E.evaluates_def env term v E.Atomic);
    #{value = v; evidence = ghost_ {typing = proof; execution = finish v E.Atomic}}
  | D.Let (rhs, body) ->
    let rp = ghost_ (match p.#derivation with
      | D.Let_binding (D.Forall (k, a), d, _) ->
        #{context = D.weaken_context k p.#context; ty = a; derivation = d;
          environment = S.weaken_environment k env p.#context p.#environment ()}
      | _ -> unreachable_ ()) in
    let r = eval env rhs #{context = ghost_ rp.#context; ty = ghost_ rp.#ty;
      derivation = ghost_ rp.#derivation; environment = ghost_ rp.#environment}
      (ghost_ env) (ghost_ rhs) (ghost_ (fun value execution -> execution)) in
    let body_env = Bind (r.#value, env) in
    let bp = ghost_ (match p.#derivation with
      | D.Let_binding (scheme, _, d) ->
        let context = D.Binding (scheme, p.#context) in
        let environment = Extend (r.#evidence.typing, p.#environment) in
        valid_def environment (Environment (body_env, context));
        #{context; ty = p.#ty; derivation = d; environment}
      | _ -> unreachable_ ()) in
    eval body_env body #{context = ghost_ bp.#context; ty = ghost_ bp.#ty;
      derivation = ghost_ bp.#derivation; environment = ghost_ bp.#environment}
      (ghost_ target_env) (ghost_ target_term)
        (ghost_ (fun value child ->
          let execution = E.Let (r.#value, r.#evidence.execution, child) in
          E.evaluates_def env term value execution;
          finish value execution))
  | D.Apply (fn, arg) ->
    let fp = ghost_ (match p.#derivation with
      | D.Application (a, d, _) ->
        #{context = p.#context; ty = D.Function (a, p.#ty);
          derivation = d; environment = p.#environment}
      | _ -> unreachable_ ()) in
    let f = eval env fn #{context = ghost_ fp.#context; ty = ghost_ fp.#ty;
      derivation = ghost_ fp.#derivation; environment = ghost_ fp.#environment}
      (ghost_ env) (ghost_ fn) (ghost_ (fun value execution -> execution)) in
    let ap = ghost_ (match p.#derivation with
      | D.Application (a, _, d) ->
        #{context = p.#context; ty = a;
          derivation = d; environment = p.#environment}
      | _ -> unreachable_ ()) in
    let a = eval env arg #{context = ghost_ ap.#context; ty = ghost_ ap.#ty;
      derivation = ghost_ ap.#derivation; environment = ghost_ ap.#environment}
      (ghost_ env) (ghost_ arg) (ghost_ (fun value execution -> execution)) in
    ghost_ (valid_def f.#evidence.typing (Value (f.#value, fp.#ty)));
    match f.#value with
    | Closure (body, captured) ->
      let body_env = Bind (a.#value, captured) in
      let bp = ghost_ (match f.#evidence.typing with
        | Capture (g, d, captured_proof) ->
          let context = D.Binding (D.Forall (D.Z, ap.#ty), g) in
          let environment = Extend (a.#evidence.typing, captured_proof) in
          valid_def environment (Environment (body_env, context));
          #{context; ty = p.#ty; derivation = d; environment}
        | _ -> unreachable_ ()) in
      eval body_env body #{context = ghost_ bp.#context; ty = ghost_ bp.#ty;
      derivation = ghost_ bp.#derivation; environment = ghost_ bp.#environment}
      (ghost_ target_env) (ghost_ target_term)
        (ghost_ (fun value child ->
          let execution = E.Apply (f.#value, a.#value, f.#evidence.execution, a.#evidence.execution, child) in
          E.evaluates_def env term value execution;
          finish value execution))
    | Recursive_closure (body, captured) ->
      let self_env = Bind (f.#value, captured) in
      let body_env = Bind (a.#value, self_env) in
      let bp = ghost_ (match f.#evidence.typing with
        | Capture (g, d, captured_proof) ->
          let self_context = D.Binding (D.Forall (D.Z, fp.#ty), g) in
          let self_proof = Extend (f.#evidence.typing, captured_proof) in
          valid_def self_proof (Environment (self_env, self_context));
          let context = D.Binding (D.Forall (D.Z, ap.#ty), self_context) in
          let environment = Extend (a.#evidence.typing, self_proof) in
          valid_def environment (Environment (body_env, context));
          #{context; ty = p.#ty; derivation = d; environment}
        | _ -> unreachable_ ()) in
      eval body_env body #{context = ghost_ bp.#context; ty = ghost_ bp.#ty;
      derivation = ghost_ bp.#derivation; environment = ghost_ bp.#environment}
      (ghost_ target_env) (ghost_ target_term)
        (ghost_ (fun value child ->
          let execution = E.Apply (f.#value, a.#value, f.#evidence.execution, a.#evidence.execution, child) in
          E.evaluates_def env term value execution;
          finish value execution))
    | True | False | Word _ | Nil | Cons _ | Empty | Bind _ -> unreachable_ ()

type typing = #{ty : D.mono @@ ghost; derivation : D.typing @@ ghost}

let run : (term : D.term) @ immutable ->
    (p : {p : typing | D.typed D.Z D.Empty_context term p.#ty p.#derivation})
      @ immutable ->
    {r : result | valid r.#evidence.typing (Value (r.#value, p.#ty)) && E.evaluates Empty term r.#value r.#evidence.execution} @ immutable =
  fun term p ->
  ghost_ (Hm_interpreter_proofs.erase_typing D.Z D.Empty_context
    term p.#ty p.#derivation ();
    valid_def Leaf (Environment (Empty, D.Empty_context)));
  eval Empty term #{context = D.Empty_context; ty = p.#ty;
    derivation = p.#derivation; environment = Leaf}
    (ghost_ Empty) (ghost_ term) (ghost_ (fun value execution -> execution))
