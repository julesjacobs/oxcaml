module D = Hm_declarative
module S = Hm_interpreter_substitution_proofs
module Spec = Hm_interpreter_typing
open Hm_interpreter_typing

type result = #{value : value; evidence : evidence @@ ghost}
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
    {r : result | valid r.#evidence
      (Value (r.#value, D.open_scheme p.#scheme p.#arguments))} @ immutable =
  fun env index p ->
  ghost_ (valid_def p.#environment (Environment (env, p.#context));
    D.lookup_def p.#context index);
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
       #{value = v; evidence = ghost_ proof}
     | D.S index -> lookup rest index
       #{context = ghost_ b.#context; scheme = ghost_ p.#scheme;
         arguments = ghost_ p.#arguments; environment = ghost_ b.#rest})
  | _ -> unreachable_ ()

let rec eval : (env : value) @ immutable -> (term : D.term) @ immutable ->
    (p : {p : request | typed p.#context term p.#ty p.#derivation
      && valid p.#environment (Environment (env, p.#context))}) @ immutable ->
    {r : result | valid r.#evidence (Value (r.#value, p.#ty))} @ immutable =
  fun env term p ->
  ghost_ (typed_def p.#context term p.#ty p.#derivation);
  match term with
  | D.Truth ->
    ghost_ (valid_def Leaf (Value (True, p.#ty)));
    #{value = True; evidence = Leaf}
  | D.Bound index ->
    let actuals = ghost_ (match p.#derivation with
      | D.Variable args -> args | _ -> unreachable_ ()) in
    let scheme = ghost_ (match D.lookup p.#context index with
      | Some s -> s | None -> unreachable_ ()) in
    lookup env index #{context = ghost_ p.#context; scheme = ghost_ scheme;
      arguments = ghost_ actuals; environment = ghost_ p.#environment}
  | D.Lambda body ->
    let proof = ghost_ (match p.#derivation with
      | D.Abstraction (_, d) -> Capture (p.#context, d, p.#environment)
      | _ -> unreachable_ ()) in
    let v = Closure (body, env) in
    ghost_ (valid_def proof (Value (v, p.#ty)));
    #{value = v; evidence = ghost_ proof}
  | D.Recursive body ->
    let proof = ghost_ (match p.#derivation with
      | D.Recursion (_, _, d) -> Capture (p.#context, d, p.#environment)
      | _ -> unreachable_ ()) in
    let v = Recursive_closure (body, env) in
    ghost_ (valid_def proof (Value (v, p.#ty)));
    #{value = v; evidence = ghost_ proof}
  | D.Let (rhs, body) ->
    let rp = ghost_ (match p.#derivation with
      | D.Let_binding (D.Forall (k, a), d, _) ->
        #{context = D.weaken_context k p.#context; ty = a; derivation = d;
          environment = S.weaken_environment k env p.#context p.#environment ()}
      | _ -> unreachable_ ()) in
    let r = eval env rhs #{context = ghost_ rp.#context; ty = ghost_ rp.#ty;
      derivation = ghost_ rp.#derivation; environment = ghost_ rp.#environment} in
    let body_env = Bind (r.#value, env) in
    let bp = ghost_ (match p.#derivation with
      | D.Let_binding (scheme, _, d) ->
        let context = D.Binding (scheme, p.#context) in
        let environment = Extend (r.#evidence, p.#environment) in
        valid_def environment (Environment (body_env, context));
        #{context; ty = p.#ty; derivation = d; environment}
      | _ -> unreachable_ ()) in
    eval body_env body #{context = ghost_ bp.#context; ty = ghost_ bp.#ty;
      derivation = ghost_ bp.#derivation; environment = ghost_ bp.#environment}
  | D.Apply (fn, arg) ->
    let fp = ghost_ (match p.#derivation with
      | D.Application (a, d, _) ->
        #{context = p.#context; ty = D.Function (a, p.#ty);
          derivation = d; environment = p.#environment}
      | _ -> unreachable_ ()) in
    let f = eval env fn #{context = ghost_ fp.#context; ty = ghost_ fp.#ty;
      derivation = ghost_ fp.#derivation; environment = ghost_ fp.#environment} in
    let ap = ghost_ (match p.#derivation with
      | D.Application (a, _, d) ->
        #{context = p.#context; ty = a;
          derivation = d; environment = p.#environment}
      | _ -> unreachable_ ()) in
    let a = eval env arg #{context = ghost_ ap.#context; ty = ghost_ ap.#ty;
      derivation = ghost_ ap.#derivation; environment = ghost_ ap.#environment} in
    ghost_ (valid_def f.#evidence (Value (f.#value, fp.#ty)));
    match f.#value with
    | Closure (body, captured) ->
      let body_env = Bind (a.#value, captured) in
      let bp = ghost_ (match f.#evidence with
        | Capture (g, d, captured_proof) ->
          let context = D.Binding (D.Forall (D.Z, ap.#ty), g) in
          let environment = Extend (a.#evidence, captured_proof) in
          valid_def environment (Environment (body_env, context));
          #{context; ty = p.#ty; derivation = d; environment}
        | _ -> unreachable_ ()) in
      eval body_env body #{context = ghost_ bp.#context; ty = ghost_ bp.#ty;
      derivation = ghost_ bp.#derivation; environment = ghost_ bp.#environment}
    | Recursive_closure (body, captured) ->
      let self_env = Bind (f.#value, captured) in
      let body_env = Bind (a.#value, self_env) in
      let bp = ghost_ (match f.#evidence with
        | Capture (g, d, captured_proof) ->
          let self_context = D.Binding (D.Forall (D.Z, fp.#ty), g) in
          let self_proof = Extend (f.#evidence, captured_proof) in
          valid_def self_proof (Environment (self_env, self_context));
          let context = D.Binding (D.Forall (D.Z, ap.#ty), self_context) in
          let environment = Extend (a.#evidence, self_proof) in
          valid_def environment (Environment (body_env, context));
          #{context; ty = p.#ty; derivation = d; environment}
        | _ -> unreachable_ ()) in
      eval body_env body #{context = ghost_ bp.#context; ty = ghost_ bp.#ty;
      derivation = ghost_ bp.#derivation; environment = ghost_ bp.#environment}
    | True | Empty | Bind _ -> unreachable_ ()

type typing = #{ty : D.mono @@ ghost; derivation : D.typing @@ ghost}

let run : (term : D.term) @ immutable ->
    (p : {p : typing | D.typed D.Z D.Empty_context term p.#ty p.#derivation})
      @ immutable ->
    {r : result | valid r.#evidence (Value (r.#value, p.#ty))} @ immutable =
  fun term p ->
  ghost_ (Hm_interpreter_proofs.erase_typing D.Z D.Empty_context
    term p.#ty p.#derivation ();
    valid_def Leaf (Environment (Empty, D.Empty_context)));
  eval Empty term #{context = D.Empty_context; ty = p.#ty;
    derivation = p.#derivation; environment = Leaf}
