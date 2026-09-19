module D = Hm_declarative
module F = Level_finite_spec
module M = Level_mgu_spec
module H = Pref.Heap

type solution = {tree : F.tree; typing : D.typing}

type evidence = {
  execution : Hm_effective_execution_spec.execution;
  heap : Copy_spec.node Pref.heap;
  pool : Generalize_spec.pool;
  solution : solution option;
}

type certificate = {c : evidence |
  Hm_effective_execution_spec.ran (H.empty ()) 0 Generalize_spec.Empty
    Hm_environment_spec.Empty c.execution c.heap c.pool
  && (match Hm_effective_execution_spec.result c.execution, c.solution with
      | None, None -> true
      | Some p, Some s -> F.finite c.heap s.tree && F.tree_root s.tree === p
        && D.typed D.Z D.Empty_context
          (Hm_effective_execution_spec.source c.execution)
          (D.embed (F.readback s.tree)) s.typing
      | _ -> false)}

module Spec = struct
  type evidence = certificate

  let[@def] (inferred @ total) (input : D.term @ immutable)
      (ty : Copy_spec.ty option @ immutable) (proof : evidence @ immutable) =
    ghost_ (Hm_effective_execution_spec.source proof.execution === input
      && match ty, proof.solution with
        | None, None -> true
        | Some t, Some s -> F.readback s.tree === t
        | _ -> false)

  let[@def] (represents @ total) (heap : Copy_spec.node Pref.heap @ immutable)
      (root : Copy_spec.node Pref.t @ immutable) (ty : Copy_spec.ty @ immutable)
      (proof : evidence @ immutable) = ghost_ (
    match proof.solution with
    | None -> false
    | Some s -> F.finite heap s.tree && F.tree_root s.tree === root
        && F.readback s.tree === ty)

  let[@def] (has_type @ total) (input : D.term @ immutable)
      (ty : Copy_spec.ty @ immutable) (proof : evidence @ immutable) = ghost_ (
    match proof.solution with
    | None -> false
    | Some s -> D.typed D.Z D.Empty_context input (D.embed ty) s.typing)

  let (typing @ total) : (input : D.term) @ immutable ->
    (ty : Copy_spec.ty) @ immutable -> (proof : evidence) @ immutable ->
    {u : unit | has_type input ty proof} ->
    {d : D.typing | D.typed D.Z D.Empty_context input (D.embed ty) d}
      @ immutable ghost = fun input ty proof premise -> ghost_ (
      has_type_def input ty proof;
      match proof.solution with
      | None -> unreachable_ ()
      | Some s -> s.typing)

end

type answer = #{root : Copy_spec.node Pref.t option @@ aliased;
  ownership : Copy_spec.node Pref.token; inferred_type : Copy_spec.ty option @@ ghost;
  evidence : Spec.evidence @@ ghost}

let infer : (input : {e : D.term | D.scoped_term D.Z e}) @ immutable ->
  {r : answer |
    Spec.inferred input r.#inferred_type r.#evidence
    && (match r.#root, r.#inferred_type with
        | None, None -> true
        | Some p, Some ty ->
            Spec.represents (Pref.own r.#ownership) p ty r.#evidence
            && Spec.has_type input ty r.#evidence
        | _ -> false)} @ unique = fun input ->
  let out = Hm_routed_infer.closed_hm input in
  let certificate = ghost_ (
    let execution = out.#execution in
    let heap = Pref.own (borrow_ out.#state) in
    let pool = out.#pool in
    let evidence = match out.#value with
    | None ->
      {execution; heap; pool; solution = None}
    | Some p ->
      let tree = Hm_effective_forest.closed_forest execution heap pool p () in
      let empty = H.empty () in
      let trees : ((x : Copy_spec.node Pref.t) @ immutable ->
        {t : F.tree | F.tree_root t === x &&
          (if H.mem empty x then F.finite empty t
           else Level_unifier_spec.observe empty x === None)} @ immutable)
        @ total = fun x ->
        let t = F.Free x in F.tree_root_def t;
        Level_unifier_spec.observe_def empty x; t in
      Hm_effective_driver_proofs.run_result empty trees 0
        Generalize_spec.Empty Hm_environment_spec.Empty execution heap pool p ();
      let typing = Hm_effective_sound.closed_sound execution heap pool p tree () in
      {execution; heap; pool; solution = Some {tree; typing}} in
    let c : certificate = evidence in
    c) in
  let inferred_type = ghost_ (
    let ty = match certificate.solution with
      | None -> None | Some s -> Some (F.readback s.tree) in
    Spec.inferred_def input ty certificate;
    (match out.#value, ty with
     | Some p, Some t ->
       Spec.represents_def (Pref.own (borrow_ out.#state)) p t certificate;
       Spec.has_type_def input t certificate
     | _ -> ()); ty) in
  #{root = out.#value; ownership = out.#state; inferred_type; evidence = certificate}

let (principal @ total) : (input : D.term) @ immutable ->
  (answer : {r : answer |
    Spec.inferred input r.#inferred_type r.#evidence}) @ local immutable ->
  (target : Copy_spec.ty) @ immutable -> (typing : D.typing) @ immutable ->
  {u : unit | D.typed D.Z D.Empty_context input (D.embed target) typing} ->
  (claim : bool) ->
  (use : ((delta : (Copy_spec.node Pref.t @ immutable total ->
      Copy_spec.ty @ immutable total)) @ total ->
    {u : unit | match answer.#inferred_type with
      | None -> false
      | Some ty -> target === M.substitute delta ty} ->
    {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun input answer target typing premise claim use -> ghost_ (
    let proof = answer.#evidence in
    Spec.inferred_def input answer.#inferred_type proof;
    match Hm_effective_execution_spec.result proof.execution, proof.solution with
    | None, _ ->
      Hm_effective_complete.closed_reject proof.execution proof.heap proof.pool
        target typing (); ()
    | Some p, Some s ->
      let accept : ((delta : (Copy_spec.node Pref.t @ immutable total ->
          Copy_spec.ty @ immutable total)) @ total ->
        {u : unit | target === M.substitute delta (F.readback s.tree)} ->
        {u : unit | claim}) @ total = fun delta _factor -> use delta () in
      Hm_effective_complete.closed_factor proof.execution proof.heap proof.pool
        p s.tree target typing () claim accept
    | Some _, None -> let _impossible : {u : unit | false} = () in ())
