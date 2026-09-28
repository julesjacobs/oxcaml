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

  let[@def] (trace_at @ total) (heap : Copy_spec.node Pref.heap @ immutable)
      (trace : Hm_annotation_trace.trace @ immutable)
      (proof : evidence @ immutable) = ghost_ (
    heap === proof.heap
    && Hm_annotation_trace_spec.records trace proof.execution)

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

type answer = #{root : Copy_spec.node Pref.t option @@ global;
  ownership : Copy_spec.node Pref.token;
  trace : Hm_annotation_trace.trace @@ global; inferred_type : Copy_spec.ty option @@ global ghost;
  evidence : Spec.evidence @@ global ghost}

let infer_with_trace : (collect_trace : bool) -> (input : {e : D.term | D.scoped_term D.Z e}) @ immutable ->
  {r : answer |
    Spec.inferred input r.#inferred_type r.#evidence
    && (if collect_trace then Spec.trace_at (Pref.own r.#ownership) r.#trace r.#evidence
        else r.#trace === Hm_annotation_trace.Failed)
    && (match r.#root, r.#inferred_type with
        | None, None -> true
        | Some p, Some ty ->
            Spec.represents (Pref.own r.#ownership) p ty r.#evidence
            && Spec.has_type input ty r.#evidence
        | _ -> false)} @ unique = fun collect_trace input ->
  let out = Hm_routed_infer.closed_hm_with_trace collect_trace input in
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
    Spec.trace_at_def (Pref.own (borrow_ out.#state)) out.#trace certificate;
    (match out.#value, ty with
     | Some p, Some t ->
       Spec.represents_def (Pref.own (borrow_ out.#state)) p t certificate;
       Spec.has_type_def input t certificate
     | _ -> ()); ty) in
  #{root = out.#value; ownership = out.#state; trace = out.#trace;
    inferred_type; evidence = certificate}

let infer : (input : {e : D.term | D.scoped_term D.Z e}) @ immutable ->
  {r : answer |
    Spec.inferred input r.#inferred_type r.#evidence
    && Spec.trace_at (Pref.own r.#ownership) r.#trace r.#evidence
    && (match r.#root, r.#inferred_type with
        | None, None -> true
        | Some p, Some ty ->
            Spec.represents (Pref.own r.#ownership) p ty r.#evidence
            && Spec.has_type input ty r.#evidence
        | _ -> false)} @ unique = fun input -> infer_with_trace true input

let infer_type : (input : {e : D.term | D.scoped_term D.Z e}) @ immutable ->
  {r : answer |
    Spec.inferred input r.#inferred_type r.#evidence
    && r.#trace === Hm_annotation_trace.Failed
    && (match r.#root, r.#inferred_type with
        | None, None -> true
        | Some p, Some ty ->
            Spec.represents (Pref.own r.#ownership) p ty r.#evidence
            && Spec.has_type input ty r.#evidence
        | _ -> false)} @ unique = fun input -> infer_with_trace false input

let (principal_evidence @ total) : (input : D.term) @ immutable ->
  (inferred_type : Copy_spec.ty option) @ immutable ->
  (evidence : {e : Spec.evidence | Spec.inferred input inferred_type e}) @ immutable ->
  (target : Copy_spec.ty) @ immutable -> (typing : D.typing) @ immutable ->
  {u : unit | D.typed D.Z D.Empty_context input (D.embed target) typing} ->
  (claim : bool) ->
  (use : ((delta : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    {u : unit | match inferred_type with None -> false | Some ty -> target === M.substitute delta ty} ->
    {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun input inferred_type evidence target typing premise claim use -> ghost_ (
    let proof = evidence in
    Spec.inferred_def input inferred_type proof;
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
    principal_evidence input answer.#inferred_type answer.#evidence target typing premise claim use)

let read_type :
  (answer : {r : answer |
    match r.#root, r.#inferred_type with
    | None, None -> true
    | Some p, Some ty ->
      Spec.represents (Pref.own r.#ownership) p ty r.#evidence
    | _ -> false}) @ local read ->
  {ty : Copy_spec.ty option | ty === answer.#inferred_type} @ immutable = fun answer ->
  match answer.#root with
  | None -> None
  | Some p ->
    let tree = ghost_ (
      match answer.#inferred_type with
      | None -> unreachable_ ()
      | Some ty ->
        let proof = answer.#evidence in
        Spec.represents_def (Pref.own (borrow_ answer.#ownership)) p ty proof;
        match proof.solution with
        | None -> unreachable_ ()
        | Some s -> s.tree) in
    Some (Hm_readback_runtime.read tree p (borrow_ answer.#ownership))

let elaborate :
  (input : D.term) @ immutable ->
  (answer : {r : answer |
    Spec.inferred input r.#inferred_type r.#evidence
    && Spec.trace_at (Pref.own r.#ownership) r.#trace r.#evidence
    && (match r.#root, r.#inferred_type with
      | None, None -> true
      | Some p, Some ty ->
        Spec.represents (Pref.own r.#ownership) p ty r.#evidence
      | _ -> false)}) @ local read ->
  {r : Hm_checked_elaboration.t option | match r with
    | None -> answer.#inferred_type === None
    | Some checked -> Hm_checked_elaboration.source checked === input
      && (match answer.#inferred_type with
        | None -> false
        | Some ty -> Hm_checked_elaboration.root checked === D.embed ty)}
  @ immutable =
  let initial_tree : ((p : Copy_spec.node Pref.t) @ immutable ->
      {t : F.tree | F.tree_root t === p &&
        (if H.mem (H.empty ()) p then F.finite (H.empty ()) t
         else Level_unifier_spec.observe (H.empty ()) p === None)} @ immutable) @ total ghost = ghost_ (
    fun p ->
      let t = F.Free p in F.tree_root_def t;
      Level_unifier_spec.observe_def (H.empty ()) p; t) in
  let tree : ((proof : Spec.evidence) @ immutable ->
      (p : Copy_spec.node Pref.t) @ immutable ->
      {t : F.tree | F.tree_root t === p &&
        (if H.mem proof.heap p then F.finite proof.heap t
         else Level_unifier_spec.observe proof.heap p === None)} @ immutable) @ total ghost = ghost_ (
    fun proof p ->
      Hm_effective_forest.closed_forest proof.execution proof.heap proof.pool p ()) in
  let prepare : ((input : D.term) @ immutable ->
      (ty : Copy_spec.ty) @ immutable -> (proof : Spec.evidence) @ immutable ->
      (heap : Copy_spec.node Pref.heap) @ immutable ->
      (trace : Hm_annotation_trace.trace) @ immutable ->
      (trees : ((p : Copy_spec.node Pref.t) @ immutable ->
        {t : F.tree | F.tree_root t === p &&
          (if H.mem heap p then F.finite heap t
           else Level_unifier_spec.observe heap p === None)} @ immutable)) @ total ->
      (rho : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
      (values : ((p : Copy_spec.node Pref.t) @ immutable ->
        {u : unit | rho p === F.readback (trees p)})) @ total ->
      {u : unit | heap === proof.heap && Spec.inferred input (Some ty) proof
        && Hm_annotation_trace_spec.records trace proof.execution} ->
      {u : unit | Hm_annotation_trace_spec.owned heap trace
        && Hm_annotation_equations.annotates input trace rho ty
        && Hm_reconstruction_instances.satisfied rho Hm_elaboration.Empty D.Empty_context input trace
        && D.scoped_term D.Z input}) @ total ghost = ghost_ (
    fun input ty proof heap trace trees rho values premise ->
      let empty = H.empty () in
      let initial : ((p : Copy_spec.node Pref.t) @ immutable ->
        {t : F.tree | F.tree_root t === p &&
          (if H.mem empty p then F.finite empty t
           else Level_unifier_spec.observe empty p === None)} @ immutable) @ total = fun p -> initial_tree p in
      Spec.inferred_def input (Some ty) proof;
      Hm_annotation_owned.run empty initial 0 Generalize_spec.Empty Hm_environment_spec.Empty
        proof.execution heap proof.pool trace ();
      Hm_annotation_shape.successful empty 0 Generalize_spec.Empty Hm_environment_spec.Empty
        proof.execution heap proof.pool trace ();
      Hm_annotation_equations.readback empty initial 0 Generalize_spec.Empty Hm_environment_spec.Empty
        proof.execution heap proof.pool trees rho values trace ();
      Hm_reconstruction_run.closed proof.execution heap proof.pool trees rho values trace ();
      Hm_annotation_trace_spec.root_agrees trace proof.execution ();
      (match proof.solution with
      | None -> unreachable_ ()
      | Some solution ->
        Hm_type_proofs.typing_scoped D.Z D.Empty_context input (D.embed ty) solution.typing ();
        D.depth_def D.Empty_context;
        F.finite_def heap solution.tree;
        let p = F.tree_root solution.tree in
        let actual = trees p in
        Level_finite_proofs.finite_unique heap solution.tree actual ();
        values p);
      Hm_annotation_equations.annotates_def input trace rho ty) in
  fun input answer ->
  match read_type answer with
  | None -> None
  | Some ty ->
    let proof = ghost_ answer.#evidence in
    let trace = answer.#trace in
    let heap = ghost_ proof.heap in
    let snapshot_trees : ((p : Copy_spec.node Pref.t) @ immutable ->
        {t : F.tree | F.tree_root t === p &&
          (if H.mem heap p then F.finite heap t
           else Level_unifier_spec.observe heap p === None)} @ immutable) @ total ghost =
      ghost_ (fun p -> tree proof p) in
    let[@def] rho : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total) @ total ghost =
      ghost_ (fun p -> F.readback (snapshot_trees p)) in
    ghost_ (
      Spec.trace_at_def (Pref.own (borrow_ answer.#ownership)) trace proof;
      prepare input ty proof heap trace snapshot_trees rho (fun p -> rho_def p) ());
    let snapshot = Hm_annotation_snapshot.collect heap snapshot_trees
      trace (borrow_ answer.#ownership) Hm_annotation_snapshot.Empty in
    let (read @ total) : (p : {p : Copy_spec.node Pref.t | Hm_annotation_trace.contains p trace}) @ immutable ->
        {ty : Copy_spec.ty | ty === rho p} @ immutable = fun p ->
      ghost_ (Hm_annotation_snapshot.record_lookup heap snapshot_trees p trace Hm_annotation_snapshot.Empty; rho_def p);
      match Hm_annotation_snapshot.lookup p snapshot with
      | Some ty -> ty
      | None -> unreachable_ () in
    ghost_ (Hm_annotation_trace.subtrace_refl trace;
      Hm_elaboration.depth_def Hm_elaboration.Empty; D.depth_def D.Empty_context;
      D.context_wf_def D.Z D.Empty_context; Hm_elaboration.interpret_empty ty);
    let derivation = Hm_typed_elaboration.build trace rho Hm_elaboration.Empty D.Empty_context
        input trace ty read () in
    Some (Hm_checked_elaboration.of_derivation input (D.embed ty) derivation ())
