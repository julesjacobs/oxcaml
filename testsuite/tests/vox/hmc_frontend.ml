(* The frontend: from a closed source term to the top-level definitions and
   entry of Hmc_templates, each with a typing derivation.

   [prepare] checks scope, runs the verified inference ([V.infer]),
   elaborates its result into a derivation at the inferred type, grounds
   it (Hmc_grounding: the free type variables of the program's type are
   instantiated so that it becomes [word -> word], and the others at
   [bool]), checks the admitted fragment (Hmc_admission) and splits off the
   top-level [let]s ([T.extract]). Its result type says that a [Prepared]
   program is [T.ready], that is well typed with no free type variables, and
   rebuilds to the input term; it also gives the meaning of each rejection.

   The type errors keep the inference run as erased evidence. [untypable]
   and [no_entry_type] refute any typing of the term with the principality
   of that run ([V.principal_evidence]). *)
module D = Hm_declarative
module V = Verified_hm
module G = Hmc_grounding
module A = Hmc_admission
module T = Hmc_templates

(* Ghost record of an inference run, kept so that a rejection can be refuted. *)
type inference = {input : D.term @@ ghost; inferred : Copy_spec.ty option @@ ghost;
  evidence : V.Spec.evidence @@ ghost}

(* Inference found no type for [term]. *)
let[@def] (untyped @ total) (term : D.term @ immutable) (run : inference @ immutable) = ghost_ (
  run.input === term && V.Spec.inferred run.input run.inferred run.evidence && run.inferred === None)

(* The principal type of [term] has no instance [Word64 -> Word64]. *)
let[@def] (mistyped @ total) (term : D.term @ immutable) (run : inference @ immutable) = ghost_ (
  run.input === term && V.Spec.inferred run.input run.inferred run.evidence
  && match run.inferred with None -> false | Some ty -> not (G.entry_instance (D.embed ty)))

type result = Unbound_variable
  | Type_error of inference [@immediate_all_void_constructor]
  | Entry_type_mismatch of inference [@immediate_all_void_constructor]
  | Unsupported_fragment of A.error | Prepared of T.program [@@inductive]

let prepare : (term : D.term) @ immutable ->
    {r : result | match r with
      | Prepared p -> T.ready p && T.rebuild p.T.globals p.T.entry === term
      | Unbound_variable -> not (D.scoped_term D.Z term)
      | Type_error run -> untyped term run
      | Entry_type_mismatch run -> mistyped term run
      | Unsupported_fragment error -> A.meaning term error} @ immutable = fun term ->
  if not (D.scoped_term D.Z term) then Unbound_variable else
  let inferred = V.infer term in
  match V.elaborate term (borrow_ inferred) with
  | None ->
    let run = {input = ghost_ term; inferred = ghost_ inferred.#inferred_type;
      evidence = ghost_ inferred.#evidence} in
    ghost_ (untyped_def term run); Type_error run
  | Some checked -> match G.ground checked with
    | G.Entry_type_mismatch ->
      let run = {input = ghost_ term; inferred = ghost_ inferred.#inferred_type;
        evidence = ghost_ inferred.#evidence} in
      ghost_ (mistyped_def term run); Entry_type_mismatch run
    | G.Grounded grounded -> match A.admit grounded with
      | A.Rejected error -> Unsupported_fragment error
      | A.Admitted admitted -> Prepared (T.extract admitted)

(* A type-error rejection: no typing of [term] at any type. *)
let (untypable @ total) : (term : D.term) @ immutable -> (run : inference) @ immutable ->
    (target : Copy_spec.ty) @ immutable -> (typing : D.typing) @ immutable ->
    {u : unit | untyped term run && D.typed D.Z D.Empty_context term (D.embed target) typing} ->
    {u : unit | false} @ ghost =
  fun term run target typing premise -> ghost_ (
    untyped_def term run;
    let impossible : ((delta : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
      {u : unit | match run.inferred with None -> false | Some ty -> target === Level_mgu_spec.substitute delta ty} ->
      {u : unit | false}) @ total = fun _delta contradiction -> () in
    V.principal_evidence run.input run.inferred run.evidence target typing () false impossible)

let (entry_instance @ total) :
    (delta : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    (ty : Copy_spec.ty) @ immutable ->
    {u : unit | Level_mgu_spec.substitute delta ty === Copy_spec.Function (Copy_spec.Word64, Copy_spec.Word64)} ->
    {u : unit | G.entry_instance (D.embed ty)} @ ghost =
  fun delta ty premise -> ghost_ (
    Level_mgu_spec.substitute_def delta ty; D.embed_def ty; G.entry_instance_def (D.embed ty);
    match ty with
    | Copy_spec.Function (a, b) ->
      Level_mgu_spec.substitute_def delta a; Level_mgu_spec.substitute_def delta b;
      D.embed_def a; D.embed_def b; G.word_instance_def (D.embed a); G.word_instance_def (D.embed b)
    | _ -> ())

(* An entry-type rejection: no typing of [term] at [Word64 -> Word64]. *)
let (no_entry_type @ total) : (term : D.term) @ immutable -> (run : inference) @ immutable ->
    (typing : D.typing) @ immutable ->
    {u : unit | mistyped term run
      && D.typed D.Z D.Empty_context term (D.Function (D.Word64, D.Word64)) typing} ->
    {u : unit | false} @ ghost =
  fun term run typing premise -> ghost_ (
    mistyped_def term run;
    let target = Copy_spec.Function (Copy_spec.Word64, Copy_spec.Word64) in
    D.embed_def target; D.embed_def Copy_spec.Word64;
    let impossible : ((delta : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
      {u : unit | match run.inferred with None -> false | Some ty -> target === Level_mgu_spec.substitute delta ty} ->
      {u : unit | false}) @ total = fun delta instance ->
        match run.inferred with None -> () | Some ty -> entry_instance delta ty () in
    V.principal_evidence run.input run.inferred run.evidence target typing () false impossible)
