module D := Hm_declarative
module M := Level_mgu_spec

module Spec : sig
  (** Ghost witnesses; their representation is private. *)
  type evidence : immutable_data

  (** Evidence of the completed inference result, used by [principal]. *)
  val inferred : D.term @ immutable -> Copy_spec.ty option @ immutable ->
    evidence @ immutable -> bool @ ghost @@ total

  (** This heap and root unfold to [ty]. The evidence
      supplies the finite-tree witness required by Vox's quantifier-free logic. *)
  val represents : Pref.heap @ immutable -> Copy_spec.node Pref.t @ immutable ->
    Copy_spec.ty @ immutable -> evidence @ immutable -> bool @ ghost @@ total

  (** A declarative typing derivation for this input at this type. *)
  val has_type : D.term @ immutable -> Copy_spec.ty @ immutable ->
    evidence @ immutable -> bool @ ghost @@ total

  (** The erased declarative derivation, for clients such as a typed evaluator. *)
  val typing : (input : D.term) @ immutable ->
    (ty : Copy_spec.ty) @ immutable -> (proof : evidence) @ immutable ->
    {u : unit | has_type input ty proof} ->
    {d : D.typing | D.typed D.Z D.Empty_context input (D.embed ty) d}
      @ immutable ghost @@ total

end

type answer = #{root : Copy_spec.node Pref.t option @@ aliased;
  ownership : Pref.token; inferred_type : Copy_spec.ty option @@ ghost;
  evidence : Spec.evidence @@ ghost}

(** Infers a closed term. The returned graph represents [inferred_type], which
    types the input. The relation refers to the returned heap snapshot.
    Inference is partial; [None] means type rejection. *)
val infer : (input : {e : D.term | D.scoped_term D.Z e}) @ immutable ->
  {r : answer |
    Spec.inferred input r.#inferred_type r.#evidence
    && (match r.#root, r.#inferred_type with
        | None, None -> true
        | Some p, Some ty ->
            Spec.represents (Pref.own r.#ownership) p ty r.#evidence
            && Spec.has_type input ty r.#evidence
        | _ -> false)} @ unique

(** Every supplied valid typing is an instance of [inferred_type]. For [None],
    the supplied typing yields a contradiction. *)
val principal : (input : D.term) @ immutable ->
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
    {u : unit | claim})) @ total -> {u : unit | claim} @ ghost @@ total
