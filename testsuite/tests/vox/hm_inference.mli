module D := Hm_declarative
module M := Hm_inference_model

type result : immutable_data

val source : result @ immutable -> D.term @ immutable ghost @@ total
val inferred_type : result @ immutable -> Copy_spec.ty option @ immutable @@ total

val infer : (input : {e : D.term | D.scoped_term D.Z e}) @ immutable ->
  {out : result | source out === input} @ immutable

val sound : (out : result) @ immutable -> (ty : Copy_spec.ty) @ immutable ->
  {u : unit | inferred_type out === Some ty} ->
  {d : D.typing | D.typed D.Z D.Empty_context (source out) (D.embed ty) d}
  @ immutable ghost @@ total

val principal : (out : result) @ immutable ->
  (target : Copy_spec.ty) @ immutable -> (typing : D.typing) @ immutable ->
  {u : unit | D.typed D.Z D.Empty_context (source out) (D.embed target) typing} ->
  (claim : bool) ->
  (use : ((delta : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    {u : unit | match inferred_type out with
      | None -> false
      | Some ty -> target === M.substitute delta ty} -> {u : unit | claim})) @ total ->
  {u : unit | claim} @ ghost @@ total

val rejected : (out : result) @ immutable ->
  (target : Copy_spec.ty) @ immutable -> (typing : D.typing) @ immutable ->
  {u : unit | inferred_type out === None &&
    D.typed D.Z D.Empty_context (source out) (D.embed target) typing} ->
  {u : unit | false} @ ghost @@ total
