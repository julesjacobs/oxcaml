open Copy_spec
open Generalize_spec
open Hm_effective_execution_spec
open Level_finite_spec
module H := Pref.Heap
module D := Hm_declarative

(** For a successful run from the empty heap and environment, constructs a
    declarative typing derivation at the returned graph's finite readback type. *)
val closed_sound :
  (e : execution) @ immutable -> (after : Pref.heap) @ immutable ->
    (pool : pool) @ immutable -> (p : node Pref.t) @ immutable -> (t : tree) @ immutable
      ->
    {u : unit | ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty e after
      pool
      && result e === Some p && finite after t && tree_root t === p} ->
    {d : D.typing | D.typed D.Z D.Empty_context (source e) (D.embed (readback t)) d} @
      immutable ghost
  @@ total

(** For a successful run from the empty heap and environment, passes [use] a
    typing derivation at the finite readback type and a substitution producing
    the supplied declaratively valid [target] type. *)
val closed_principal :
  (e : execution) @ immutable -> (after : Pref.heap) @ immutable ->
    (pool : pool) @ immutable -> (p : node Pref.t) @ immutable -> (tree : tree) @
      immutable ->
    (target : ty) @ immutable -> (typing : D.typing) @ immutable ->
    {u : unit | ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty e after
      pool
      && result e === Some p && finite after tree && tree_root tree === p
      && D.typed D.Z D.Empty_context (source e) (D.embed target) typing} -> (claim : bool)
        ->
    (use : ((inferred : D.typing) @ immutable ->
      (delta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | D.typed D.Z D.Empty_context (source e) (D.embed (readback tree))
        inferred
        && target === Level_mgu_spec.substitute delta (readback tree)} -> {u : unit |
          claim})) @ total ->
    {u : unit | claim} @ ghost
  @@ total
