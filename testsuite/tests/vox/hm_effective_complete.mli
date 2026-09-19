open Copy_spec
open Generalize_spec
open Hm_effective_execution_spec
open Level_finite_spec
module H := Pref.Heap
module D := Hm_declarative

(** A completed run from the empty heap and environment succeeds if its source
    has the supplied declarative typing derivation. Assumes the run terminates. *)
val closed_completes :
  (e : execution) @ immutable -> (after : node Pref.heap) @ immutable ->
    (pool : pool) @ immutable -> (target : ty) @ immutable -> (d : D.typing) @ immutable
      ->
    {u : unit | ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty e after
      pool
      && D.typed D.Z D.Empty_context (source e) (D.embed target) d} ->
    {u : unit | not (result e === None)} @ ghost
  @@ total

(** A failed run from the empty heap and environment contradicts any supplied
    declarative typing derivation for its source at [target]. *)
val closed_reject :
  (e : execution) @ immutable -> (after : node Pref.heap) @ immutable ->
    (pool : pool) @ immutable -> (target : ty) @ immutable -> (d : D.typing) @ immutable
      ->
    {u : unit | ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty e after
      pool
      && result e === None && D.typed D.Z D.Empty_context (source e) (D.embed target) d}
        ->
    {u : unit | false} @ ghost
  @@ total

(** For a successful run from the empty heap and environment, passes [use] a
    substitution taking the returned graph's finite readback type to the
    supplied declaratively valid [target] type. *)
val closed_factor :
  (e : execution) @ immutable -> (after : node Pref.heap) @ immutable ->
    (pool : pool) @ immutable -> (p : node Pref.t) @ immutable -> (tree :
      Level_finite_spec.tree) @ immutable ->
    (target : ty) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty e after
      pool
      && result e === Some p && Level_finite_spec.finite after tree &&
        Level_finite_spec.tree_root tree === p
      && D.typed D.Z D.Empty_context (source e) (D.embed target) d} -> (claim : bool) ->
    (use : ((delta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | target === Level_mgu_spec.substitute delta (Level_finite_spec.readback
        tree)} ->
      {u : unit | claim})) @ total -> {u : unit | claim} @ ghost
  @@ total
