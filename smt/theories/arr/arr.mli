(** The arrays theory (QF_AX: functional, extensional arrays), built as an
    {b e-graph client} over {!Oxsmt_euf.Euf} — no change to the SAT core, mirroring the
    datatypes theory {!Oxsmt_dt.Dt}.

    [select]/[store] terms are ordinary {!Oxsmt_core.Term} [App] nodes over the operator
    symbols the parser mints per (index, element) instantiation (recorded in
    {!Oxsmt_core.Array_defs}); congruence over those nodes is exactly what the {!Euf}
    engine already gives, so this theory {e owns an [Euf.t] instance} and layers the array
    axioms on top:

    - {b read-over-write (ROW)}: [select (store a i v) j] equals [v] when [i = j] and
      [select a j] when [i <> j]. When the index relation is entailed the equality is
      propagated; when it is open a {b lazy} case split
      [i = j ∨ select (store a i v) j = select a j] is emitted (theory-valid, so a
      refutation using it is sound). Congruence handles [store] equality directly (e.g.
      [store a i v <> store a i w] with [v = w] conflicts).
    - {b extensionality}: an asserted array {e disequality} [a <> b] introduces a fresh
      witness index [k] and the disequality [select a k <> select b k] (a sound
      Skolemization of "[a] and [b] differ somewhere"), which drives ROW to a refutation.

    {b Soundness posture (v1).} Every rule adds only theory-valid consequences, so every
    [unsat] is sound. A [Final]→[Sat] on an array problem is deliberately
    {e degraded to [unknown]} by the session ("no [sat] on arrays in v1"): the
    ROW/extensionality saturation is complete enough for refutation but the model is not
    self-checked, so reporting [sat] is withheld rather than risk a wrong-[sat].
    Splits/witnesses are bounded by the engine's split budget (→ [unknown]) so a
    pathological input never hangs.

    Presented to the CDCL(T) seam as a standalone {!Oxsmt_core.Theory.THEORY}; its
    [create] additionally takes the {!Array_defs} registry. Stdlib-only over
    [oxsmt_core] + [oxsmt_euf] (dependency firewall I3). *)

open Oxsmt_core

type t

(** [create ctx env cap registry] is an empty arrays theory over session [ctx]/[env],
    reading the [select]/[store] symbol classification from [registry] ({!Array_defs}).
    With an empty [registry] the theory is inert pure congruence closure, so a non-array
    problem is unaffected. [env] is retained so the theory can mint the [select] symbol it
    needs mid-solve; [cap] is the session's ADR-0012 R1 reserved-minting capability,
    required because the extensionality rule mints FRESH witness index constants in the
    reserved [".oxsmt.*"] namespace (unforgeable — a user-nameable witness would be a
    wrong-UNSAT vector). *)
val create : Context.t -> Env.t -> Env.reserved_cap -> Array_defs.t -> t

(* The frozen {!Oxsmt_core.Theory.THEORY} operations, driven by the CDCL(T) seam. *)

val register_atom : t -> Atom.t -> Term.t -> unit
val assert_lit : t -> Lit.t -> unit
val check : t -> Theory.effort -> Theory.check_result
val explain : t -> Lit.t -> Explanation.t
val push : t -> unit
val pop : t -> int -> unit
val model : t -> Model.t
