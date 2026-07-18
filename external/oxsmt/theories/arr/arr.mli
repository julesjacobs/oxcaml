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

(** A model value for the §8 array self-check ({!Oxsmt_interface.Array_model_check}):
    either a [Scalar] (an index/element leaf — [Int]/[Bool]/uninterpreted-element witness)
    or an [Array] as a finite index→element map ([entries], first-match) plus a [default]
    element for every unlisted index. Array equality is extensional: two [Array] values
    are equal iff their defaults are equal and they agree on the union of their listed
    indices. *)
type value =
  | Scalar of Model.value
  | Array of
      { entries : (value * value) list
      ; default : value
      }

(** [array_model t] is a candidate model — one {!value} per registered array-sorted term
    (a finite map built from the [select] terms on its e-class plus a base default) and
    one scalar per registered index/element leaf — valid after a [check Final] returned
    [Sat]. [None] if a needed value cannot be formed (fail-closed). The independent
    {!Oxsmt_interface.Array_model_check} evaluates every ORIGINAL assertion under it,
    computing [select]/[store]/equality itself, and the session reports [sat] only if all
    hold — so a satisfiable array query becomes a CHECKED sat, never a guessed one. *)
val array_model : t -> (Term.t * value) list option

(** Non-registering queries over the array theory's congruence closure, used by quantified
    lemma matching. See the corresponding accessors in {!Oxsmt_euf.Euf}. *)
val app_terms_by_symbol : t -> Symbol.t -> Term.t list
val find_class_opt : t -> Term.t -> int option
val equal_if_registered : t -> Term.t -> Term.t -> bool
val class_members : t -> Term.t -> Term.t list
val registered_terms : t -> Term.t list
val registered_terms_by_sort : t -> Sort.t -> Term.t list
