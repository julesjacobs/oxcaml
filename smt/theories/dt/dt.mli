(** The datatypes theory, built as an {b e-graph client} (GOALS Datatypes; the e-graph
    architecture's acceptance test — no change to the SAT core).

    Algebraic-datatype terms are ordinary {!Oxsmt_core.Term} [App] nodes over
    constructor/selector/tester symbols (the core is frozen; see {!Datatype}). Congruence
    over those nodes is exactly what the {!Oxsmt_euf.Euf} engine already does, so this
    theory {e owns an [Euf.t] instance as its e-graph substrate} and layers the four
    datatype axioms on top of it:

    - {b distinctness} — two different constructors in one e-class conflict;
    - {b injectivity} — the same constructor in one e-class propagates field equalities;
    - {b selector evaluation} — [sel_i (C a₁ … aₙ)] evaluates to [aᵢ] once the class of
      the argument is known to be a [C];
    - {b acyclicity} — a term that is a constructor-descendant of itself (e.g.
      [x = cons (h, x)]) is unsat (the occurs check).

    Constructor case splits enter as SAT decisions (a multi-literal, non-tautological
    exhaustiveness clause [is-C₁ x ∨ … ∨ is-Cₖ x] over the sort's constructors), per the
    DESIGN A2 erratum — only {e entailed} facts ride the e-graph. This decides enum
    problems: four pairwise-distinct values of a three-constructor type is unsat.

    Presented to the CDCL(T) seam ({!Cdclt}) as a standalone {!Oxsmt_core.Theory.THEORY}
    (its [create] additionally takes the {!Datatype} registry). Every unsat path emits
    certificate events through the SAT trace seam (a theory conflict becomes a theory
    clause); full Lean replay of the DT rules is a named follow-up. Stdlib-only over
    [oxsmt_core] + [oxsmt_euf] (dependency firewall I3). *)

open Oxsmt_core

type t

(** [create ctx env registry] is an empty DT theory over session [ctx]/[env], reading
    datatype structure from the LIVE [registry] ref ({!Oxsmt_core.Datatype_defs}) — held
    by reference, not snapshotted, so datatypes declared after this theory is instantiated
    (batched queries in one session) are visible. With an empty [registry] the theory is
    inert (pure congruence closure), so a non-datatype problem is unaffected. *)
val create : Context.t -> Env.t -> Datatype_defs.t ref -> t

(* The frozen {!Oxsmt_core.Theory.THEORY} operations, driven by the CDCL(T) seam. *)

val register_atom : t -> Atom.t -> Term.t -> unit
val assert_lit : t -> Lit.t -> unit
val check : t -> Theory.effort -> Theory.check_result
val explain : t -> Lit.t -> Explanation.t
val push : t -> unit
val pop : t -> int -> unit
val model : t -> Model.t

(** [internalize_term t term] internalises [term] and its subterm closure into the DT
    theory's e-graph WITHOUT binding it to an atom — never asserted, watched, propagated,
    or explained; only made visible to congruence + the DT saturation cataloguer (so
    selector-evaluation fires on a selector application, e.g. [key t], that surfaces only
    inside the arithmetic child's atom [(> (key t) 0)]) and to the model enumeration. The
    Nelson–Oppen combinator ({!Oxsmt_combine.Combine.CONGRUENCE_CHILD}) uses this to make
    the DT congruence child see a boundary term of the OTHER (arithmetic) child.
    Idempotent; undone by [pop] of the introducing frame (the engine's internalisation is
    trailed). *)
val internalize_term : t -> Term.t -> unit

(** [constructor_clash_for_premises t premises] returns two applications of distinct
    constructors of the same datatype when they are equal in [t]'s current congruence
    closure and the engine's explanation for that equality is exactly [premises] after the
    same deduplication used by {!check}. This is a conservative certificate query: it
    returns [None] for every other datatype conflict, including injectivity and
    acyclicity. It does not mutate the theory. *)
val constructor_clash_for_premises : t -> Lit.t list -> (Term.t * Term.t) option

(** A model value for a datatype term, as a constructor tree the §8 self-check evaluator
    checks against the formula (GOALS: "a model with actual constructor trees"). A leaf is
    a non-datatype value (an [Int]/[Bool]/uninterpreted witness) or a nullary constructor;
    an internal node is [Ctor (name, fields)]. *)
type ctor_tree =
  | Ctor of string * ctor_tree list
  | Leaf of Model.value

(** [constructor_model t] is a constructor tree for every registered datatype-sorted term,
    valid after a [check Final] returned [Sat]. Terms in one e-class share a tree; an
    unconstrained class is given its sort's first terminating constructor (so recursive
    sorts get a finite witness). [None] if a needed value is missing (fail-closed). *)
val constructor_model : t -> (Term.t * ctor_tree) list option

(** [check_model t] is the full candidate model the §8 DT self-check evaluates against the
    formula: a [Term.t -> ctor_tree] assignment for every registered datatype term (via
    {!constructor_model}, so datatype variables, nested fields, and underspecified
    selector terms all resolve) UNIONED with a [Leaf] scalar for every registered
    non-datatype atomic (nullary [App]) subterm (Int/Bool/uninterpreted-sort variable).
    Compound terms are omitted — the evaluator computes them structurally. [None] iff
    {!constructor_model} degrades (fail-closed). Valid after a [check Final] returned
    [Sat]. *)
val check_model : t -> (Term.t * ctor_tree) list option

(** [check_model_with_leaf t override] is {!check_model} with an external scalar-leaf
    override: for a non-datatype leaf term [x], the model value is [override x] when it
    returns [Some], else the theory's own per-class default. The Nelson–Oppen combined
    DT+LIA path ({!Oxsmt_interface.Cdclt}) passes the merged model's Int values as the
    override, so the constructor tree's Int fields (and Int scalar leaves) carry the
    ARITHMETIC child's values rather than the pure-DT default (which has no arithmetic and
    would give [0]) — the reconciliation the mixed-sat self-check
    ({!Oxsmt_interface.Dt_model_check}) needs. {!check_model} is exactly this with a
    constant-[None] override. *)
val check_model_with_leaf
  :  t
  -> (Term.t -> Model.value option)
  -> (Term.t * ctor_tree) list option

(** Non-registering queries over the DT theory's congruence closure, used by quantified
    lemma matching. See the corresponding accessors in {!Oxsmt_euf.Euf}. *)
val app_terms_by_symbol : t -> Symbol.t -> Term.t list

val find_class_opt : t -> Term.t -> int option
val equal_if_registered : t -> Term.t -> Term.t -> bool
val class_members : t -> Term.t -> Term.t list
val registered_terms : t -> Term.t list
val registered_terms_by_sort : t -> Sort.t -> Term.t list

(** {2 ADR-0014 fabric seam (DARK).}

    The rich, non-frozen child surface the Nelson–Oppen combinator drives when the DT
    theory is on the e-graph fabric — the concrete counterpart of
    {!Oxsmt_combine.Combine.FABRIC_CONGRUENCE_CHILD}. In Stage B this machinery EXISTS and
    is trail-correct but is never driven by the default solver path
    ([Oxsmt_combine.Dtlia_router.fabric_disabled] is compile-time [true]): the classic
    {!check}/{!explain} path above is byte-identical to before. All results are in the
    {!Oxsmt_core.Fabric} currency so a hub-injected equality's edge handle is preserved. *)

(** {!check} in fabric currency: same reasoning, conflicts carry {!Fabric.justification}s. *)
val check_fabric : t -> Theory.effort -> Fabric.check_result

(** {!explain} in fabric currency (served from the snapshotted propagation cache). *)
val explain_fabric : t -> Lit.t -> Fabric.Explanation.t

(** The congruence child fixes no arithmetic value: always [None]. *)
val fixed_bounds : t -> Term.t -> Fabric.fixed_bounds option

(** The congruence child is never the fixed-value witness: always [false]. *)
val fabric_verify
  :  t
  -> Term.t
  -> string
  -> Fabric.justification
  -> Fabric.justification
  -> bool

(** A hub [new_eq] reaction: no-op (the congruence hub reacts to no notifications). *)
val notify_eq : t -> edge_id:Fabric.edge_id -> Term.t -> unit

(** OXSMT_LIA_MODELFIND disequality hint: ignored (the DT child does not model-find). *)
val note_disequalities : t -> (Term.t * Term.t) list -> unit

(** ADR-0014 Stage 4.2 sub-frame checkpoint/rewind (chrono earliest-removed incremental
    undo). Captures the engine checkpoint plus the frame/diseq-frame lengths; the rewind
    restores the engine and drops the reasons/diseq-pairs added since. *)
type checkpoint

val checkpoint : t -> checkpoint
val rewind_to_checkpoint : t -> checkpoint -> unit

(** Read-only congruence-equality query (the combinator asks before injecting/explaining a
    fabric equality). *)
val fabric_are_equal : t -> Term.t -> Term.t -> bool

(** Assert a hub-injected equality attributed to fabric [edge_id]; the edge is preserved
    as a {!Fabric.justification} through every downstream explanation. *)
val assert_fabric_eq : t -> edge_id:Fabric.edge_id -> Term.t -> Term.t -> unit

(** ADR-0014 Stage 2/3 multi-consumer merge log, per-class tag slot, and the
    congruence-explanation accessor for the [new_eq] justification. *)
type merge_cursor

val set_record_merges : t -> bool -> unit
val add_merge_consumer : t -> merge_cursor
val drain_merges : t -> merge_cursor -> Fabric.merge_event list
val set_class_tag : t -> Term.t -> Term.t -> unit
val class_tag : t -> Term.t -> Term.t option
val fabric_explain_eq : t -> Term.t -> Term.t -> Fabric.justification list
