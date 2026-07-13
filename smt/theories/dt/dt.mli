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
    datatype structure from [registry] ({!Oxsmt_core.Datatype_defs}). With an empty
    [registry] the theory is inert (pure congruence closure), so a non-datatype problem is
    unaffected. *)
val create : Context.t -> Env.t -> Datatype_defs.t -> t

(* The frozen {!Oxsmt_core.Theory.THEORY} operations, driven by the CDCL(T) seam. *)

val register_atom : t -> Atom.t -> Term.t -> unit
val assert_lit : t -> Lit.t -> unit
val check : t -> Theory.effort -> Theory.check_result
val explain : t -> Lit.t -> Explanation.t
val push : t -> unit
val pop : t -> int -> unit
val model : t -> Model.t

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
