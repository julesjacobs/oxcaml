(** The obligatory in-process model self-check for the DATATYPES theory (GOALS Datatypes),
    the datatypes analogue of {!Model_check} (which speaks the UF {!Cdclt} vocabulary and
    cannot interpret constructor/selector/tester applications).

    [check registry model assertions] first validates that every model value INHABITS its
    term's declared sort — a [Bool] position holds a [Model.Bool] (a finite 2-element sort
    is never admitted through the unbounded [Uninterp] bucket, codex B1), a datatype
    position a [Ctor] naming a LEGAL constructor with recursively-inhabiting fields (codex
    B2) — then evaluates every ORIGINAL asserted term under the candidate constructor-tree
    [model] ({!Oxsmt_dt.Dt.check_model}, a [Term.t -> ctor_tree] assignment) with faithful
    datatype semantics and returns [true] iff all hold — so a passing model is a
    well-sorted genuine witness (satisfiability by definition, INDEPENDENTLY of the DT
    solving engine). Fail-closed: [false] on any sort-inhabitance violation / missing
    binding / underspecified value / type confusion / out-of-fragment term, so {!Session}
    degrades an un-self-checkable DT [sat] to [unknown]. [oxsmt_core] + the datatype shape
    registry + the [Dt.ctor_tree] type only; it does NOT consult the [Euf] engine, keeping
    the evaluation an independent re-derivation. A fail-closed witness/self-cert guard,
    never the verdict from a bricked check. ([And]/[Or]/[Ite] short-circuit, so an
    out-of-fragment term in a statically-dead branch is simply not reached — harmless, as
    a dead branch cannot change the verdict.) *)

open Oxsmt_core

val check
  :  Datatype_defs.t
  -> (Term.t * Oxsmt_dt.Dt.ctor_tree) list
  -> Term.t list
  -> bool
