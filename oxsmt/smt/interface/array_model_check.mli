(** The obligatory in-process model self-check for the ARRAYS theory (QF_AX), the arrays
    analogue of {!Dt_model_check}.

    [check registry model assertions] first validates that every model value INHABITS its
    term's declared sort (an [Array] position holds an [Array] whose keys/values
    recursively inhabit the index/element sorts; a [Bool] a [Model.Bool]; etc.), then
    evaluates every ORIGINAL asserted term under the candidate [model]
    ({!Oxsmt_arr.Arr.array_model}) with faithful array semantics — [select] reads a finite
    map (first match) or the default, [store] overlays, array equality is extensional —
    and returns [true] iff all hold. A passing model is a well-sorted genuine witness
    (satisfiability by definition, INDEPENDENTLY of the arrays solving engine).
    Fail-closed: [false] on any inhabitance violation / missing binding / type confusion /
    out-of-fragment term (arithmetic, an applied uninterpreted function), so {!Session}
    degrades an un-self-checkable array [sat] to [unknown], never a wrong sat.
    [oxsmt_core] + the [Array_defs] shape + the [Arr.value] type only; it does NOT consult
    the [Euf] engine, keeping the evaluation an independent re-derivation. *)

open Oxsmt_core

val check : Array_defs.t -> (Term.t * Oxsmt_arr.Arr.value) list -> Term.t list -> bool
