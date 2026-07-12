(** A universally-quantified variable placeholder (ADR-0012 §1.1): a fresh nullary
    [.oxsmt.qvar.<lemma-id>.<index>] constant in the reserved namespace, wrapped so the
    caller can {e use} it (to build a lemma body / triggers through the session
    {!Oxsmt_core.Context}) but cannot {e forge} it.

    [t = private Term.t]: upward coercion to a raw {!Oxsmt_core.Term.t} is allowed (needed
    to build the body), but a [Qvar.t] cannot be {e constructed} outside this module —
    minting is the sole build path and is capability-gated at the {!Oxsmt_core.Env} layer.
    Coercion-escape (stash a [Qvar.t], coerce to [Term.t], assert it) is NOT prevented by
    the private alias; it is closed at the assert funnel by {!term_contains_qvar} (R1
    POINT 4). So the private alias is defense-in-depth; the assert-side gate is
    load-bearing. *)

open Oxsmt_core

type t = private Term.t

(** [to_term q] is the placeholder as an ordinary nullary-[App] {!Term.t}, for building a
    lemma body / triggers through the session {!Context}. *)
val to_term : t -> Term.t

(** [mint env ctx ~lemma_id ~index sort] mints the [index]-th placeholder of lemma
    [lemma_id] as a fresh nullary constant [.oxsmt.qvar.<lemma_id>.<index>] of [sort],
    interned in [env] and built through [ctx]. Distinct [(lemma_id, index)] pairs mint
    distinct placeholders, so no placeholder is ever reused across lemmas (I1/I2 hold by
    construction, capture is vacuous — ADR-0012 §3a).

    {b Phase B (env unfreeze):} the reserved namespace is closed to public declaration, so
    this will take the [Oxsmt_core.Env.reserved_cap] capability and mint via
    [Env.declare_reserved]. Until the frozen [env.mli] unfreeze lands, minting uses the
    current [Env.declare_fun] door (the session's public declaration guard already blocks
    [.oxsmt.*], so only the raw-[Env] forge door is open in the interim). *)
val mint : Env.t -> Context.t -> lemma_id:int -> index:int -> Sort.t -> t

(** The reserved qvar name prefix, [".oxsmt.qvar."]. *)
val prefix : string

(** [is_qvar_name name] recognizes a placeholder symbol name (the {!prefix}). *)
val is_qvar_name : string -> bool

(** [term_contains_qvar t] is [true] iff any [App] head in [t] is a placeholder symbol. An
    O(term) structural walk. This is the load-bearing assert-side gate (ADR-0012 §1.1, R1
    POINT 4): {!Session.assert_term} uses it to reject a user term carrying a coerced /
    interned placeholder (→ clean [Unknown]); {!Instance.of_subst} uses the same walk as
    an internal bug-catch (→ [Failure]). Distinct failure modes, one walk. *)
val term_contains_qvar : Term.t -> bool
