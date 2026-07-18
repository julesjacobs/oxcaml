(** A ground instance of a lemma (ADR-0012 §1.1): [φ[σ]], the lemma body with each
    placeholder qvar replaced by a ground term. [t = private Term.t]: an [Instance.t] can
    only be minted by {!of_subst}, which re-checks that no placeholder survives the
    substitution — so a value of this type is, by construction, a ground Bool term with no
    residual qvar. The private [Session.assert_term_at_frame] takes an [Instance.t] (not a
    raw [Term.t]) precisely so the assert-side gate cannot be bypassed (R2 / codex POINT
    6). *)

open Oxsmt_core

type t = private Term.t

(** [to_term i] is the instance as an ordinary ground {!Term.t}. *)
val to_term : t -> Term.t

(** [of_subst ctx ~qvars ~body sigma] is [body] with each placeholder [qvars.(k)] replaced
    by the ground term [sigma.(k)], rebuilt bottom-up through [ctx]'s smart constructors
    (capture-free — placeholders are distinct fresh constants, ADR-0012 §1.1). [sigma]
    must have the same length as [qvars] ([Invalid_argument] otherwise) and each
    [sigma.(k)] must be ground (contain no qvar).

    The result is re-checked for any residual [.oxsmt.qvar.*]: because the manager only
    ever feeds ground substitutions, a residual is an
    {b internal invariant violation = a bug}, so this raises [Failure] (a loud bug-catch,
    NOT the user-facing clean-[Unknown] path — that is {!Session.assert_term}'s gate over
    the same walk, ADR-0012 §1.1). *)
val of_subst : Context.t -> qvars:Qvar.t array -> body:Term.t -> Term.t array -> t
