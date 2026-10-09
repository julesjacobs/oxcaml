(** The uniform reason currency (DESIGN.md §7; INVARIANTS.md I4): every derived fact — a
    theory propagation or a theory conflict — is justified by a
    {b premise set + rule tag}. The engine turns a conflict's premises into the learned
    clause and resolves them against the trail for 1UIP and selector-based unsat cores
    (§7).

    Frozen at the M1 THEORY freeze (ADR-0005 Tranche A). Pure signature — no
    implementation module. *)

(** A certificate-shaped classifier of a derived fact. {b Payload-free, permanently}
    (ADR-0006): the M5 certificate witnesses (Farkas vectors, congruence chains) live in
    the off-core [smt/certificate/] module, never as a tag payload — this keeps LIA's
    rational type off the frozen core on the hot 1UIP path (I3). A future theory may add
    {e new constructors} (e.g. datatype rules); that is an additive enum unfreeze,
    orthogonal to the no-payload rule. *)
module Rule_tag : sig
  type t =
    | Trivial (** a tautology / constant-folded fact *)
    | Euf_congruence (** EUF: a proof-forest transitivity + congruence chain *)
    | Lia_bound (** LIA: a simplex bound propagation *)
    | Lia_farkas (** LIA: an infeasible row, Farkas-certified *)
    | Lia_branch (** LIA: a branch-and-bound case split *)
    | Shared_eq
    (** Nelson–Oppen: an equality entailed in one theory, replayed in another *)
end

(** The premises are asserted theory literals currently true on the trail; their
    conjunction T-entails the explained fact (is T-unsat, for a conflict). For a
    propagated literal they are {b precedence-valid} (ADR-0005 CONTRACT-EX: each assigned
    strictly before the propagated literal) and in deterministic order (C2). *)
type t =
  { premises : Lit.t list
  ; rule : Rule_tag.t
  }
