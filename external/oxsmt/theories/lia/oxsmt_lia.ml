(** LIA theory plugin: incremental simplex over rationals + branch-and-bound for
    integrality (Dutertre-de Moura).

    Responsibility: the LIA decision procedure for linear integer arithmetic — general
    simplex over δ-rationals for bound reasoning with backtracking-friendly state,
    branch-and-bound on top; conflicts emitted as infeasible bound sets justified by
    Farkas coefficients, self-checked at production (DESIGN.md §6-7, INVARIANTS.md I4).

    This is the {b algorithm} surface (M3-lia). The THEORY-functor adapter that binds
    {!Lia} to the frozen engine interface (ADR-0005) is a later, separate concern;
    {!Lia}'s ['tok] premise-token parameter is the seam it will bind [Lit.t] to.

    Public submodules: {!Bigint} (stdlib arbitrary-precision ints, the [Rational] Big-tier
    fallback — core-bignum W2), {!Rational} (exact ℚ, two-tier Small/Big), {!Delta}
    (δ-rationals), {!Simplex} (the DdM tableau), {!Lia} (the integer decision procedure),
    {!Lra} (the linear-real decision procedure), and {!Lia_adapter} (the frozen
    {!Oxsmt_core.Theory.THEORY} binding, ADR-0005 M4). *)

(* [Bigint] moved down into [oxsmt_core] (it now also backs the core term coefficients);
   re-exported here so [Oxsmt_lia.Bigint] keeps naming it for existing consumers/tests. *)
module Bigint = Oxsmt_core.Bigint
module Rational = Rational
module Delta = Delta
module Simplex = Simplex
module Hnf = Hnf
module Lia = Lia
module Lra = Lra
module Lia_adapter = Lia_adapter
module Lra_adapter = Lra_adapter
