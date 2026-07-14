(** Word-level pre-blast simplification for the QF_BV lane. A term-to-term rewrite applied
    to the asserted set before eager bit-blasting, to shrink the resulting SAT instance.

    The only value-changing transform is ADDITIVE NORMALIZATION over the mod-2^w group
    ([bvadd]/[bvsub]/[bvneg]): each additive expression is flattened into a constant plus
    a coefficient per distinct non-additive atom, coefficients are combined mod 2^w
    (cancelling to zero drops the atom), and a canonical term is rebuilt. This (a) folds
    constants,

    (b) cancels cross terms ([a + b - a = b]), and (c) exposes a shared additive subterm
    across sibling expressions so the DAG blaster encodes it once. Every other node is
    rebuilt with simplified children by re-applying its own operator symbol, so no
    operator semantics change.

    Soundness of the additive rewrite is exactly group/ring arithmetic mod 2^w, guarded by
    the exhaustive small-width oracle ({!Bv_eval} equivalence over all assignments) plus
    the symbolic-equivalence goldens. NOTE: the bv model re-check ({!Bv_solve}) validates
    the REWRITTEN formula, so it does not on its own catch an unsound rewrite — the oracle
    and goldens are the authority, especially on the unsat side which has no model net. *)

open Oxsmt_core

(** [simplify ctx mint terms] returns the simplified assertions. New literals/constants it
    introduces are minted through [mint] (the reserved bit-vector namespace). Free
    variables are never renamed, so a model over the result is a model over the originals. *)
val simplify : Context.t -> Bv.minter -> Term.t list -> Term.t list
