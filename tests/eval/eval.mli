open Oxsmt_core

(** The independent model evaluator (DESIGN.md §8 layer 1; task #74). Total denotational
    semantics for the 9 frozen {!Term.node}s under a model, written from ADR-0003 alone
    with no access to solver internals. Answers the one question layer 1 needs: does a
    model satisfy a set of assertions?

    Semantics (ADR-0003):
    - [Bool_const]/[Int_const] evaluate to themselves.
    - [Arith {coeffs; const}] = [Σ cᵢ · ⟦tᵢ⟧ + const] over ℤ (subterms evaluated
      recursively — they may be [App]s). Every integer operation is overflow-guarded and
      {b raises} rather than wrapping (I8 spirit).
    - [Le arg] = [⟦arg⟧ ≤ 0].
    - [Eq (a, b)] = structural value equality; for Bool operands this is exactly iff.
    - [Not]/[And]/[Or] standard ([And]/[Or] force every operand so a model error anywhere
      is still loud); [Ite] evaluates the condition then the taken branch only.
    - [App] with the reserved [div]/[mod] symbols is euclidean:
      [x = d·q + r ∧ 0 ≤ r < |d|]. Any other [App] is looked up in the model — a nullary
      symbol via its constant binding, an applied function via its finite table (first
      matching case, else the mandatory default).
    - A symbol the model does not define, or a type mismatch, is a loud {!Eval_error} — it
      is never silently defaulted. *)

(** Model resolution failure (undefined symbol, type mismatch) or an integer overflow. A
    loud, deliberate stop — never a wraparound or a silent default. *)
exception Eval_error of string

(** [eval model t] evaluates [t] under [model]. *)
val eval : Eval_model.t -> Term.t -> Value.t

(** The spec-shaped entry (task #74): [env] resolves nullary symbols to values. This
    covers the constant + arithmetic + boolean + div/mod fragment; an applied (arity ≥ 1)
    uninterpreted function raises {!Eval_error}, since a [Symbol.t -> Value.t option]
    cannot supply a function table (a function is not a {!Value.t}). Use {!eval} for
    models with such functions. *)
val eval_term : env:(Symbol.t -> Value.t option) -> Term.t -> Value.t

type outcome =
  | Satisfies
  | Fails of
      { index : int (* 0-based index of the first assertion the model falsifies *)
      ; trace : string (* rendered subterm values along the failing assertion *)
      }

(** [check model assertions] evaluates the assertions in order; [Satisfies] iff every one
    evaluates to [Bool true]. A non-Bool assertion is an {!Eval_error} (well-sorted
    assertions are Bool, so this signals a malformed query). *)
val check : Eval_model.t -> Term.t list -> outcome

(** A bounded, indented dump of a term with each visited subterm's value — the
    failing-path detail printed to stderr on [MODEL-FAILS]. *)
val explain : Eval_model.t -> Term.t -> string
