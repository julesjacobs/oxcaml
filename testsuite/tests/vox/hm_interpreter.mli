module D := Hm_declarative
module Spec = Hm_interpreter_typing

type certificate = {typing : Spec.evidence; execution : Hm_evaluation.execution}
type result = #{value : Spec.value; evidence : certificate @@ ghost}
type typing = #{ty : D.mono @@ ghost; derivation : D.typing @@ ghost}

(** Evaluates a closed, declaratively typed term. Every returned value has the
    supplied type and a finite big-step execution witness. Evaluation may diverge; all runtime type-error branches are
    proved unreachable. The typing derivation and result evidence are erased. *)
val run : (term : D.term) @ immutable ->
  (p : {p : typing | D.typed D.Z D.Empty_context term p.#ty p.#derivation})
    @ immutable ->
  {r : result | Spec.valid r.#evidence.typing (Spec.Value (r.#value, p.#ty))
    && Hm_evaluation.evaluates Spec.Empty term r.#value r.#evidence.execution} @ immutable
