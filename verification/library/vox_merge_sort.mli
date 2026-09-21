module S = Vox_sequence

module Make (O : Vox_ordered_sequence.Order) (C : Vox_credits.S) (Compare : sig
  type result = #{ before : bool; state : C.token @@ ghost }
  val compare : (left : O.elt) @ immutable -> (right : O.elt) @ immutable ->
    (token : {t : C.token | C.credits t > 0}) @ unique total ghost ->
    {r : result | r.#before = (O.le left right) &&
      C.credits r.#state = C.credits token - 1} @ unique @@ total
end) : sig
  module P : module type of Vox_ordered_sequence.Make (O)
  type result = #{ values : O.elt list @@ aliased; state : C.token @@ ghost }

  val sort : (values : O.elt list) @ immutable ->
      (token : {t : C.token | Vox_sort_cost.budget (S.length values) <=
        Bigint.of_int (C.credits t)}) @ unique total ghost ->
      {r : result | P.sorted r.#values && P.permutation values r.#values &&
        S.length r.#values = S.length values &&
        0 <= C.credits r.#state && C.credits r.#state <= C.credits token &&
        Bigint.of_int (C.credits r.#state) >=
          Bigint.sub (Bigint.of_int (C.credits token))
            (Vox_sort_cost.budget (S.length values))} @ unique @@ total
end
