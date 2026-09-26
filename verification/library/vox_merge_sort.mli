module S = Vox_sequence

module Make (O : sig
  type elt : immutable_data mod total
  val le : elt @ immutable -> elt @ immutable -> bool @ ghost @@ total
  val reflexive : (x : elt) @ immutable -> {u : unit | le x x}
    @ ghost @@ total
  val totality : (x : elt) @ immutable -> (y : elt) @ immutable ->
    {u : unit | le x y || le y x} @ ghost @@ total
  val transitive : (x : elt) @ immutable -> (y : elt) @ immutable ->
    (z : elt) @ immutable ->
    {u : unit | if le x y && le y z then le x z else true} @ ghost @@ total
end) (C : Vox_credits.S) (Compare : sig
  type result = #{ before : bool; state : C.token @@ ghost }
  val compare : (left : O.elt) @ immutable -> (right : O.elt) @ immutable ->
    (token : {t : C.token | C.credits t > 0}) @ unique total ghost ->
    {r : result | let token = token in
      r.#before = (O.le left right) &&
      C.credits r.#state = C.credits token - 1} @ unique @@ total
end) : sig
  module P : sig
    val count : O.elt list @ immutable -> O.elt @ immutable ->
      Bigint.t @ ghost @@ total
    val count_def : (values : O.elt list) @ immutable ->
      (target : O.elt) @ immutable ->
      {u : unit | count values target === (ghost_ (match values with
        | [] -> 0Z
        | head :: tail -> Bigint.add (if head === target then 1Z else 0Z)
            (count tail target)))} @@ total
    val permutation : O.elt list @ immutable -> O.elt list @ immutable ->
      bool @ ghost @@ total
    val count_extensional : (left : O.elt list) @ immutable ->
      (right : O.elt list) @ immutable ->
      ((target : O.elt) @ immutable ->
        {u : unit | count left target = count right target} @ ghost)
        @ total ghost -> {u : unit | permutation left right} @ ghost @@ total
    val sorted : O.elt list @ immutable -> bool @ ghost @@ total
    val sorted_adjacent : (values : O.elt list) @ immutable ->
      {u : unit | sorted values === (match values with
        | [] -> true
        | first :: rest -> (match rest with [] -> true
          | second :: _ -> O.le first second && sorted rest))} @ ghost @@ total
    val permutation_count : (left : O.elt list) @ immutable ->
      (right : O.elt list) @ immutable -> (target : O.elt) @ immutable ->
      {u : unit | if permutation left right then
        count left target = count right target else true} @ ghost @@ total
  end
  type result = #{ values : O.elt list @@ aliased; state : C.token @@ ghost }

  val sort : (values : O.elt list) @ immutable ->
      (token : {t : C.token | Vox_sort_cost.budget (S.length values) <=
        Bigint.of_int (C.credits t)}) @ unique total ghost ->
      {r : result | let token = token in
        P.sorted r.#values && P.permutation values r.#values &&
        S.length r.#values = S.length values &&
        0 <= C.credits r.#state && C.credits r.#state <= C.credits token &&
        Bigint.of_int (C.credits r.#state) >=
          Bigint.sub (Bigint.of_int (C.credits token))
            (Vox_sort_cost.budget (S.length values))} @ unique @@ total
end
