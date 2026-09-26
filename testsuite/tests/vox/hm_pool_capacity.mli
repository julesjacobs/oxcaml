module T := Fast_term

val fits : T.term @ immutable -> int -> int -> bool @ ghost
  @@ total

val fits_def :
  (term : T.term) @ immutable ->
  (depth : int) ->
  (limit : int) ->
  {u : unit
    | (fits term depth limit) ===
        (ghost_
           ((0 <= depth) &&
              ((depth <= limit) &&
                 (match term with
                  | T.Bound _ | T.Truth | T.False | T.Word _ | T.Nil -> true
                  | T.Lambda body | T.Recursive body -> fits body depth limit
                  | T.Apply (a', b') | T.Cons (a', b') | T.Primitive (_, a', b') ->
                      (fits a' depth limit) && (fits b' depth limit)
                  | T.If (a, b, c) | T.CaseList (a, b, c) ->
                      fits a depth limit && fits b depth limit && fits c depth limit
                  | T.Let (a, b) ->
                      ((depth + 1) > depth) &&
                        ((fits a (depth + 1) limit) && (fits b depth limit))))))}
  @@ total

val measure : (term : T.term) @ immutable -> {n : int | fits term 0 n}

val create :
  (term : T.term) @ immutable ->
  {a : Generalize_spec.pool Borrow_iarray.Owned_array.t
    | fits term 0 (Iarray.length (Borrow_iarray.Owned_array.contents a))} @ unique
