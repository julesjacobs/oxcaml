module D := Hm_declarative

module F := Fast_environment

type index = { number : int; original : D.index @@ ghost; }

type term =
    Bound of index
  | Truth
  | Lambda of term
  | Recursive of term
  | Apply of term * term
  | Let of term * term
[@@inductive]

val source : term @ immutable -> D.term @ ghost
  @@ total

val source_def :
  (term : term) @ immutable ->
  {u : unit
    | (source term) ===
        (ghost_
           (match term with
            | Bound i -> D.Bound (i.original)
            | Truth -> D.Truth
            | Lambda b' -> D.Lambda (source b')
            | Recursive b'' -> D.Recursive (source b'')
            | Apply (a', b''') -> D.Apply ((source a'), (source b'''))
            | Let (a, b) -> D.Let ((source a), (source b))))}
  @@ total

val valid : term @ immutable -> bool @ ghost
  @@ total

val valid_def :
  (term : term) @ immutable ->
  {u : unit
    | (valid term) ===
        (ghost_
           (match term with
            | Bound i -> (i.number >= 0) && (F.encoded i.original i.number)
            | Truth -> true
            | Lambda b' | Recursive b' -> valid b'
            | Apply (a, b) | Let (a, b) -> (valid a) && (valid b)))}
  @@ total

val encode :
  (index : D.index) @ immutable ->
  {out : index
    | (out.number >= 0) &&
        ((F.encoded index out.number) && (out.original === index))} @ immutable

val compile :
  (term : D.term) @ immutable ->
  {t : term | (valid t) && ((source t) === term)} @ immutable

val decode : int -> D.index @ ghost
  @@ total

val decode_def :
  (n : int) ->
  {u : unit
    | (decode n) === (ghost_ (if n <= 0 then D.Z else D.S (decode (n - 1))))}
  @@ total

val bound :
  (n' : {n : int | n >= 0}) ->
  {t : term
    | let refine_ n = n' in
      (valid t) && ((source t) === (D.Bound (decode n)))} @ immutable
