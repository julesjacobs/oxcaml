module D := Hm_declarative

type t : immutable_data

val source : t @ immutable -> D.term @ immutable @@ total
val root : t @ immutable -> D.mono @ immutable @@ total

val check :
  (term : D.term) @ immutable -> (ty : D.mono) @ immutable ->
  (derivation : D.typing) @ immutable ->
  {r : t option | match r with None -> true
    | Some checked -> source checked === term && root checked === ty}
  @ immutable @@ total

val derivation :
  (checked : t) @ immutable ->
  {d : D.typing | D.typed D.Z D.Empty_context
    (source checked) (root checked) d} @ immutable @@ total

val of_derivation : (term : D.term) @ immutable -> (ty : D.mono) @ immutable ->
  (derivation : D.typing) @ immutable ->
  {u : unit | D.typed D.Z D.Empty_context term ty derivation} ->
  {checked : t | source checked === term && root checked === ty} @ immutable @@ total
