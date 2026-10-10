module D = Hm_declarative

type payload = { term : D.term; ty : D.mono; proof : D.typing }
type t = {c : payload | D.typed D.Z D.Empty_context c.term c.ty c.proof}

let[@def] (source @ total) (checked : t @ immutable) = checked.term
let[@def] (root @ total) (checked : t @ immutable) = checked.ty

let (check @ total) :
    (term : D.term) @ immutable -> (ty : D.mono) @ immutable ->
    (proof : D.typing) @ immutable ->
    {r : t option | match r with None -> true
      | Some checked -> source checked === term && root checked === ty}
    @ immutable = fun term ty proof ->
  if Hm_elaboration_check.check D.Z D.Empty_context term ty proof then (
    let checked : t = {term; ty; proof} in
    ghost_ (source_def checked; root_def checked);
    Some checked)
  else None

let (derivation @ total) : (checked : t) @ immutable ->
    {d : D.typing | D.typed D.Z D.Empty_context
      (source checked) (root checked) d} @ immutable = fun checked ->
  ghost_ (source_def checked; root_def checked);
  checked.proof

let (of_derivation @ total) : (term : D.term) @ immutable -> (ty : D.mono) @ immutable ->
    (derivation : D.typing) @ immutable ->
    {u : unit | D.typed D.Z D.Empty_context term ty derivation} ->
    {checked : t | source checked === term && root checked === ty} @ immutable =
  fun term ty derivation premise ->
    let checked : t = {term; ty; proof = derivation} in
    ghost_ (source_def checked; root_def checked); checked
