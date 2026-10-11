module V = Flat_hashtbl_public.V
let f () =
  let r : int V.created = V.create () in
  let other : int V.created = V.create () in
  V.find_opt r.#table 1 (borrow_ other.#permission)
