module V = Flat_hashtbl_public.V
let f () =
  let r : int V.created = V.create () in
  let _changed = V.replace r.#table 1 84 r.#permission in
  V.find_opt r.#table 1 (borrow_ r.#permission)
