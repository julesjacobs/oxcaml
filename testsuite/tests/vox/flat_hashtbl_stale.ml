(* Compiled without the refinement extension by flat_hashtbl_boundary.ml:
   a stale view is still rejected. *)
module V = Flat_hashtbl_public.V
let f () =
  let r : int V.created = V.create (Ghost_pref.empty ()) in
  let changed = V.replace r.#table r.#view 1 84 r.#token in
  V.find_opt r.#table r.#view 1 (borrow_ changed.#token)
