(* Compiled without the refinement extension by flat_hashtbl_boundary.ml:
   a consumed token cannot be used again. *)
module V = Flat_hashtbl_public.V
let f () =
  let r : int V.created = V.create (Ghost_pref.empty ()) in
  let changed = V.replace r.#table r.#view 1 84 r.#token in
  V.replace r.#table changed.#view 2 90 r.#token
