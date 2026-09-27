(* Compiled without the refinement extension by flat_hashtbl_boundary.ml:
   a lookup without the table's ownership is still rejected. *)
module V = Flat_hashtbl_public.V
let f () =
  let r : int V.created = V.create (Ghost_pref.empty ()) in
  let empty = Ghost_pref.empty () in
  V.find_opt r.#table r.#view 1 (borrow_ empty)
