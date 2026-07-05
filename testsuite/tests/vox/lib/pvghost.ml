type ilist =
  | Nil
  | Cons of int * ilist

type 'a proph : value refines ('a) = P of { u : unit }

let new_proph : unit -> 'a proph @ unique =
  fun () -> Obj.magic_unique (P { u = () })

let resolve : (p : 'a proph) @ unique -> (v : 'a) -> unit{ p = v } =
  fun p v ->
    let (P _) = p in
    ignore v;
    assume_unchecked_ ()
