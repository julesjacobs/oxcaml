open Copy_spec
open Generalize_spec

let[@def] (retained @ total) (h : Pref.heap @ immutable)
    (p : node Pref.t @ immutable) = ghost_ (
  match H.at h p with Some {level = Finite _; _} -> true | _ -> false)
let[@def] rec (transfer @ total) (h : Pref.heap @ immutable)
    (child : pool @ immutable) (parent : pool @ immutable) = ghost_ (
  match child with Empty -> parent | Entry (p, rest) ->
    transfer h rest (if retained h p then Entry (p, parent) else parent))

type closed = #{state : Pref.token; parent : pool @@ aliased}
