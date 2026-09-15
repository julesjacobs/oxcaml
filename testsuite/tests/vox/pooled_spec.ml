open Copy_spec
open Generalize_spec

let[@def] rec (registered @ total) (base : pool @ immutable) (epoch : node Pref.t @ immutable)
    (d : history @ immutable) = ghost_ (match d with
  | Start -> Entry (epoch, base)
  | Fresh (rest, _, q, _, _) -> Entry (q, registered base epoch rest)
  | Alias (rest, _, _, _) -> registered base epoch rest)

type allocated = #{value : node Pref.t @@ aliased; state : node Pref.token; pool : pool @@ aliased}
type copied = #{value : node Pref.t @@ aliased; state : node Pref.token; pool : pool @@ aliased; history : history @@ ghost}
type instance = #{value : node Pref.t @@ aliased; state : node Pref.token; pool : pool @@ aliased;
  epoch : node Pref.t @@ ghost; history : history @@ ghost}
