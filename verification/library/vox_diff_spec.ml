(** A diff describes its source, target, and number of edits.
    Keeps cost zero; insertions and deletions cost one. *)

type ('a : logical_data) operation =
  | Keep of 'a | Delete of 'a | Insert of 'a
[@@inductive]
type 'a diff = 'a operation list

let[@def] rec source (edits : 'a diff) =
  match edits with
  | [] -> []
  | Keep x :: rest | Delete x :: rest -> x :: source rest
  | Insert _ :: rest -> source rest

let[@def] rec target (edits : 'a diff) =
  match edits with
  | [] -> []
  | Keep x :: rest | Insert x :: rest -> x :: target rest
  | Delete _ :: rest -> target rest

let[@def] rec cost (edits : 'a diff) =
  match edits with
  | [] -> 0Z
  | Keep _ :: rest -> cost rest
  | Delete _ :: rest | Insert _ :: rest -> Bigint.add 1Z (cost rest)

type 'a optimal_diff = {
  edits : 'a diff;
  optimality : (other : 'a diff) ->
    {u : unit |
      if source other === source edits && target other === target edits
      then cost edits <= cost other else true} @@ ghost total;
}
