(** Counted keeps omit unchanged content; edits retain their content. *)

type ('a : logical_data) operation =
  | Keep of int | Delete of 'a | Insert of 'a
[@@inductive]
type 'a diff = 'a operation list

module Sequence = Vox_sequence

let[@def] rec patch (edits : 'a diff) (old : 'a list) = ghost_ (
  match edits with
  | [] -> (match old with [] -> Some [] | _ :: _ -> None)
  | Keep n :: rest ->
    let count = Bigint.of_int n in
    if n < 0 || Sequence.length old < count then None else
    (match patch rest (Sequence.drop count old) with
     | None -> None
     | Some fresh -> Some (Sequence.append (Sequence.take count old) fresh))
  | Delete x :: rest ->
    (match old with
     | y :: tail when x === y -> patch rest tail
     | _ -> None)
  | Insert x :: rest ->
    (match patch rest old with
     | None -> None | Some fresh -> Some (x :: fresh)))

let[@def] relates (edits : 'a diff) (old : 'a list) (fresh : 'a list) =
  ghost_ (patch edits old === Some fresh)

let[@def] rec cost (edits : 'a diff) =
  match edits with
  | [] -> 0Z
  | Keep _ :: rest -> cost rest
  | Delete _ :: rest | Insert _ :: rest -> Bigint.add 1Z (cost rest)

let[@def] rec inverse (edits : 'a diff) =
  match edits with
  | [] -> []
  | Keep n :: rest -> Keep n :: inverse rest
  | Delete x :: rest -> Insert x :: inverse rest
  | Insert x :: rest -> Delete x :: inverse rest

type 'a optimal_diff = {
  old : 'a list @@ ghost;
  fresh : 'a list @@ ghost;
  edits : {edits : 'a diff | relates edits old fresh};
  optimality : (other : 'a diff) ->
    {u : unit | if relates other old fresh
      then cost edits <= cost other else true} @@ ghost total;
}
