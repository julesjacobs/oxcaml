@@ portable

module P = Ghost_pref
module H = P.Heap
module M = Raw_memory
module C = One_shot

type slot : value mod portable contended = {
  block : M.t @@ aliased;
  index : int;
  permission : M.contents P.token @@ ghost;
}
type owned : value mod portable contended = {s : slot | 0 <= s.index && s.index < M.length s.block &&
  H.mem (P.own s.permission) (M.location s.block s.index)}

type receipt : value mod portable contended = { slot : owned; expected : M.byte }
type filled : value mod portable contended = {r : receipt |
  match H.at (P.own r.slot.permission)
    (M.location r.slot.block r.slot.index) with
  | Some (Some v) -> v = r.expected
  | _ -> false}

val fill : (s : owned) @ unique -> (value : M.byte) ->
    {r : filled | r.slot.block === s.block && r.slot.index = s.index &&
      r.expected = value} @ unique

type ('a : value mod portable contended) pending = {
  answer : 'a C.recv;
  worker : unit Domain.t @@ aliased;
}

val dispatch : (block : M.t) ->
    (index : {i : int | 0 <= i && i < M.length block}) ->
    (value : M.byte) ->
    (permission : {t : M.contents P.token | H.mem (P.own t) (M.location block index)})
      @ unique ghost ->
    {r : filled | r.slot.block === block && r.slot.index = index &&
      r.expected = value} pending @ unique

val read_receipt : (r : filled) @ unique ->
    ({v : int | v = r.expected} *
     {s : owned | s.block === r.slot.block && s.index = r.slot.index})
      @ unique
