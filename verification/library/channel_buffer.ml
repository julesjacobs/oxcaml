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

let fill : (s : owned) @ unique -> (value : M.byte) ->
    {r : filled | r.slot.block === s.block && r.slot.index = s.index &&
      r.expected = value} @ unique = fun s value ->
  let { block; index; permission } = s in
  let before = ghost_ (P.own (borrow_ permission)) in
  let permission = M.write block index value permission in
  ghost_ (
    let after = H.put before (M.location block index) (Some value) in
    let _ = H.mem after (M.location block index) in
    let _ = H.at after (M.location block index) in ());
  { slot = { block; index; permission }; expected = value }

type ('input : value mod portable contended,
      'output : value mod portable contended) job
    : value mod portable contended = {
  work : 'input;
  reply : 'output C.send;
}

type ('a : value mod portable contended) pending = {
  answer : 'a C.recv;
  worker : unit Domain.t @@ aliased;
}

let dispatch : (block : M.t) ->
    (index : {i : int | 0 <= i && i < M.length block}) ->
    (value : M.byte) ->
    (permission : {t : M.contents P.token | H.mem (P.own t) (M.location block index)})
      @ unique ghost ->
    {r : filled | r.slot.block === block && r.slot.index = index &&
      r.expected = value} pending @ unique = fun block index value permission ->
  let module Protocol = struct
    type input : value mod portable contended =
      {x : owned | x.block === block && x.index = index}
    type output : value mod portable contended =
      {r : filled | r.slot.block === block && r.slot.index = index &&
        r.expected = value}
    type request : value mod portable contended = (input, output) job
  end in
  let (reply, answer : Protocol.output C.send * Protocol.output C.recv) =
    C.create () in
  let (send_job, jobs : Protocol.request C.send * Protocol.request C.recv) =
    C.create () in
  let worker = Domain.Safe.spawn (fun () ->
    let { work; reply } = C.recv jobs in
    C.send reply (fill work value)) in
  C.send send_job { work = { block; index; permission }; reply };
  { answer; worker }

let read_receipt : (r : filled) @ unique ->
    ({v : int | v = r.expected} *
     {s : owned | s.block === r.slot.block && s.index = r.slot.index})
      @ unique = fun r ->
  let { slot; expected = _ } = r in
  let { block; index; permission } = slot in
  let value = M.read block index (borrow_ permission) in
  value, { block; index; permission }
