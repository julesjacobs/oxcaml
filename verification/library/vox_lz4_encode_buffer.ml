module P = Ghost_pref
module H = P.Heap
module M = Raw_memory

let[@def] rec (initialized @ total) (h : P.heap @ immutable)
    (p : M.t @ immutable) (count : int) = ghost_ (
  if count <= 0 then true
  else
    (match H.at h (M.location p (count - 1)) with
     | Some (Some _) -> true
     | _ -> false)
    && initialized h p (count - 1))
[@@decreases count]

let rec (initialized_get @ total) :
    (h : P.heap) @ immutable -> (p : M.t) @ immutable ->
    (count : int) -> (index : int) ->
    {u : unit | not (initialized h p count && 0 <= index && index < count)
      || match H.at h (M.location p index) with
         | Some (Some _) -> true | _ -> false} @ ghost =
  fun h p count index -> ghost_ (
    initialized_def h p count;
    if 0 <= index && index < count && count - 1 <> index then
      initialized_get h p (count - 1) index;
    ())
[@@decreases count]

let rec (write_preserves_initialized @ total) :
    (h : P.heap) @ immutable -> (p : M.t) @ immutable ->
    (count : int) -> (index : int) -> (value : M.byte) ->
    {u : unit | not (initialized h p count && 0 <= count && count <= index)
      || initialized (H.put h (M.location p index) (Some value)) p count}
      @ ghost =
  fun h p count index value -> ghost_ (
    initialized_def h p count;
    initialized_def (H.put h (M.location p index) (Some value)) p count;
    if count > 0 && count <= index && initialized h p count then begin
      M.location_law p p (count - 1) index;
      write_preserves_initialized h p (count - 1) index value
    end;
    ())
[@@decreases count]

let (append_initialized @ total) :
    (h : P.heap) @ immutable -> (p : M.t) @ immutable ->
    (count : int) -> (value : M.byte) ->
    {u : unit | not (0 <= count && initialized h p count)
      || initialized (H.put h (M.location p count) (Some value)) p
           (count + 1)} @ ghost =
  fun h p count value -> ghost_ (
    write_preserves_initialized h p count count value;
    initialized_def (H.put h (M.location p count) (Some value)) p
      (count + 1);
    ())

type storage : value mod portable contended = {
  block : M.t @@ aliased;
  permission : P.token @@ ghost;
  used : int;
}

type t : value mod portable contended = {s : storage |
  0 <= s.used && s.used <= M.length s.block
  && M.length s.block <= 4210768
  && M.covers (P.own s.permission) s.block 0 (M.length s.block)
  && H.mem (P.own s.permission) (M.location s.block (-1))
  && initialized (P.own s.permission) s.block s.used}

let used : (s : t) @ local aliased -> {n : int | n = s.used} =
  fun s -> s.used

let capacity : (s : t) @ local aliased ->
    {n : int | n = M.length s.block} =
  fun s -> M.length s.block

let create (capacity : {n : int | 0 <= n && n <= 4210768}) :
    {result : t option | match result with
      | None -> true
      | Some buffer -> M.length buffer.block = capacity && buffer.used = 0
        && P.own buffer.permission === M.footprint buffer.block}
      @ unique =
  let initial = P.empty () in
  let before = ghost_ (P.own (borrow_ initial)) in
  let allocation = M.malloc capacity initial in
  match allocation.value with
  | None -> None
  | Some block ->
    let permission = allocation.state in
    ghost_ (M.allocated_covers block before);
    ghost_ (M.footprint_at block (-1));
    ghost_ (H.union_law (M.footprint block) before (H.empty ()));
    ghost_ (
      let h = P.own (borrow_ permission) in
      let marker = M.location block (-1) in
      initialized_def h block 0;
      let _ = H.mem h marker in
      ());
    Some { block; permission; used = 0 }

let append : (s : {s : t | s.used < M.length s.block}) @ unique ->
    (value : M.byte) ->
    {r : t | r.block === s.block && r.used = s.used + 1
      && P.own r.permission ===
           H.put (P.own s.permission) (M.location s.block s.used)
             (Some value)} @ unique =
  fun s value ->
    let { block; permission; used } = s in
    let before = ghost_ (P.own (borrow_ permission)) in
    ghost_ (M.covers_get before block 0 (M.length block) used);
    let permission = M.write block used value permission in
    ghost_ (M.write_covers before block 0 (M.length block) used value);
    ghost_ (append_initialized before block used value);
    ghost_ (M.location_law block block used (-1));
    { block; permission; used = used + 1 }

let get : (s : t) @ unique ->
    (index : {i : int | 0 <= i && i < s.used}) ->
    (M.byte * {r : t | r.block === s.block && r.used = s.used
      && P.own r.permission === P.own s.permission}) @ unique =
  fun s index ->
    let { block; permission; used } = s in
    ghost_ (initialized_get (P.own (borrow_ permission)) block used index);
    ghost_ (M.covers_get (P.own (borrow_ permission)) block 0
              (M.length block) index);
    let value = M.read block index (borrow_ permission) in
    value, { block; permission; used }

type observation : value mod portable contended = {
  value : M.byte;
  next : t;
}

let observe : (s : t) @ unique ->
    (index : {i : int | 0 <= i && i < s.used}) ->
    {r : observation | r.next.block === s.block
      && r.next.used = s.used
      && P.own r.next.permission === P.own s.permission
      && H.at (P.own r.next.permission) (M.location s.block index)
         === Some (Some r.value)} @ unique =
  fun s index ->
    let { block; permission; used } = s in
    ghost_ (initialized_get (P.own (borrow_ permission)) block used index);
    ghost_ (M.covers_get (P.own (borrow_ permission)) block 0
              (M.length block) index);
    let value = M.read block index (borrow_ permission) in
    { value; next = { block; permission; used } }

let release (s : t) @ unique =
  let { block; permission; used = _ } = s in
  let _ = M.free block permission in
  ()
