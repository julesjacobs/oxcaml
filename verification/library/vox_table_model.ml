module S = Vox_sequence

type ('k : immutable_data, 'v : immutable_data) state = {
  capacity : int;
  size : int;
  deleted : int;
  slots : ('k * 'v) option list;
  controls : int list;
}

let[@def] rec (repeat @ total) (n : int)
    (value : 'a @ immutable) : 'a list @ immutable =
  if n <= 0 then [] else value :: repeat (n - 1) value
  [@@decreases if n > 0 then n else 0]

let[@def] (initial @ total) (capacity : int)
    (entry : ('k * 'v) option @ immutable) = {
  capacity; size = 0; deleted = 0;
  slots = repeat capacity entry;
  controls = repeat (capacity + 15) 128;
}

let[@def] (slot @ total)
    (s : ('k, 'v) state @ immutable) (i : int) =
  S.at s.slots (Bigint.of_int i)

let[@def] (control @ total)
    (s : ('k, 'v) state @ immutable) (i : int) =
  S.at s.controls (Bigint.of_int i)

let[@def] (set_slot @ total)
    (s : ('k, 'v) state @ immutable) (i : int)
    (entry : ('k * 'v) option @ immutable) =
  {s with slots = S.set s.slots (Bigint.of_int i) entry}

let[@def] (set_control @ total)
    (s : ('k, 'v) state @ immutable) (i : int) (byte : int) =
  {s with controls = S.set s.controls (Bigint.of_int i) byte}

let[@def] (set_counts @ total)
    (s : ('k, 'v) state @ immutable) (size : int) (deleted : int) =
  {s with size; deleted}

let[@def] (lane_bit @ total) (lane : int) =
  match lane with
  | 0 -> 1 | 1 -> 2 | 2 -> 4 | 3 -> 8
  | 4 -> 16 | 5 -> 32 | 6 -> 64 | 7 -> 128
  | 8 -> 256 | 9 -> 512 | 10 -> 1024 | 11 -> 2048
  | 12 -> 4096 | 13 -> 8192 | 14 -> 16384 | 15 -> 32768
  | _ -> 0

let[@def] rec (matching @ total)
    (s : ('k, 'v) state @ immutable) (offset : int) (byte : int)
    (lanes : int) =
  if lanes <= 0 then 0
  else
    let lane = lanes - 1 in
    let bit = match control s (offset + lane) with
      | Some x -> if x = byte then lane_bit lane else 0
      | None -> 0 in
    matching s offset byte lane lor bit
  [@@decreases if lanes > 0 then lanes else 0]

let[@def] (set_byte @ total)
    (s : ('k, 'v) state @ immutable) (index : int) (byte : int) =
  let changed = set_control s index byte in
  if index < 15 then set_control changed (s.capacity + index) byte
  else changed

let[@def] (remove_slot @ total)
    (s : ('k, 'v) state @ immutable) (index : int) =
  set_counts (set_byte (set_slot s index None) index 254)
    (s.size - 1) (s.deleted + 1)
