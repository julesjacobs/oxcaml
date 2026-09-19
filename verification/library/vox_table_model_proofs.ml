module M = Vox_table_model
module S = Vox_sequence

let rec (repeat_at @ total) : ('a : immutable_data).
    (n : int) -> (value : 'a) @ immutable -> (index : Bigint.t) ->
    {u : unit | S.at (M.repeat n value) index ===
      (if 0Z <= index && index < Bigint.of_int n then Some value else None)}
    @ ghost = fun n value index -> ghost_ (
  M.repeat_def n value;
  S.at_def (M.repeat n value) index;
  if n > 0 then begin
    if index <> 0Z then repeat_at (n - 1) value (Bigint.sub index 1Z);
    ()
  end else ())
  [@@decreases if n > 0 then n else 0]

let rec (repeat_length @ total) : ('a : immutable_data).
    (n : int) -> (value : 'a) @ immutable ->
    {u : unit | S.length (M.repeat n value) ===
      (if n > 0 then Bigint.of_int n else 0Z)} @ ghost =
  fun n value -> ghost_ (
    M.repeat_def n value;
    S.length_def (M.repeat n value);
    if n > 0 then repeat_length (n - 1) value;
    ())
  [@@decreases if n > 0 then n else 0]

let rec (at_set @ total) : ('a : immutable_data).
    (values : 'a list) @ immutable -> (index : Bigint.t) ->
    (value : 'a) @ immutable -> (query : Bigint.t) ->
    {u : unit | S.at (S.set values index value) query ===
      (match S.at values index with
       | None -> S.at values query
       | Some _ -> if index = query then Some value else S.at values query)}
    @ ghost = fun values index value query -> ghost_ (
  S.set_def values index value;
  S.at_def values index;
  S.at_def values query;
  S.at_def (S.set values index value) query;
  match values with
  | [] -> ()
  | _ :: tail ->
    if index <> 0Z && query <> 0Z then
      at_set tail (Bigint.sub index 1Z) value (Bigint.sub query 1Z);
    ())

let (initial_slot @ total) : ('k : immutable_data) ('v : immutable_data).
    (model : ('k, 'v) M.state) @ immutable -> (capacity : int) ->
    (entry : ('k * 'v) option) @ immutable -> (index : int) ->
    {u : unit | not (model === M.initial capacity entry) ||
      M.slot model index ===
      (if 0 <= index && index < capacity then Some entry else None)} @ ghost =
  fun model capacity entry index -> ghost_ (
    M.initial_def capacity entry;
    M.slot_def model index;
    repeat_at capacity entry (Bigint.of_int index);
    ())

let (initial_control @ total) : ('k : immutable_data) ('v : immutable_data).
    (model : ('k, 'v) M.state) @ immutable -> (capacity : int) ->
    (entry : ('k * 'v) option) @ immutable -> (index : int) ->
    {u : unit | not (model === M.initial capacity entry) ||
      M.control model index ===
      (if 0 <= index && index < capacity + 15 then Some 128 else None)}
    @ ghost = fun model capacity entry index -> ghost_ (
  M.initial_def capacity entry;
  M.control_def model index;
  repeat_at (capacity + 15) 128 (Bigint.of_int index);
  ())

let (slot_write @ total) : ('k : immutable_data) ('v : immutable_data).
    (s : ('k, 'v) M.state) @ immutable -> (index : int) ->
    (entry : ('k * 'v) option) @ immutable -> (query : int) ->
    {u : unit | M.slot (M.set_slot s index entry) query ===
      (match M.slot s index with
       | None -> M.slot s query
       | Some _ -> if index = query then Some entry else M.slot s query)}
    @ ghost = fun s index entry query -> ghost_ (
  M.set_slot_def s index entry;
  M.slot_def s index;
  M.slot_def s query;
  M.slot_def (M.set_slot s index entry) query;
  at_set s.slots (Bigint.of_int index) entry (Bigint.of_int query);
  ())

let (control_write @ total) : ('k : immutable_data) ('v : immutable_data).
    (s : ('k, 'v) M.state) @ immutable -> (index : int) ->
    (byte : int) -> (query : int) ->
    {u : unit | M.control (M.set_control s index byte) query ===
      (match M.control s index with
       | None -> M.control s query
       | Some _ -> if index = query then Some byte else M.control s query)}
    @ ghost = fun s index byte query -> ghost_ (
  M.set_control_def s index byte;
  M.control_def s index;
  M.control_def s query;
  M.control_def (M.set_control s index byte) query;
  at_set s.controls (Bigint.of_int index) byte (Bigint.of_int query);
  ())

let rec (at_present @ total) : ('a : immutable_data).
    (values : 'a list) @ immutable -> (index : Bigint.t) ->
    {u : unit | not (0Z <= index && index < S.length values) ||
      (match S.at values index with Some _ -> true | None -> false)} @ ghost =
  fun values index -> ghost_ (
    S.length_def values; S.at_def values index;
    match values with
    | [] -> ()
    | _ :: tail ->
      if index <> 0Z then at_present tail (Bigint.sub index 1Z);
      ())

let (index_bounds @ total) (capacity : int)
    (index : {i : int | 0 <= i && i < capacity}) :
    {u : unit | 0Z <= Bigint.of_int index &&
      Bigint.of_int index < Bigint.of_int capacity} = ()

let (control_present @ total) : ('k : immutable_data) ('v : immutable_data).
    (s : ('k, 'v) M.state) @ immutable -> (index : int) ->
    {u : unit | not (16 <= s.capacity && s.capacity <= 1073741824 &&
      S.length s.controls = Bigint.of_int (s.capacity + 15) &&
      0 <= index && index < s.capacity + 15) ||
      (match M.control s index with Some _ -> true | None -> false)} @ ghost =
  fun s index -> ghost_ (
    M.control_def s index;
    at_present s.controls (Bigint.of_int index);
    ())

let (byte_write @ total) : ('k : immutable_data) ('v : immutable_data).
    (s : ('k, 'v) M.state) @ immutable ->
    (index : int) -> (byte : int) -> (query : int) ->
    {u : unit | not (16 <= s.capacity && s.capacity <= 1073741824 &&
      S.length s.controls = Bigint.of_int (s.capacity + 15) &&
      0 <= index && index < s.capacity) ||
      M.control (M.set_byte s index byte) query ===
        (if query = index || (index < 15 && query = s.capacity + index)
         then Some byte else M.control s query)} @ ghost =
  fun s index byte query -> ghost_ (
    M.set_byte_def s index byte;
    control_present s index;
    control_write s index byte query;
    if index < 15 then begin
      control_present s (s.capacity + index);
      control_write s index byte (s.capacity + index);
      control_write (M.set_control s index byte) (s.capacity + index) byte
        query;
      ()
    end else ())

let (byte_clone @ total) : ('k : immutable_data) ('v : immutable_data).
    (s : ('k, 'v) M.state) @ immutable ->
    (index : int) -> (byte : int) -> (lane : int) ->
    {u : unit | not (16 <= s.capacity && s.capacity <= 1073741824 &&
      S.length s.controls = Bigint.of_int (s.capacity + 15) &&
      0 <= index && index < s.capacity && 0 <= lane && lane < 15 &&
      M.control s (s.capacity + lane) === M.control s lane) ||
      M.control (M.set_byte s index byte) (s.capacity + lane) ===
        M.control (M.set_byte s index byte) lane} @ ghost =
  fun s index byte lane -> ghost_ (
    byte_write s index byte lane;
    byte_write s index byte (s.capacity + lane);
    ())

let rec (length_nonnegative @ total) : ('a : immutable_data).
    (values : 'a list) @ immutable ->
    {u : unit | 0Z <= S.length values} @ ghost = fun values -> ghost_ (
  S.length_def values;
  match values with [] -> () | _ :: tail -> length_nonnegative tail)

let (byte_at @ total) : ('k : immutable_data) ('v : immutable_data).
    (s : ('k, 'v) M.state) @ immutable ->
    (index : int) -> (byte : int) -> (query : Bigint.t) ->
    {u : unit | not (16 <= s.capacity && s.capacity <= 1073741824 &&
      S.length s.controls = Bigint.of_int (s.capacity + 15) &&
      0 <= index && index < s.capacity) ||
      S.at (M.set_byte s index byte).controls query ===
        (if query = Bigint.of_int index ||
            (index < 15 && query = Bigint.of_int (s.capacity + index))
         then Some byte else S.at s.controls query)} @ ghost =
  fun s index byte query -> ghost_ (
    M.set_byte_def s index byte;
    M.set_control_def s index byte;
    control_present s index; M.control_def s index;
    at_set s.controls (Bigint.of_int index) byte query;
    if index < 15 then begin
      control_present s (s.capacity + index);
      M.control_def s (s.capacity + index);
      at_set s.controls (Bigint.of_int index) byte
        (Bigint.of_int (s.capacity + index));
      M.set_control_def (M.set_control s index byte) (s.capacity + index) byte;
      at_set (M.set_control s index byte).controls
        (Bigint.of_int (s.capacity + index)) byte query;
      ()
    end else ())

let rec (set_negative @ total) : ('a : immutable_data).
    (values : 'a list) @ immutable -> (index : Bigint.t) ->
    (value : 'a) @ immutable ->
    {u : unit | not (index < 0Z) || S.set values index value === values}
    @ ghost = fun values index value -> ghost_ (
  S.set_def values index value;
  match values with
  | [] -> ()
  | _ :: tail -> set_negative tail (Bigint.sub index 1Z) value; ())

let (remove_fields @ total) : ('k : immutable_data) ('v : immutable_data).
    (s : ('k, 'v) M.state) @ immutable -> (index : int) ->
    {u : unit | (M.remove_slot s index).capacity = s.capacity &&
      (M.remove_slot s index).size = s.size - 1 &&
      (M.remove_slot s index).deleted = s.deleted + 1 &&
      (M.remove_slot s index).slots === S.set s.slots (Bigint.of_int index)
        None &&
      (M.remove_slot s index).controls === (M.set_byte s index 254).controls}
    @ ghost = fun s index -> ghost_ (
  M.remove_slot_def s index;
  M.set_slot_def s index None;
  let slots = M.set_slot s index None in
  M.set_byte_def slots index 254;
  M.set_byte_def s index 254;
  M.set_control_def slots index 254;
  M.set_control_def s index 254;
  if index < 15 then begin
    M.set_control_def (M.set_control slots index 254) (s.capacity + index) 254;
    M.set_control_def (M.set_control s index 254) (s.capacity + index) 254;
    ()
  end;
  M.set_counts_def (M.set_byte slots index 254) (s.size - 1) (s.deleted + 1);
  ())

let (byte_at_primary @ total) : ('k : immutable_data) ('v : immutable_data).
    (s : ('k, 'v) M.state) @ immutable ->
    (index : int) -> (byte : int) -> (query : Bigint.t) ->
    {u : unit | not (16 <= s.capacity && s.capacity <= 1073741824 &&
      S.length s.controls = Bigint.of_int (s.capacity + 15) &&
      0 <= index && index < s.capacity &&
      0Z <= query && query < Bigint.of_int s.capacity) ||
      S.at (M.set_byte s index byte).controls query ===
        (if query = Bigint.of_int index then Some byte else S.at s.controls
          query)}
    @ ghost = fun s index byte query -> ghost_ (
  byte_at s index byte query;
  ())
