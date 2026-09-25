module P = Ghost_pref
module H = P.Heap
module M = Vox_table_model

type ('k : immutable_data, 'v : immutable_data) t : immutable_data
type ('k : immutable_data, 'v : immutable_data) view =
  { model : ('k, 'v) M.state @@ ghost }

type ('k : immutable_data) stored_key = { key : 'k @@ ghost }

(** One logical region owns the header and both backing blocks. Only the
    handle escapes; callers cannot separately access its arrays. *)
external location : ('k, 'v) t @ immutable ->
  ('k, 'v) M.state P.t @ immutable ghost
  @@ total = "caml_vox_table_location"

external create : (capacity : {n : int | 16 <= n && n <= 1073741824}) ->
  (token : P.token) @ unique ghost ->
  {r : ('k, 'v) t P.step |
    not (H.mem (P.own token) (location r.value)) &&
    P.own r.state === H.put (P.own token) (location r.value)
      (M.initial capacity None)} @ unique
  @@ portable = "caml_vox_table_create_bytecode" "caml_vox_table_create"

external capacity : (table : ('k, 'v) t) @ immutable ->
  (state : ('k, 'v) view) @ immutable ->
  (token : {t : P.token |
    H.at (P.own t) (location table) === Some state.model}) @ local read ghost ->
  {n : int | n = state.model.capacity}
  @@ portable = "caml_vox_table_capacity_bytecode" "caml_vox_table_capacity"
    [@@noalloc] [@@builtin] [@@no_effects]

external size : (table : ('k, 'v) t) @ immutable ->
  (state : ('k, 'v) view) @ immutable ->
  (token : {t : P.token |
    H.at (P.own t) (location table) === Some state.model}) @ local read ghost ->
  {n : int | n = state.model.size}
  @@ portable = "caml_vox_table_size_bytecode" "caml_vox_table_size"
    [@@noalloc] [@@builtin] [@@no_effects]

external deleted : (table : ('k, 'v) t) @ immutable ->
  (state : ('k, 'v) view) @ immutable ->
  (token : {t : P.token |
    H.at (P.own t) (location table) === Some state.model}) @ local read ghost ->
  {n : int | n = state.model.deleted}
  @@ portable = "caml_vox_table_deleted_bytecode" "caml_vox_table_deleted"
    [@@noalloc] [@@builtin] [@@no_effects]

external read_control : (table : ('k, 'v) t) @ immutable ->
  (state : ('k, 'v) view) @ immutable ->
  (index : {i : int | 0 <= i && i < state.model.capacity + 15}) ->
  (token : {t : P.token |
    H.at (P.own t) (location table) === Some state.model}) @ local read ghost ->
  {byte : int | M.control state.model index === Some byte}
  @@ portable = "caml_vox_table_read_control_bytecode"
    "caml_vox_table_read_control" [@@noalloc] [@@builtin] [@@no_effects]

external write_control : (table : ('k, 'v) t) @ immutable ->
  (state : ('k, 'v) view) @ immutable ->
  (index : {i : int | 0 <= i && i < state.model.capacity + 15}) ->
  (byte : {b : int | 0 <= b && b <= 255}) ->
  (token : {t : P.token |
    H.at (P.own t) (location table) === Some state.model})
    @ unique read_write ghost ->
  {t : P.token | P.own t === H.put (P.own token) (location table)
    (M.set_control state.model index byte)} @ unique ghost
  @@ portable = "caml_vox_table_write_control_bytecode"
    "caml_vox_table_write_control" [@@noalloc] [@@builtin]

external write_slot : (table : ('k, 'v) t) @ immutable ->
  (state : ('k, 'v) view) @ immutable ->
  (index : {i : int | 0 <= i && i < state.model.capacity}) ->
  (key : 'k) @ immutable -> (value : 'v) @ immutable ->
  (token : {t : P.token |
    H.at (P.own t) (location table) === Some state.model})
    @ unique read_write ghost ->
  {t : P.token | P.own t === H.put (P.own token) (location table)
    (M.set_slot state.model index (Some (key, value)))} @ unique ghost
  @@ portable = "caml_vox_table_write_slot_bytecode"
    "caml_vox_table_write_slot" [@@noalloc]

external write_value : (table : ('k, 'v) t) @ immutable ->
  (state : ('k, 'v) view) @ immutable ->
  (index : {i : int | 0 <= i && i < state.model.capacity}) ->
  (stored : {s : 'k stored_key | match M.slot state.model index with
    | Some (Some (key, _)) -> key === s.key | _ -> false}) @ immutable ->
  (value : 'v) @ immutable ->
  (token : {t : P.token |
    H.at (P.own t) (location table) === Some state.model})
    @ unique read_write ghost ->
  {t : P.token | P.own t === H.put (P.own token) (location table)
    (M.set_slot state.model index (Some (stored.key, value)))} @ unique ghost
  @@ portable = "caml_vox_table_write_value_bytecode"
    "caml_vox_table_write_value" [@@noalloc]

external clear_slot : (table : ('k, 'v) t) @ immutable ->
  (state : ('k, 'v) view) @ immutable ->
  (index : {i : int | 0 <= i && i < state.model.capacity}) ->
  (token : {t : P.token |
    H.at (P.own t) (location table) === Some state.model})
    @ unique read_write ghost ->
  {t : P.token | P.own t === H.put (P.own token) (location table)
    (M.set_slot state.model index None)} @ unique ghost
  @@ portable = "caml_vox_table_clear_slot_bytecode"
    "caml_vox_table_clear_slot" [@@noalloc]

external set_counts : (table : ('k, 'v) t) @ immutable ->
  (state : ('k, 'v) view) @ immutable ->
  (size : int) -> (deleted : int) ->
  (token : {t : P.token |
    H.at (P.own t) (location table) === Some state.model})
    @ unique read_write ghost ->
  {t : P.token | P.own t === H.put (P.own token) (location table)
    (M.set_counts state.model size deleted)} @ unique ghost
  @@ portable = "caml_vox_table_set_counts_bytecode"
    "caml_vox_table_set_counts" [@@noalloc] [@@builtin]

external read_key : (table : ('k, 'v) t) @ immutable ->
  (state : ('k, 'v) view) @ immutable ->
  (index : {i : int | 0 <= i && i < state.model.capacity}) ->
  (token : {t : P.token |
    H.at (P.own t) (location table) === Some state.model &&
    (match M.slot state.model index with
     | Some (Some _) -> true | _ -> false)}) @ local read ghost ->
  {key : 'k | match M.slot state.model index with
    | Some (Some (k, _)) -> key === k | _ -> false} @ immutable
  @@ portable = "caml_vox_table_read_key_bytecode" "caml_vox_table_read_key"
    [@@noalloc] [@@builtin] [@@no_effects]

external read_value : (table : ('k, 'v) t) @ immutable ->
  (state : ('k, 'v) view) @ immutable ->
  (index : {i : int | 0 <= i && i < state.model.capacity}) ->
  (token : {t : P.token |
    H.at (P.own t) (location table) === Some state.model &&
    (match M.slot state.model index with
     | Some (Some _) -> true | _ -> false)}) @ local read ghost ->
  {value : 'v | match M.slot state.model index with
    | Some (Some (_, v)) -> value === v | _ -> false} @ immutable
  @@ portable = "caml_vox_table_read_value_bytecode"
    "caml_vox_table_read_value"
    [@@noalloc] [@@builtin] [@@no_effects]

(** Exact observation of the 16 physical control bytes, including clones. *)
external match16 : (table : ('k, 'v) t) @ immutable ->
  (state : ('k, 'v) view) @ immutable ->
  (offset : {i : int | 0 <= i && i < state.model.capacity}) ->
  (byte : {b : int | 0 <= b && b <= 255}) ->
  (token : {t : P.token |
    H.at (P.own t) (location table) === Some state.model}) @ local read ghost ->
  {bits : int | 0 <= bits && bits <= 65535 &&
    bits = M.matching state.model offset byte 16}
  @@ portable = "caml_vox_table_match16_bytecode" "caml_vox_table_match16"
    [@@noalloc] [@@builtin]

(** Low 16 bits match [byte]; bit 16 records an empty lane. *)
external match16_empty : (table : ('k, 'v) t) @ immutable ->
  (state : ('k, 'v) view) @ immutable ->
  (offset : {i : int | 0 <= i && i < state.model.capacity}) ->
  (byte : {b : int | 0 <= b && b <= 255}) ->
  (token : {t : P.token |
    H.at (P.own t) (location table) === Some state.model}) @ local read ghost ->
  {bits : int | 0 <= bits && bits <= 131071 &&
    bits land 65535 = M.matching state.model offset byte 16 &&
    (bits land 65536 <> 0) = (M.matching state.model offset 128 16 <> 0)}
  @@ portable = "caml_vox_table_match16_empty_bytecode" "caml_vox_table_match16_empty"
    [@@noalloc] [@@builtin] [@@no_effects]

external exchange : (left : ('k, 'v) t) @ immutable ->
  (before_left : ('k, 'v) view) @ immutable ->
  (right : ('k, 'v) t) @ immutable ->
  (before_right : ('k, 'v) view) @ immutable ->
  (token : {t : P.token |
    H.at (P.own t) (location left) === Some before_left.model &&
    H.at (P.own t) (location right) === Some before_right.model})
    @ unique read_write ghost ->
  {t : P.token | P.own t ===
    H.put (H.put (P.own token) (location left) before_right.model)
      (location right) before_left.model} @ unique ghost
  @@ portable = "caml_vox_table_exchange_bytecode" "caml_vox_table_exchange"
    [@@noalloc]

(** Bulk initialization of the private storage, with write barriers for
    every scanned word that stops retaining a key or value. *)
external clear : (table : ('k, 'v) t) @ immutable ->
  (before : ('k, 'v) view) @ immutable ->
  (token : {t : P.token |
    H.at (P.own t) (location table) === Some before.model})
    @ unique read_write ghost ->
  {t : P.token | P.own t === H.put (P.own token) (location table)
    (M.initial before.model.capacity None)} @ unique ghost
  @@ portable = "caml_vox_table_clear_bytecode" "caml_vox_table_clear"
    [@@noalloc]

(** Publish private replacement storage and discard its ownership. The source
    token owns exactly the temporary table region; other regions stay owned
    by the destination token. *)
external replace_storage : (destination : ('k, 'v) t) @ immutable ->
  (before : ('k, 'v) view) @ immutable ->
  (source : ('k, 'v) t) @ immutable ->
  (replacement : ('k, 'v) view) @ immutable ->
  (source_token : {t : P.token | P.own t ===
    H.put (H.empty ()) (location source) replacement.model}) @ unique
      read_write ghost ->
  (token : {t : P.token | H.at (P.own t) (location destination) === Some
    before.model})
    @ unique read_write ghost ->
  {t : P.token | P.own t === H.put (P.own token) (location destination)
    replacement.model}
    @ unique ghost
  @@ portable = "caml_vox_table_replace_storage_bytecode"
    "caml_vox_table_exchange" [@@noalloc]
