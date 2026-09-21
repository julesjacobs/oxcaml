type 'a t : immutable_data [@@phantom_parameters]
type 'a pref = 'a t
type 'a token : void mod total contended
type 'a heap : immutable_data

type ('v : immutable_data, 'a) step =
  { value : 'v @@ global; state : 'a token }

external own : ('a : immutable_data). 'a token @ local immutable -> 'a heap @
  immutable total ghost
  @@ total = "caml_pref_own_bytecode" "caml_pref_own"

module Heap = struct
  external empty : ('a : immutable_data). unit -> 'a heap @ immutable total
    ghost
    @@ total = "caml_pref_heap_empty"
  external mem : ('a : immutable_data).
    'a heap @ immutable -> 'a t @ immutable -> bool @ ghost
    @@ total = "caml_pref_heap_mem"
  external at : ('a : immutable_data).
    'a heap @ immutable -> 'a t @ immutable -> 'a option @ immutable total ghost
    @@ total = "caml_pref_heap_at"
  external put : ('a : immutable_data).
    'a heap @ immutable -> 'a t @ immutable -> 'a @ immutable ->
    'a heap @ immutable total ghost
    @@ total = "caml_pref_heap_put"
end

module Raw = struct
  external empty : ('a : immutable_data). unit ->
    {t : 'a token | own t === Heap.empty ()} @ unique
    @@ total = "caml_pref_empty_bytecode" "caml_pref_empty"
  external alloc : ('a : immutable_data).
    'a @ immutable -> 'a t @@ portable = "caml_pref_alloc"
  external attach : ('a : immutable_data).
    (p : 'a t) @ immutable -> (v : 'a) @ immutable ->
    (t : 'a token) @ unique ->
    {u : 'a token | not (Heap.mem (own t) p)
      && Heap.mem (own u) p
      && own u === Heap.put (own t) p v} @ unique
    @@ total = "caml_pref_attach_bytecode" "caml_pref_attach"
end

let empty = Raw.empty

let alloc : ('a : immutable_data).
  (value : 'a) @ immutable -> (t : 'a token) @ unique ->
  {r : ('a t, 'a) step | not (Heap.mem (own t) r.value)
    && Heap.mem (own r.state) r.value
    && own r.state === Heap.put (own t) r.value value} @ unique =
  fun value t ->
    let p = Raw.alloc value in
    let refine_ state = Raw.attach p value t in
    let r = { value = p; state } in
    refine_ r

external read : ('a : immutable_data).
  (p : 'a t) @ immutable ->
  (t : {t : 'a token | Heap.mem (own t) p}) @ local read ->
  {v : 'a | let refine_ t = t in Some v === Heap.at (own t) p} @ immutable
  @@ portable = "caml_pref_read_bytecode" "caml_pref_read"

external write : ('a : immutable_data).
  (p : 'a t) @ immutable -> (v : 'a) @ immutable ->
  (t : {t : 'a token | Heap.mem (own t) p}) @ unique read_write ->
  {u : 'a token | let refine_ t = t in
    own u === Heap.put (own t) p v} @ unique
  @@ portable = "caml_pref_write_bytecode" "caml_pref_write"

