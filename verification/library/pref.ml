type ('a : immutable_data) t : immutable_data
type ('a : immutable_data) pref = 'a t
type token : void mod total contended
type heap : immutable_data

module Data = struct
  type 'a t : void mod everything

  external int : unit -> int t @@ total = "%unbox_unit"
  external bool : unit -> bool t @@ total = "%unbox_unit"
  external unit : unit -> unit t @@ total = "%unbox_unit"
  external string : unit -> string t @@ total = "%unbox_unit"
  external pref : ('a : immutable_data).
    unit -> 'a pref t @@ total = "%unbox_unit"
  external option : ('a : immutable_data).
    'a t -> 'a option t @@ total = "%identity"
  external list : ('a : immutable_data).
    'a t -> 'a list t @@ total = "%identity"
  external witness : unit -> 'a t @@ total = "%unbox_unit"
  let (pair @ total) : ('a : immutable_data) ('b : immutable_data).
    'a t -> 'b t -> ('a * 'b) t = fun _ _ -> witness ()
end

type ('a : immutable_data) step =
  { value : 'a @@ global; state : token }

external own : token @ local immutable -> heap @ immutable total ghost
  @@ total = "caml_pref_own_bytecode" "caml_pref_own"

module Heap = struct
  external empty : unit -> heap @ immutable total ghost
    @@ total = "caml_pref_heap_empty"
  external mem : ('a : immutable_data).
    heap @ immutable -> 'a t @ immutable -> bool @ ghost
    @@ total = "caml_pref_heap_mem"
  external at : ('a : immutable_data).
    heap @ immutable -> 'a t @ immutable -> 'a option @ immutable total ghost
    @@ total = "caml_pref_heap_at"
  external put : ('a : immutable_data).
    heap @ immutable -> 'a t @ immutable -> 'a @ immutable ->
    heap @ immutable total ghost
    @@ total = "caml_pref_heap_put"
end

module Raw = struct
  external empty : unit ->
    {t : token | own t === Heap.empty ()} @ unique
    @@ total = "caml_pref_empty_bytecode" "caml_pref_empty"
  external alloc : ('a : immutable_data).
    'a @ immutable -> 'a t @@ portable = "caml_pref_alloc"
  external attach : ('a : immutable_data).
    (p : 'a t) @ immutable -> (v : 'a) @ immutable ->
    (t : token) @ unique ->
    {u : token | not (Heap.mem (own t) p)
      && own u === Heap.put (own t) p v} @ unique
    @@ total = "caml_pref_attach_bytecode" "caml_pref_attach"
end

let empty = Raw.empty

let alloc : ('a : immutable_data).
  'a Data.t @ ghost ->
  (value : 'a) @ immutable -> (t : token) @ unique ->
  {r : 'a t step | not (Heap.mem (own t) r.value)
    && own r.state === Heap.put (own t) r.value value} @ unique =
  fun _data value t ->
    let p = Raw.alloc value in
    let refine_ state = Raw.attach p value t in
    let r = { value = p; state } in
    refine_ r

external read : ('a : immutable_data).
  (p : 'a t) @ immutable ->
  (t : {t : token | Heap.mem (own t) p}) @ local read ->
  {v : 'a | let refine_ t = t in Some v === Heap.at (own t) p} @ immutable
  @@ total = "caml_pref_read_bytecode" "caml_pref_read"

external write : ('a : immutable_data).
  (p : 'a t) @ immutable -> (v : 'a) @ immutable ->
  (t : {t : token | Heap.mem (own t) p}) @ unique read_write ->
  {u : token | let refine_ t = t in
    own u === Heap.put (own t) p v} @ unique
  @@ total = "caml_pref_write_bytecode" "caml_pref_write"
