type 'a t = 'a Pref.t
type 'a token = 'a Pref.token
type 'a heap = 'a Pref.heap
module Heap = Pref.Heap

type ('v : immutable_data, 'a) step =
  { value : 'v @@ global; state : 'a token @@ ghost }

type 'a partition = { left : 'a token @@ ghost; right : 'a token @@ ghost }

external own : ('a : immutable_data). 'a token @ local immutable ghost -> 'a
  heap @ immutable total ghost
  @@ total = "caml_pref_own_bytecode" "caml_pref_own"

external empty : ('a : immutable_data). unit -> {t : 'a token | own t ===
  Heap.empty ()} @ unique ghost
  @@ total = "caml_pref_empty_bytecode" "caml_pref_empty"

external read : ('a : immutable_data).
  (p : 'a t) @ immutable ->
  (t : {t : 'a token | Heap.mem (own t) p}) @ local read ghost ->
  {v : 'a | let refine_ t = t in Some v === Heap.at (own t) p} @ immutable
  @@ portable = "caml_pref_read_bytecode" "caml_pref_read"

external write : ('a : immutable_data).
  (p : 'a t) @ immutable -> (v : 'a) @ immutable ->
  (t : {t : 'a token | Heap.mem (own t) p}) @ unique read_write ghost ->
  {u : 'a token | let refine_ t = t in
    own u === Heap.put (own t) p v} @ unique ghost
  @@ portable = "caml_pref_write_bytecode" "caml_pref_write"

external split : ('a : immutable_data). (selection : 'a heap) @ immutable ghost
  ->
  (t : 'a token) @ unique ghost ->
  {r : 'a partition | own r.left === Heap.restrict (own t) selection
    && own r.right === Heap.exclude (own t) selection} @ unique
  @@ total = "caml_pref_ghost_split_bytecode" "caml_pref_split"

external join : ('a : immutable_data). (left : 'a token) @ unique ghost ->
  (right : 'a token) @ unique ghost ->
  {t : 'a token | own t === Heap.union (own left) (own right)
    && Heap.disjoint (own left) (own right)} @ unique ghost
  @@ total = "caml_pref_join_bytecode" "caml_pref_join"

external alloc : ('a : immutable_data).
  (value : 'a) @ immutable -> (t : 'a token) @ unique ghost ->
  {r : ('a t, 'a) step | not (Heap.mem (own t) r.value)
    && Heap.mem (own r.state) r.value
    && own r.state === Heap.put (own t) r.value value} @ unique
  @@ portable = "caml_pref_alloc_step_bytecode" "caml_pref_alloc_step"
