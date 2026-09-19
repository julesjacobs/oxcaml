@@ portable

(** Pref operations accepting unique ghost permissions. *)
type 'a t = 'a Pref.t
type token = Pref.token
type heap = Pref.heap
module Heap = Pref.Heap

type ('a : immutable_data) step =
  { value : 'a @@ global; state : token @@ ghost }

type partition = { left : token @@ ghost; right : token @@ ghost }

external own : token @ local immutable ghost -> heap @ immutable total ghost
  @@ total = "caml_pref_own_bytecode" "caml_pref_own"

external empty : unit -> {t : token | own t === Heap.empty ()} @ unique ghost
  @@ total = "caml_pref_empty_bytecode" "caml_pref_empty"

external read : ('a : immutable_data).
  (p : 'a t) @ immutable ->
  (t : {t : token | Heap.mem (own t) p}) @ local read ghost ->
  {v : 'a | let refine_ t = t in Some v === Heap.at (own t) p} @ immutable
  = "caml_pref_read_bytecode" "caml_pref_read"

external write : ('a : immutable_data).
  (p : 'a t) @ immutable -> (v : 'a) @ immutable ->
  (t : {t : token | Heap.mem (own t) p}) @ unique read_write ghost ->
  {u : token | let refine_ t = t in
    own u === Heap.put (own t) p v} @ unique ghost
  = "caml_pref_write_bytecode" "caml_pref_write"

external split : (selection : heap) @ immutable ghost ->
  (t : token) @ unique ghost ->
  {r : partition | own r.left === Heap.restrict (own t) selection
    && own r.right === Heap.exclude (own t) selection} @ unique
  @@ total = "caml_pref_ghost_split_bytecode" "caml_pref_split"

external join : (left : token) @ unique ghost -> (right : token) @ unique ghost ->
  {t : token | own t === Heap.union (own left) (own right)
    && Heap.disjoint (own left) (own right)} @ unique ghost
  @@ total = "caml_pref_join_bytecode" "caml_pref_join"

val alloc : ('a : immutable_data).
  (value : 'a) @ immutable -> (t : token) @ unique ghost ->
  {r : 'a t step | not (Heap.mem (own t) r.value)
    && own r.state === Heap.put (own t) r.value value} @ unique
