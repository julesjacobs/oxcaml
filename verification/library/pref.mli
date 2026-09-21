@@ portable

type 'a t : immutable_data [@@phantom_parameters]
type 'a pref = 'a t
(** Affine heap ownership. [void] erases its representation while retaining
    uniqueness, access, and ghostliness checking. Bytecode uses the existing
    [void] unit placeholders; native code has no token fields or arguments. *)
type 'a token : void mod total contended
type 'a heap : immutable_data

type ('v : immutable_data, 'a) step =
  { value : 'v @@ global; state : 'a token }

(** The finite map at this token occurrence. Saved observations remain valid
    after a write consumes the token. Observations do not grant access. *)
external own : ('a : immutable_data). 'a token @ local immutable -> 'a heap @
  immutable total ghost
  @@ total = "caml_pref_own_bytecode" "caml_pref_own"

module Heap : sig
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

val empty : ('a : immutable_data). unit -> {t : 'a token | own t === Heap.empty
  ()} @ unique
  @@ total

(** Allocate a fresh cell and extend the token. Allocation is partial because
    it creates observable identity. *)
val alloc : ('a : immutable_data).
  (value : 'a) @ immutable -> (t : 'a token) @ unique ->
  {r : ('a t, 'a) step | not (Heap.mem (own t) r.value)
    && Heap.mem (own r.state) r.value
    && own r.state === Heap.put (own t) r.value value} @ unique

(** Read under a temporary borrow. Payloads are shared immutable values.
    Reads and writes are partial, including for higher-order payloads. *)
external read : ('a : immutable_data).
  (p : 'a t) @ immutable ->
  (t : {t : 'a token | Heap.mem (own t) p}) @ local read ->
  {v : 'a | let refine_ t = t in Some v === Heap.at (own t) p} @ immutable
  = "caml_pref_read_bytecode" "caml_pref_read"

(** Consume writable ownership and return the updated finite map. *)
external write : ('a : immutable_data).
  (p : 'a t) @ immutable -> (v : 'a) @ immutable ->
  (t : {t : 'a token | Heap.mem (own t) p}) @ unique read_write ->
  {u : 'a token | let refine_ t = t in
    own u === Heap.put (own t) p v} @ unique
  = "caml_pref_write_bytecode" "caml_pref_write"

