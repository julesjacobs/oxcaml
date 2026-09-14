@@ portable

type ('a : immutable_data) t : immutable_data
type ('a : immutable_data) pref = 'a t
(** Affine heap ownership. [void] erases its representation while retaining
    uniqueness, access, and ghostliness checking. Bytecode uses the existing
    [void] unit placeholders; native code has no token fields or arguments. *)
type token : void mod total contended
type heap : immutable_data

(** Erased evidence that a payload type contains only immutable data and
    passive pref handles. [immutable_data] alone also admits annotated function
    fields, which would permit heap-mediated recursion through total reads.
    Allocation requires this evidence; the invariant pref parameter retains it.
    There is deliberately no constructor for functions or arbitrary records. *)
module Data : sig
  type 'a t : void mod everything

  val int : unit -> int t @@ total
  val bool : unit -> bool t @@ total
  val unit : unit -> unit t @@ total
  val string : unit -> string t @@ total
  val pref : ('a : immutable_data). unit -> 'a pref t @@ total
  val option : ('a : immutable_data). 'a t -> 'a option t @@ total
  val list : ('a : immutable_data). 'a t -> 'a list t @@ total
  val pair : ('a : immutable_data) ('b : immutable_data).
    'a t -> 'b t -> ('a * 'b) t @@ total
end

type ('a : immutable_data) step =
  { value : 'a @@ global; state : token }

(** The finite map at this token occurrence. Saved observations remain valid
    after a write consumes the token. Observations do not grant access. *)
external own : token @ local immutable -> heap @ immutable total ghost
  @@ total = "caml_pref_own_bytecode" "caml_pref_own"

module Heap : sig
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

val empty : unit -> {t : token | own t === Heap.empty ()} @ unique
  @@ total

(** Allocate a fresh cell and extend the token. Allocation is partial because
    it creates observable identity. *)
val alloc : ('a : immutable_data).
  'a Data.t @ ghost ->
  (value : 'a) @ immutable -> (t : token) @ unique ->
  {r : 'a t step | not (Heap.mem (own t) r.value)
    && own r.state === Heap.put (own t) r.value value} @ unique

(** Read under a temporary borrow. Payloads are shared immutable values. *)
external read : ('a : immutable_data).
  (p : 'a t) @ immutable ->
  (t : {t : token | Heap.mem (own t) p}) @ local read ->
  {v : 'a | let refine_ t = t in Some v === Heap.at (own t) p} @ immutable
  @@ total = "caml_pref_read_bytecode" "caml_pref_read"

(** Consume writable ownership and return the updated finite map. *)
external write : ('a : immutable_data).
  (p : 'a t) @ immutable -> (v : 'a) @ immutable ->
  (t : {t : token | Heap.mem (own t) p}) @ unique read_write ->
  {u : token | let refine_ t = t in
    own u === Heap.put (own t) p v} @ unique
  @@ total = "caml_pref_write_bytecode" "caml_pref_write"
