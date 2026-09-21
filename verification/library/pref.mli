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
  external union : ('a : immutable_data). 'a heap @ immutable -> 'a heap @
    immutable ->
    'a heap @ immutable total ghost @@ total = "caml_pref_heap_union"
  external restrict : ('a : immutable_data). 'a heap @ immutable -> 'a heap @
    immutable ->
    'a heap @ immutable total ghost @@ total = "caml_pref_heap_restrict"
  external exclude : ('a : immutable_data). 'a heap @ immutable -> 'a heap @
    immutable ->
    'a heap @ immutable total ghost @@ total = "caml_pref_heap_exclude"
  external disjoint : ('a : immutable_data). 'a heap @ immutable -> 'a heap @
    immutable -> bool @ ghost
    @@ total = "caml_pref_heap_disjoint"
  external same_domain : ('a : immutable_data). 'a heap @ immutable -> 'a heap @
    immutable -> bool @ ghost
    @@ total = "caml_pref_heap_same_domain"

  external partition_law : ('a : immutable_data). (a : 'a heap) @ immutable ->
    (b : 'a heap) @ immutable ->
    {u : unit | not (disjoint a b) ||
      (restrict (union a b) a === a && exclude (union a b) a === b)} @ ghost
    @@ total = "caml_pref_heap_law2"
  external union_law : ('a : immutable_data). (a : 'a heap) @ immutable -> (b :
    'a heap) @ immutable ->
    (c : 'a heap) @ immutable ->
    {u : unit |
      union (empty ()) a === a && union a (empty ()) === a
      && union (union a b) c === union a (union b c)
      && same_domain (union a b) (union b a)
      && (not (disjoint a b) || union a b === union b a)
      && disjoint a b = disjoint b a
      && disjoint a (union b c) = (disjoint a b && disjoint a c)} @ ghost
    @@ total = "caml_pref_heap_law3"
  external domain_law : ('a : immutable_data). (a : 'a heap) @ immutable -> (b :
    'a heap) @ immutable ->
    (c : 'a heap) @ immutable ->
    {u : unit | same_domain a a
      && (not (same_domain a b) ||
        (same_domain b a && disjoint a c = disjoint b c
         && same_domain (union a c) (union b c)
         && same_domain (union c a) (union c b)))
      && (not (same_domain a b && same_domain b c) || same_domain a c)} @ ghost
    @@ total = "caml_pref_heap_law3"
  external union_domain_law :
    ('a : immutable_data). (a : 'a heap) @ immutable -> (b : 'a heap) @
      immutable ->
    (c : 'a heap) @ immutable -> (d : 'a heap) @ immutable ->
    {u : unit | not (same_domain a b && same_domain c d)
      || same_domain (union a c) (union b d)} @ ghost
    @@ total = "caml_pref_heap_law4"
  external put_law : ('a : immutable_data).
    (h : 'a heap) @ immutable -> (p : 'a t) @ immutable ->
    (x : 'a) @ immutable -> (y : 'a) @ immutable ->
    {u : unit | put (put h p x) p y === put h p y
      && same_domain (put h p x) (put h p y)
      && (not (mem h p) || same_domain (put h p x) h)} @ ghost
    @@ total = "caml_pref_heap_law4"
  external put_union_law : ('a : immutable_data).
    (a : 'a heap) @ immutable -> (b : 'a heap) @ immutable ->
    (p : 'a t) @ immutable -> (x : 'a) @ immutable ->
    {u : unit | put (union a b) p x === union (put a p x) b} @ ghost
    @@ total = "caml_pref_heap_law4"
  external commute_law : ('a : immutable_data).
    (h : 'a heap) @ immutable -> (p : 'a t) @ immutable ->
    (x : 'a) @ immutable -> (q : 'a t) @ immutable -> (y : 'a) @ immutable ->
    {u : unit | mem (put (empty ()) p x) q
      || put (put h p x) q y === put (put h q y) p x} @ ghost
    @@ total = "caml_pref_heap_law5"

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

(** Runtime identity comparison for handles. *)
external equal : ('a : immutable_data).
  (p : 'a t) @ immutable -> (q : 'a t) @ immutable ->
  {b : bool | b = (p === q)} @ total
  @@ total = "%eq"

type 'a partition = #{ left : 'a token; right : 'a token }

(** Divide ownership by the domain of [selection]. Its values are ignored. *)
external split : ('a : immutable_data). (selection : 'a heap) @ immutable ghost
  -> (t : 'a token) @ unique ->
  {r : 'a partition |
    own r.#left === Heap.restrict (own t) selection
    && own r.#right === Heap.exclude (own t) selection} @ unique
  @@ total = "caml_pref_split_bytecode" "caml_pref_split"

(** Combine two live token occurrences. Their ownership is disjoint. *)
external join : ('a : immutable_data). (left : 'a token) @ unique -> (right : 'a
  token) @ unique ->
  {t : 'a token | own t === Heap.union (own left) (own right)
    && Heap.disjoint (own left) (own right)} @ unique
  @@ total = "caml_pref_join_bytecode" "caml_pref_join"
