type 'a t : immutable_data
type 'a pref = 'a t
type token : void mod total contended
type heap : immutable_data


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
  external union : heap @ immutable -> heap @ immutable ->
    heap @ immutable total ghost @@ total = "caml_pref_heap_union"
  external restrict : heap @ immutable -> heap @ immutable ->
    heap @ immutable total ghost @@ total = "caml_pref_heap_restrict"
  external exclude : heap @ immutable -> heap @ immutable ->
    heap @ immutable total ghost @@ total = "caml_pref_heap_exclude"
  external disjoint : heap @ immutable -> heap @ immutable -> bool @ ghost
    @@ total = "caml_pref_heap_disjoint"
  external same_domain : heap @ immutable -> heap @ immutable -> bool @ ghost
    @@ total = "caml_pref_heap_same_domain"

  external partition_law : (a : heap) @ immutable -> (b : heap) @ immutable ->
    {u : unit | not (disjoint a b) ||
      (restrict (union a b) a === a && exclude (union a b) a === b)} @ ghost
    @@ total = "caml_pref_heap_law2"
  external union_law : (a : heap) @ immutable -> (b : heap) @ immutable ->
    (c : heap) @ immutable ->
    {u : unit |
      union (empty ()) a === a && union a (empty ()) === a
      && union (union a b) c === union a (union b c)
      && same_domain (union a b) (union b a)
      && (not (disjoint a b) || union a b === union b a)
      && disjoint a b = disjoint b a
      && disjoint a (union b c) = (disjoint a b && disjoint a c)} @ ghost
    @@ total = "caml_pref_heap_law3"
  external domain_law : (a : heap) @ immutable -> (b : heap) @ immutable ->
    (c : heap) @ immutable ->
    {u : unit | same_domain a a
      && (not (same_domain a b) ||
        (same_domain b a && disjoint a c = disjoint b c
         && same_domain (union a c) (union b c)
         && same_domain (union c a) (union c b)))
      && (not (same_domain a b && same_domain b c) || same_domain a c)} @ ghost
    @@ total = "caml_pref_heap_law3"
  external union_domain_law :
    (a : heap) @ immutable -> (b : heap) @ immutable ->
    (c : heap) @ immutable -> (d : heap) @ immutable ->
    {u : unit | not (same_domain a b && same_domain c d)
      || same_domain (union a c) (union b d)} @ ghost
    @@ total = "caml_pref_heap_law4"
  external put_law : ('a : immutable_data).
    (h : heap) @ immutable -> (p : 'a t) @ immutable ->
    (x : 'a) @ immutable -> (y : 'a) @ immutable ->
    {u : unit | put (put h p x) p y === put h p y
      && (not (at h p === Some x) || put h p x === h)
      && same_domain (put h p x) (put h p y)
      && (not (mem h p) || same_domain (put h p x) h)} @ ghost
    @@ total = "caml_pref_heap_law4"
  external put_union_law : ('a : immutable_data).
    (a : heap) @ immutable -> (b : heap) @ immutable ->
    (p : 'a t) @ immutable -> (x : 'a) @ immutable ->
    {u : unit | put (union a b) p x === union (put a p x) b} @ ghost
    @@ total = "caml_pref_heap_law4"
  external commute_law : ('a : immutable_data) ('b : immutable_data).
    (h : heap) @ immutable -> (p : 'a t) @ immutable ->
    (x : 'a) @ immutable -> (q : 'b t) @ immutable -> (y : 'b) @ immutable ->
    {u : unit | mem (put (empty ()) p x) q
      || put (put h p x) q y === put (put h q y) p x} @ ghost
    @@ total = "caml_pref_heap_law5"

  external split_law : (h : heap) @ immutable ->
    (selection : heap) @ immutable ->
    {u : unit | union (restrict h selection) (exclude h selection) === h
      && disjoint (restrict h selection) (exclude h selection)} @ ghost
    @@ total = "caml_pref_heap_law2"
  external exclude_put_law : ('a : immutable_data).
    (h : heap) @ immutable -> (selection : heap) @ immutable ->
    (p : 'a t) @ immutable -> (v : 'a) @ immutable ->
    {u : unit | not (mem selection p) ||
      exclude (put h p v) selection === exclude h selection} @ ghost
    @@ total = "caml_pref_heap_law4"
  external exclude_union_law : (a : heap) @ immutable ->
    (b : heap) @ immutable -> (selection : heap) @ immutable ->
    {u : unit | exclude (union a b) selection ===
      union (exclude a selection) (exclude b selection)} @ ghost
    @@ total = "caml_pref_heap_law3"

end

external empty : unit -> {t : token | own t === Heap.empty ()} @ unique
  @@ total = "caml_pref_empty_bytecode" "caml_pref_empty"

external alloc : ('a : immutable_data).
  (value : 'a) @ immutable -> (t : token) @ unique ->
  {r : 'a t step | not (Heap.mem (own t) r.value)
    && own r.state === Heap.put (own t) r.value value} @ unique
  @@ portable = "caml_pref_alloc_step_bytecode" "caml_pref_alloc_step"

external read : ('a : immutable_data).
  (p : 'a t) @ immutable ->
  (t : {t : token | Heap.mem (own t) p}) @ local read ->
  {v : 'a | let refine_ t = t in Some v === Heap.at (own t) p} @ immutable
  @@ portable = "caml_pref_read_bytecode" "caml_pref_read"

external write : ('a : immutable_data).
  (p : 'a t) @ immutable -> (v : 'a) @ immutable ->
  (t : {t : token | Heap.mem (own t) p}) @ unique read_write ->
  {u : token | let refine_ t = t in
    own u === Heap.put (own t) p v} @ unique
  @@ portable = "caml_pref_write_bytecode" "caml_pref_write"

(** Runtime identity comparison for handles. *)
external equal : ('a : immutable_data).
  (p : 'a t) @ immutable -> (q : 'a t) @ immutable ->
  {b : bool | b = (p === q)} @ total
  @@ portable total = "%eq"

type partition = #{ left : token; right : token }

external split : (selection : heap) @ immutable ghost -> (t : token) @ unique ->
  {r : partition |
    own r.#left === Heap.restrict (own t) selection
    && own r.#right === Heap.exclude (own t) selection} @ unique
  @@ total = "caml_pref_split_bytecode" "caml_pref_split"

external join : (left : token) @ unique -> (right : token) @ unique ->
  {t : token | own t === Heap.union (own left) (own right)
    && Heap.disjoint (own left) (own right)} @ unique
  @@ total = "caml_pref_join_bytecode" "caml_pref_join"
