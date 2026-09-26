@@ portable

(** Explicitly freed byte buffers with GC-backed reclamation. External memory operations and location
    identity laws are trusted; coverage and range lemmas are verified.
    Allocation failure returns the original token on normal return. Managed
    allocation and identity exhaustion may raise Out_of_memory.
    Exceptions consume the passed authority; handlers must not restore it.
    Split unrelated ownership before a fallible call to retain that frame.
    [free] releases storage promptly. A finalizer also releases storage once
    the descriptor is unreachable, including after exceptional exits. Dropping
    a token alone does not release storage while the descriptor is reachable.
    Finalizer timing is unspecified; neither reclamation nor exception safety
    is part of the normal-return refinement contracts. *)
module P = Ghost_pref
module H = P.Heap

type t : immutable_data
type byte : immediate = {v : int | 0 <= v && v <= 255}
type contents : immutable_data = byte option
type allocation : immutable_data = t option

external length : t @ local immutable -> {n : int | n >= 0}
  @@ total = "caml_raw_memory_length" [@@noalloc] [@@builtin] [@@no_effects]

external equal : (p : t) @ immutable -> (q : t) @ immutable ->
  {b : bool | b = (p === q)} @ total
  @@ total = "%eq"

(* These locations exist only in the ownership model. Index -1 is the
   deallocation permission; nonnegative indices describe bytes. *)
external location : t @ immutable -> int -> contents P.t @ immutable ghost
  @@ total = "caml_raw_memory_location"
external location_law : (p : t) @ immutable -> (q : t) @ immutable ->
  (i : int) -> (j : int) ->
  {u : unit | H.mem (H.put (H.empty ()) (location p i) None)
    (location q j) = (p === q && i = j)} @ ghost
  @@ total = "caml_pref_heap_law4"

(** Ghost selectors contain no runtime byte array. Index -1 is reserved
    for the deallocation permission. [range] selects [lo, hi). *)
val range : t @ immutable -> int -> int -> contents P.heap @ ghost @@ total
val range_def : (p : t) @ immutable -> (lo : int) -> (hi : int) ->
  {u : unit | range p lo hi ===
    (ghost_ (if 0 <= lo && lo < hi then
      H.put (range p lo (hi - 1)) (location p (hi - 1)) None
     else H.empty ()))} @@ total
val footprint : t @ immutable -> contents P.heap @ ghost @@ total
val footprint_def : (p : t) @ immutable ->
  {u : unit | footprint p ===
    (ghost_ (H.put (range p 0 (length p)) (location p (-1)) None))} @@ total

(** Coverage refers to this heap snapshot; it confers no ownership. *)
val covers : contents P.heap @ immutable -> t @ immutable -> int -> int -> bool
  @ ghost
  @@ total
val covers_def : (h : contents P.heap) @ immutable -> (p : t) @ immutable ->
  (lo : int) -> (hi : int) ->
  {u : unit | covers h p lo hi ===
    (ghost_ (if 0 <= lo && lo < hi then
      H.mem h (location p (hi - 1)) && covers h p lo (hi - 1)
     else true))} @@ total

(** Add all bytes and the deallocation permission to an arbitrary token. *)
external malloc : (n : {n : int | n >= 0}) ->
  (token : contents P.token) @ unique ghost ->
  {r : (allocation, contents) P.step | match r.value with
    | None -> P.own r.state === P.own token
    | Some p -> length p = n && H.disjoint (footprint p) (P.own token)
      && P.own r.state === H.union (footprint p) (P.own token)} @ unique
  = "caml_raw_memory_malloc_bytecode" "caml_raw_memory_malloc"

external read : (p : t) @ immutable ->
  (i : {i : int | 0 <= i && i < length p}) ->
  (token : {s : contents P.token | H.mem (P.own s) (location p i) &&
    match H.at (P.own s) (location p i) with
    | Some (Some _) -> true | _ -> false}) @ local read ghost ->
  {v : byte | H.at (P.own token) (location p i) === Some (Some v)}
  = "caml_raw_memory_read_bytecode"
    "caml_raw_memory_read" [@@noalloc] [@@builtin] [@@no_effects]

external write : (p : t) @ immutable ->
  (i : {i : int | 0 <= i && i < length p}) -> (v : byte) ->
  (token : {s : contents P.token | H.mem (P.own s) (location p i)})
    @ unique read_write ghost ->
  {s : contents P.token | P.own s === H.put (P.own token) (location p i) (Some
    v)}
    @ unique ghost
  = "caml_raw_memory_write_bytecode"
    "caml_raw_memory_write" [@@noalloc] [@@builtin]

(** Consume the deallocation permission and every byte, initialized or not. *)
external free : (p : t) @ immutable ->
  (token : {s : contents P.token | H.mem (P.own s) (location p (-1))
    && covers (P.own s) p 0 (length p)}) @ unique read_write ghost ->
  {s : contents P.token | P.own s === H.exclude (P.own token) (footprint p)}
    @ unique ghost
  = "caml_raw_memory_free_bytecode" "caml_raw_memory_free" [@@noalloc]

val range_at : (p : t) @ immutable -> (lo : int) ->
    (hi : int) -> (i : int) ->
    {u : unit | H.mem (range p lo hi) (location p i) =
      (0 <= lo && lo <= i && i < hi) &&
      H.at (range p lo hi) (location p i) ===
        (if 0 <= lo && lo <= i && i < hi then Some None else None)}
      @ ghost
  @@ total

val footprint_at : (p : t) @ immutable -> (i : int) ->
    {u : unit | H.mem (footprint p) (location p i) =
      (i = -1 || (0 <= i && i < length p)) &&
      H.at (footprint p) (location p i) ===
        (if i = -1 || (0 <= i && i < length p)
         then Some None else None)} @ ghost
  @@ total

val covers_get : (h : contents P.heap) @ immutable ->
    (p : t) @ immutable -> (lo : int) -> (hi : int) -> (i : int) ->
    {u : unit | not (covers h p lo hi && 0 <= lo && lo <= i && i < hi)
      || H.mem h (location p i)} @ ghost
  @@ total

val covers_intro : (h : contents P.heap) @ immutable ->
    (p : t) @ immutable -> (lo : int) -> (hi : int) ->
    ((i : int) -> {u : unit | not (0 <= lo && lo <= i && i < hi)
      || H.mem h (location p i)}) @ total ->
    {u : unit | covers h p lo hi} @ ghost
  @@ total

val allocated_covers : (p : t) @ immutable ->
    (frame : contents P.heap) @ immutable ->
    {u : unit | covers (H.union (footprint p) frame) p 0 (length p)} @ ghost
  @@ total

val write_covers : (h : contents P.heap) @ immutable ->
    (p : t) @ immutable -> (lo : int) -> (hi : int) ->
    (i : int) -> (v : byte) ->
    {u : unit | not (covers h p lo hi) ||
      covers (H.put h (location p i) (Some v)) p lo hi} @ ghost
  @@ total

val split_covers : (h : contents P.heap) @ immutable ->
    (p : t) @ immutable -> (lo : int) -> (mid : int) -> (hi : int) ->
    {u : unit | not (0 <= lo && lo <= mid && mid <= hi && covers h p lo hi)
      || (covers (H.restrict h (range p lo mid)) p lo mid &&
          covers (H.exclude h (range p lo mid)) p mid hi)} @ ghost
  @@ total

val join_covers : (left : contents P.heap) @ immutable ->
    (right : contents P.heap) @ immutable -> (p : t) @ immutable ->
    (lo : int) -> (mid : int) -> (hi : int) ->
    {u : unit | not (0 <= lo && lo <= mid && mid <= hi &&
      covers left p lo mid && covers right p mid hi)
      || covers (H.union left right) p lo hi} @ ghost
  @@ total
