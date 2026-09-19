module P = Ghost_pref
module H = P.Heap

type t : immutable_data
type byte : immediate = {v : int | 0 <= v && v <= 255}
type contents : immutable_data = byte option
type allocation : immutable_data = t option

external length : t @ local immutable -> {n : int | n >= 0}
  @@ total portable = "caml_raw_memory_length"

external equal : (p : t) @ immutable -> (q : t) @ immutable ->
  {b : bool | b = (p === q)} @ total
  @@ total portable = "%eq"

(* These locations exist only in the ownership model. Index -1 is the
   deallocation permission; nonnegative indices describe bytes. *)
external location : t @ immutable -> int -> contents P.t @ immutable ghost
  @@ total = "caml_raw_memory_location"
external location_law : (p : t) @ immutable -> (q : t) @ immutable ->
  (i : int) -> (j : int) ->
  {u : unit | H.mem (H.put (H.empty ()) (location p i) None)
    (location q j) = (p === q && i = j)} @ ghost
  @@ total = "caml_pref_heap_law4"

let[@def] rec (range @ total) (p : t @ immutable)
    (lo : int) (hi : int) = ghost_ (
  if 0 <= lo && lo < hi then
    H.put (range p lo (hi - 1)) (location p (hi - 1)) None
  else H.empty ())
[@@decreases let hi : int = hi in if hi > 0 then hi else 0]

let[@def] (footprint @ total) (p : t @ immutable) = ghost_ (
  H.put (range p 0 (length p)) (location p (-1)) None)

let[@def] rec (covers @ total) (h : P.heap @ immutable) (p : t @ immutable)
    (lo : int) (hi : int) = ghost_ (
  if 0 <= lo && lo < hi then
    H.mem h (location p (hi - 1)) && covers h p lo (hi - 1)
  else true)
[@@decreases let hi : int = hi in if hi > 0 then hi else 0]

external malloc : (n : {n : int | n >= 0}) ->
  (token : P.token) @ unique ghost ->
  {r : allocation P.step | match r.value with
    | None -> P.own r.state === P.own token
    | Some p -> length p = n && H.disjoint (footprint p) (P.own token)
      && P.own r.state === H.union (footprint p) (P.own token)} @ unique
  @@ portable = "caml_raw_memory_malloc_bytecode" "caml_raw_memory_malloc"

external read : (p : t) @ immutable ->
  (i : {i : int | 0 <= i && i < length p}) ->
  (token : {s : P.token | H.mem (P.own s) (location p i) &&
    match H.at (P.own s) (location p i) with
    | Some (Some _) -> true | _ -> false}) @ local read ghost ->
  {v : byte | H.at (P.own token) (location p i) === Some (Some v)}
  @@ portable = "caml_raw_memory_read_bytecode" "caml_raw_memory_read"

external write : (p : t) @ immutable ->
  (i : {i : int | 0 <= i && i < length p}) -> (v : byte) ->
  (token : {s : P.token | H.mem (P.own s) (location p i)})
    @ unique read_write ghost ->
  {s : P.token | P.own s === H.put (P.own token) (location p i) (Some v)}
    @ unique ghost
  @@ portable = "caml_raw_memory_write_bytecode" "caml_raw_memory_write"

external free : (p : t) @ immutable ->
  (token : {s : P.token | H.mem (P.own s) (location p (-1))
    && covers (P.own s) p 0 (length p)}) @ unique read_write ghost ->
  {s : P.token | P.own s === H.exclude (P.own token) (footprint p)}
    @ unique ghost
  @@ portable = "caml_raw_memory_free_bytecode" "caml_raw_memory_free"

let rec (range_at @ total) : (p : t) @ immutable -> (lo : int) ->
    (hi : int) -> (i : int) ->
    {u : unit | H.mem (range p lo hi) (location p i) =
      (0 <= lo && lo <= i && i < hi) &&
      H.at (range p lo hi) (location p i) ===
        (if 0 <= lo && lo <= i && i < hi then Some None else None)}
      @ ghost = fun p lo hi i -> ghost_ (
  range_def p lo hi;
  if 0 <= lo && lo < hi then begin
    let refine_ distinct = location_law p p (hi - 1) i in
    let refine_ induction = range_at p lo (hi - 1) i in
    let updated = H.put (range p lo (hi - 1)) (location p (hi - 1)) None in
    let _present = H.mem updated (location p i) in
    let _value = H.at updated (location p i) in
    ()
  end else (
    let _present = H.mem (H.empty ()) (location p i) in
    let _value = H.at (H.empty ()) (location p i) in ()))
[@@decreases let hi : int = hi in if hi > 0 then hi else 0]

let (footprint_at @ total) : (p : t) @ immutable -> (i : int) ->
    {u : unit | H.mem (footprint p) (location p i) =
      (i = -1 || (0 <= i && i < length p)) &&
      H.at (footprint p) (location p i) ===
        (if i = -1 || (0 <= i && i < length p)
         then Some None else None)} @ ghost = fun p i -> ghost_ (
  footprint_def p;
  range_at p 0 (length p) i;
  location_law p p (-1) i;
  let h = H.put (range p 0 (length p)) (location p (-1)) None in
  let _present = H.mem h (location p i) in
  let _value = H.at h (location p i) in
  ())

let rec (covers_get @ total) : (h : P.heap) @ immutable ->
    (p : t) @ immutable -> (lo : int) -> (hi : int) -> (i : int) ->
    {u : unit | not (covers h p lo hi && 0 <= lo && lo <= i && i < hi)
      || H.mem h (location p i)} @ ghost = fun h p lo hi i -> ghost_ (
  covers_def h p lo hi;
  if 0 <= lo && lo < hi then begin
    covers_get h p lo (hi - 1) i;
    ()
  end else ())
[@@decreases let hi : int = hi in if hi > 0 then hi else 0]

let rec (covers_intro @ total) : (h : P.heap) @ immutable ->
    (p : t) @ immutable -> (lo : int) -> (hi : int) ->
    ((i : int) -> {u : unit | not (0 <= lo && lo <= i && i < hi)
      || H.mem h (location p i)}) @ total ->
    {u : unit | covers h p lo hi} @ ghost = fun h p lo hi proof -> ghost_ (
  covers_def h p lo hi;
  if 0 <= lo && lo < hi then begin
    proof (hi - 1);
    covers_intro h p lo (hi - 1) (fun i -> proof i);
    ()
  end else ())
[@@decreases let hi : int = hi in if hi > 0 then hi else 0]

let (allocated_covers @ total) : (p : t) @ immutable ->
    (frame : P.heap) @ immutable ->
    {u : unit | covers (H.union (footprint p) frame) p 0 (length p)} @ ghost =
    fun p frame -> ghost_ (
  covers_intro (H.union (footprint p) frame) p 0 (length p) (fun i ->
    footprint_at p i;
    ());
  ())

let (write_covers @ total) : (h : P.heap) @ immutable ->
    (p : t) @ immutable -> (lo : int) -> (hi : int) ->
    (i : int) -> (v : byte) ->
    {u : unit | not (covers h p lo hi) ||
      covers (H.put h (location p i) (Some v)) p lo hi} @ ghost =
    fun h p lo hi i v -> ghost_ (
  if covers h p lo hi then
    covers_intro (H.put h (location p i) (Some v)) p lo hi (fun j ->
      covers_get h p lo hi j;
      ())
  else ())

let (split_covers @ total) : (h : P.heap) @ immutable ->
    (p : t) @ immutable -> (lo : int) -> (mid : int) -> (hi : int) ->
    {u : unit | not (0 <= lo && lo <= mid && mid <= hi && covers h p lo hi)
      || (covers (H.restrict h (range p lo mid)) p lo mid &&
          covers (H.exclude h (range p lo mid)) p mid hi)} @ ghost =
    fun h p lo mid hi -> ghost_ (
  if 0 <= lo && lo <= mid && mid <= hi && covers h p lo hi then begin
    covers_intro (H.restrict h (range p lo mid)) p lo mid (fun i ->
      range_at p lo mid i;
      covers_get h p lo hi i;
      ());
    covers_intro (H.exclude h (range p lo mid)) p mid hi (fun i ->
      range_at p lo mid i;
      covers_get h p lo hi i;
      ())
  end else ())

let (join_covers @ total) : (left : P.heap) @ immutable ->
    (right : P.heap) @ immutable -> (p : t) @ immutable ->
    (lo : int) -> (mid : int) -> (hi : int) ->
    {u : unit | not (0 <= lo && lo <= mid && mid <= hi &&
      covers left p lo mid && covers right p mid hi)
      || covers (H.union left right) p lo hi} @ ghost =
    fun left right p lo mid hi -> ghost_ (
  if 0 <= lo && lo <= mid && mid <= hi &&
      covers left p lo mid && covers right p mid hi then
    covers_intro (H.union left right) p lo hi (fun i ->
      covers_get left p lo mid i;
      covers_get right p mid hi i;
      ())
  else ())
