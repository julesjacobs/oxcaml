@@ portable

(** A spin lock on one atomic flag guarding one cell, shared by
    [Reference_lock] and [Unique_lock]. Flag 0 means the lock owns the cell,
    which is full; flag 1 means a holder owns it. [try_acquire] makes one
    compare-and-set from 0 to 1 and [release] one from 1 to 0. Atomic-event
    assumptions are in [Verified_atomic]. No fairness, termination,
    cancellation recovery or exception-safe restoration is claimed. All
    contracts describe normal returns. *)

(** The guarded cell: its ghost location and when its payload is full. *)
module type Cell = sig
  type payload : logical_data
  type cell : logical_data
  val location : cell @ immutable -> payload Ghost_pref.t @ immutable ghost
    @@ total
  val full : payload @ immutable -> bool @ ghost @@ total
end

module Make (C : Cell) : sig @@ portable
  type t : logical_data
  val cell : t @ local immutable -> C.cell @ immutable @@ total
  (** [owned a h]: [h] is exactly the cell of [a], holding a full payload. *)
  val owned : t @ immutable -> C.payload Ghost_pref.heap @ immutable ->
    bool @ ghost @@ total
  val owned_def : (a : t) @ immutable ->
    (h : C.payload Ghost_pref.heap) @ immutable ->
    {u : unit | owned a h === (ghost_ (
      let p = C.location (cell a) in
      match Ghost_pref.Heap.at h p with
      | None -> false
      | Some x -> C.full x &&
        h === Ghost_pref.Heap.put (Ghost_pref.Heap.empty ()) p x))} @@ total

  (** Hands the cell and its full authority to a new, released lock. *)
  val create : (c : C.cell) ->
    (token : {t : C.payload Ghost_pref.token |
      let p = C.location c in
      match Ghost_pref.Heap.at (Ghost_pref.own t) p with
      | None -> false
      | Some x -> C.full x &&
        Ghost_pref.own t === Ghost_pref.Heap.put (Ghost_pref.Heap.empty ()) p x})
      @ unique ghost ->
    {a : t | cell a === c}
  (** Success transfers the full cell authority to the caller; failure
      transfers only empty authority. *)
  val try_acquire : (a : t) ->
    {r : (bool, C.payload) Ghost_pref.step |
      if r.value then owned a (Ghost_pref.own r.state)
      else Ghost_pref.own r.state === Ghost_pref.Heap.empty ()} @ unique
  (** Release consumes full cell authority and restores it to the lock. *)
  val release : (a : t) ->
    {t : C.payload Ghost_pref.token | owned a (Ghost_pref.own t)} @ unique ghost ->
    {t : C.payload Ghost_pref.token |
      Ghost_pref.own t === Ghost_pref.Heap.empty ()} @ unique ghost
end
