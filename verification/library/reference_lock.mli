@@ portable

(** A nonnegative integer reference guarded by a strong sequentially
    consistent lock. Failure transfers no reference authority. Normal-return
    safety only: no fairness, exception recovery or termination guarantee. *)
type t : immutable_data
val location : t @ immutable -> int Ghost_pref.t @ immutable @@ total
val owned : t @ immutable -> int Ghost_pref.heap @ immutable -> bool @ ghost @@ total
val owned_def : (a : t) @ immutable -> (h : int Ghost_pref.heap) @ immutable ->
  {u : unit | owned a h === (ghost_ (
    match Ghost_pref.Heap.at h (location a) with
    | None -> false
    | Some x -> 0 <= x && h === Ghost_pref.Heap.put
        (Ghost_pref.Heap.empty ()) (location a) x))} @@ total
val make : {n : int | 0 <= n} -> t
val try_acquire : (a : t) ->
  {r : (bool, int) Ghost_pref.step |
    if r.value then owned a (Ghost_pref.own r.state)
    else Ghost_pref.own r.state === Ghost_pref.Heap.empty ()} @ unique
val release : (a : t) ->
  {t : int Ghost_pref.token | owned a (Ghost_pref.own t)} @ unique ghost ->
  {t : int Ghost_pref.token | Ghost_pref.own t === Ghost_pref.Heap.empty ()} @ unique ghost
val read_owned : (a : t) ->
  (t : {t : int Ghost_pref.token | owned a (Ghost_pref.own t)}) @ local read ghost ->
  {n : int | 0 <= n && Ghost_pref.Heap.at (Ghost_pref.own t) (location a) === Some n}
(** Implementation behavior: attempts one increment, keeping the old value
    on machine-integer overflow; the bool reports acquisition. These behaviors
    are exercised by clients but are not exported refinement postconditions. *)
val try_increment : t -> bool
