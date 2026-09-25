@@ portable

(** A shared lock over one unique payload. Atomic-event assumptions are in
    [Verified_atomic]. No fairness, termination, cancellation recovery or
    exception-safe restoration is claimed. All contracts describe normal returns. *)
module Make (V : Unique_cell.Payload) : sig @@ portable
  type t : immutable_data
  type contents : immutable_data = V.model option
  type 'a step = { value : 'a; state : contents Ghost_pref.token @@ ghost }
  val location : t @ local immutable -> contents Ghost_pref.t @ immutable ghost
    @@ total
  val owned : t @ immutable -> contents Ghost_pref.heap @ immutable -> bool @ ghost
    @@ total
  val owned_def : (a : t) @ immutable ->
    (h : contents Ghost_pref.heap) @ immutable ->
    {u : unit | owned a h === (ghost_ (
      let p = location a in match Ghost_pref.Heap.at h p with
      | Some (Some x) -> h === Ghost_pref.Heap.put (Ghost_pref.Heap.empty ()) p (Some x)
      | _ -> false))} @@ total

  val make : V.t @ unique total -> t
  (** Success transfers the full singleton cell authority to the caller;
      failure transfers only empty authority. *)
  val try_acquire : (a : t) ->
    {r : (bool, contents) Ghost_pref.step |
      if r.value then owned a (Ghost_pref.own r.state)
      else Ghost_pref.own r.state === Ghost_pref.Heap.empty ()} @ unique
  (** Release consumes full cell authority and restores it to the lock. *)
  val release : (a : t) ->
    {t : contents Ghost_pref.token | owned a (Ghost_pref.own t)} @ unique ghost ->
    {t : contents Ghost_pref.token | Ghost_pref.own t === Ghost_pref.Heap.empty ()}
      @ unique ghost
  val take : (a : t) ->
    (token : {t : contents Ghost_pref.token |
      match Ghost_pref.Heap.at (Ghost_pref.own t) (location a) with
      | Some (Some _) -> true | _ -> false}) @ unique ghost ->
    {r : V.t step | let refine_ token = token in
      Ghost_pref.Heap.at (Ghost_pref.own token) (location a)
        === Some (Some (V.snapshot r.value)) &&
      Ghost_pref.own r.state === Ghost_pref.Heap.put (Ghost_pref.own token)
        (location a) None} @ unique
  val put : (a : t) -> (value : V.t) @ unique ->
    (token : {t : contents Ghost_pref.token |
      Ghost_pref.Heap.at (Ghost_pref.own t) (location a) === Some None}) @ unique ghost ->
    {t : contents Ghost_pref.token | let refine_ token = token in
      Ghost_pref.own t === Ghost_pref.Heap.put (Ghost_pref.own token)
        (location a) (Some (V.snapshot value))} @ unique ghost
end
