(** Strong sequentially consistent integer atomics with unique ghost ownership.
    These external contracts are trusted. Each operation opens the invariant
    at its atomic event and requires a total erased transition restoring it.
    The transition receives disjoint live invariant and caller resources;
    it must return distinct resources for the invariant and the caller. *)

module type Invariant = sig
  type key : immutable_data
  val holds : key @ immutable -> int @ immutable ->
    Ghost_pref.heap @ immutable -> bool @ ghost
    @@ total
end
module Make (I : Invariant) : sig
  type t : immutable_data
  external key : t @ local immutable -> I.key @ immutable
    @@ total = "caml_vox_atomic_key"
  type transfer = { restored : Ghost_pref.token @@ ghost;
                    outgoing : Ghost_pref.token @@ ghost }
  external create :
    (k : I.key) @ immutable -> (initial : int) ->
    (g : {g : Ghost_pref.token | I.holds k initial (Ghost_pref.own g)})
      @ unique ghost -> {a : t | key a === k}
    @@ portable = "caml_vox_atomic_create_bytecode" "caml_vox_atomic_create"
  external load :
    (a : t) @ local contended ->
    (post : (int @ immutable -> Ghost_pref.heap @ immutable -> bool @ ghost))
      @ immutable total ghost ->
    (caller : Ghost_pref.token) @ unique ghost ->
    ((before : int) @ immutable ghost ->
     (inside : {g : Ghost_pref.token |
       I.holds (key a) before (Ghost_pref.own g) &&
       Ghost_pref.Heap.disjoint (Ghost_pref.own g) (Ghost_pref.own caller)})
       @ unique ghost ->
     (outside : {c : Ghost_pref.token |
       Ghost_pref.own c === Ghost_pref.own caller}) @ unique ghost ->
     {r : transfer | I.holds (key a) before (Ghost_pref.own r.restored) &&
       post before (Ghost_pref.own r.outgoing)} @ unique)
      @ immutable total ghost ->
    {r : int Ghost_pref.step | post r.value (Ghost_pref.own r.state)} @ unique
    @@ portable = "caml_vox_atomic_load_bytecode" "caml_vox_atomic_load"
  external compare_and_set :
    (a : t) @ local contended -> (expected : int) -> (desired : int) ->
    (post : (bool @ immutable -> Ghost_pref.heap @ immutable -> bool @ ghost))
      @ immutable total ghost ->
    (caller : Ghost_pref.token) @ unique ghost ->
    ((before : int) @ immutable ghost ->
     (inside : {g : Ghost_pref.token |
       I.holds (key a) before (Ghost_pref.own g) &&
       Ghost_pref.Heap.disjoint (Ghost_pref.own g) (Ghost_pref.own caller)})
       @ unique ghost ->
     (outside : {c : Ghost_pref.token |
       Ghost_pref.own c === Ghost_pref.own caller}) @ unique ghost ->
     {r : transfer | I.holds (key a) (if before = expected then desired else before)
         (Ghost_pref.own r.restored) &&
       post (before = expected) (Ghost_pref.own r.outgoing)} @ unique)
      @ immutable total ghost ->
    {r : bool Ghost_pref.step | post r.value (Ghost_pref.own r.state)} @ unique
    @@ portable = "caml_vox_atomic_cas_bytecode" "caml_vox_atomic_cas"
end
