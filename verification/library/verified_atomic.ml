module type Invariant = sig
  type key : void
  val holds : key @ immutable -> int @ immutable ->
    Ghost_pref.heap @ immutable -> bool @ ghost
    @@ total
end
module Make (I : Invariant) = struct
  type t : immutable_data
  external key : t @ local immutable -> I.key @ immutable ghost
    @@ total = "caml_vox_atomic_key_bytecode" "caml_vox_atomic_key"
  type ('a : immediate) result = #{
    value : 'a; state : Ghost_pref.token @@ ghost }
  type transfer = { restored : Ghost_pref.token @@ ghost;
                    outgoing : Ghost_pref.token @@ ghost }
  external create :
    (k : I.key) @ immutable ghost -> (initial : int) ->
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
     {r : transfer | let refine_ inside = inside in
       let refine_ outside = outside in
       I.holds (key a) before (Ghost_pref.own r.restored) &&
       post before (Ghost_pref.own r.outgoing)} @ unique)
      @ immutable total ghost ->
    {r : int result | post r.#value (Ghost_pref.own r.#state)} @ unique
    @@ portable = "caml_vox_atomic_load_bytecode" "caml_vox_atomic_load"
    [@@noalloc]
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
     {r : transfer | let refine_ inside = inside in
       let refine_ outside = outside in
       I.holds (key a) (if before = expected then desired else before)
         (Ghost_pref.own r.restored) &&
       post (before = expected) (Ghost_pref.own r.outgoing)} @ unique)
      @ immutable total ghost ->
    {r : bool result | post r.#value (Ghost_pref.own r.#state)} @ unique
    @@ portable = "caml_vox_atomic_cas_bytecode" "caml_vox_atomic_cas"
    [@@noalloc]
end
