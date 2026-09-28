module type Invariant = sig
  type payload : immutable_data [@@total_matchable]
  type key : void [@@total_matchable]
  val holds : key @ immutable -> int @ immutable ->
    payload Ghost_pref.heap @ immutable -> bool @ ghost
    @@ total
end
module Make (I : Invariant) = struct
  type t : immutable_data [@@total_matchable]
  external key : t @ local immutable -> I.key @ immutable ghost
    @@ total = "caml_vox_atomic_key_bytecode" "caml_vox_atomic_key"
  type ('a : immediate) result = #{
    value : 'a; state : I.payload Ghost_pref.token @@ ghost }
  type transfer = { restored : I.payload Ghost_pref.token @@ ghost;
                    outgoing : I.payload Ghost_pref.token @@ ghost }
  external create :
    (k : I.key) @ immutable ghost -> (initial : int) ->
    (g : {g : I.payload Ghost_pref.token | I.holds k initial (Ghost_pref.own
      g)})
      @ unique ghost -> {a : t | key a === k}
    @@ portable = "caml_vox_atomic_create_bytecode" "caml_vox_atomic_create"
  external load :
    (a : t) @ local contended ->
    (post : (int @ immutable -> I.payload Ghost_pref.heap @ immutable -> bool @
      ghost))
      @ immutable total ghost ->
    (caller : I.payload Ghost_pref.token) @ unique ghost ->
    ((before : int) @ immutable ghost ->
     (inside : {g : I.payload Ghost_pref.token |
       I.holds (key a) before (Ghost_pref.own g) &&
       Ghost_pref.Heap.disjoint (Ghost_pref.own g) (Ghost_pref.own caller)})
       @ unique ghost ->
     (outside : {c : I.payload Ghost_pref.token |
       Ghost_pref.own c === Ghost_pref.own caller}) @ unique ghost ->
     {r : transfer | I.holds (key a) before (Ghost_pref.own r.restored) &&
       post before (Ghost_pref.own r.outgoing)} @ unique)
      @ immutable total ghost ->
    {r : int result | post r.#value (Ghost_pref.own r.#state)} @ unique
    @@ portable = "caml_vox_atomic_load_bytecode" "caml_vox_atomic_load"
    [@@noalloc]
  external compare_and_set :
    (a : t) @ local contended -> (expected : int) -> (desired : int) ->
    (post : (bool @ immutable -> I.payload Ghost_pref.heap @ immutable -> bool @
      ghost))
      @ immutable total ghost ->
    (caller : I.payload Ghost_pref.token) @ unique ghost ->
    ((before : int) @ immutable ghost ->
     (inside : {g : I.payload Ghost_pref.token |
       I.holds (key a) before (Ghost_pref.own g) &&
       Ghost_pref.Heap.disjoint (Ghost_pref.own g) (Ghost_pref.own caller)})
       @ unique ghost ->
     (outside : {c : I.payload Ghost_pref.token |
       Ghost_pref.own c === Ghost_pref.own caller}) @ unique ghost ->
     {r : transfer | I.holds (key a) (if before = expected then desired else before)
         (Ghost_pref.own r.restored) &&
       post (before = expected) (Ghost_pref.own r.outgoing)} @ unique)
      @ immutable total ghost ->
    {r : bool result | post r.#value (Ghost_pref.own r.#state)} @ unique
    @@ portable = "caml_vox_atomic_cas_bytecode" "caml_vox_atomic_cas"
    [@@noalloc]
  external exchange :
    (a : t) @ local contended -> (desired : int) ->
    (post : (int @ immutable -> I.payload Ghost_pref.heap @ immutable -> bool @
      ghost))
      @ immutable total ghost ->
    (caller : I.payload Ghost_pref.token) @ unique ghost ->
    ((before : int) @ immutable ghost ->
     (inside : {g : I.payload Ghost_pref.token |
       I.holds (key a) before (Ghost_pref.own g) &&
       Ghost_pref.Heap.disjoint (Ghost_pref.own g) (Ghost_pref.own caller)})
       @ unique ghost ->
     (outside : {c : I.payload Ghost_pref.token |
       Ghost_pref.own c === Ghost_pref.own caller}) @ unique ghost ->
     {r : transfer | I.holds (key a) desired (Ghost_pref.own r.restored) &&
       post before (Ghost_pref.own r.outgoing)} @ unique)
      @ immutable total ghost ->
    {r : int result | post r.#value (Ghost_pref.own r.#state)} @ unique
    @@ portable = "caml_vox_atomic_exchange_bytecode" "caml_vox_atomic_exchange"
    [@@noalloc]
  external set :
    (a : t) @ local contended -> (desired : int) ->
    (post : (I.payload Ghost_pref.heap @ immutable -> bool @ ghost))
      @ immutable total ghost ->
    (caller : I.payload Ghost_pref.token) @ unique ghost ->
    ((before : int) @ immutable ghost ->
     (inside : {g : I.payload Ghost_pref.token |
       I.holds (key a) before (Ghost_pref.own g) &&
       Ghost_pref.Heap.disjoint (Ghost_pref.own g) (Ghost_pref.own caller)})
       @ unique ghost ->
     (outside : {c : I.payload Ghost_pref.token |
       Ghost_pref.own c === Ghost_pref.own caller}) @ unique ghost ->
     {r : transfer | I.holds (key a) desired (Ghost_pref.own r.restored) &&
       post (Ghost_pref.own r.outgoing)} @ unique)
      @ immutable total ghost ->
    {r : unit result | post (Ghost_pref.own r.#state)} @ unique
    @@ portable = "caml_vox_atomic_set_bytecode" "caml_vox_atomic_set"
    [@@noalloc]
  external fetch_and_add :
    (a : t) @ local contended -> (n : int) ->
    (post : (int @ immutable -> I.payload Ghost_pref.heap @ immutable -> bool @
      ghost))
      @ immutable total ghost ->
    (caller : I.payload Ghost_pref.token) @ unique ghost ->
    ((before : int) @ immutable ghost ->
     (inside : {g : I.payload Ghost_pref.token |
       I.holds (key a) before (Ghost_pref.own g) &&
       Ghost_pref.Heap.disjoint (Ghost_pref.own g) (Ghost_pref.own caller)})
       @ unique ghost ->
     (outside : {c : I.payload Ghost_pref.token |
       Ghost_pref.own c === Ghost_pref.own caller}) @ unique ghost ->
     {r : transfer | I.holds (key a) (before + n) (Ghost_pref.own r.restored) &&
       post before (Ghost_pref.own r.outgoing)} @ unique)
      @ immutable total ghost ->
    {r : int result | post r.#value (Ghost_pref.own r.#state)} @ unique
    @@ portable = "caml_vox_atomic_fetch_add_bytecode"
      "caml_vox_atomic_fetch_add"
    [@@noalloc]
end
