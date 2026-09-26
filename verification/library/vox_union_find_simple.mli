module M = Vox_union_find_model
module F = Vox_union_find_forest
module S = Vox_union_find_spec
module A = Vox_union_find_amortized
module K = Vox_ackermann
module P = Ghost_pref
module H = P.Heap

module Make (C : Vox_big_credits.S) : sig
  type t : (void & void & void & void & void & void) & void
  type result = #{value : M.elem @@ aliased; state : t}

  val contents : t @ local immutable total ghost forkable unyielding -> M.path list @ immutable ghost @@ total
  val heap : t @ local immutable total ghost forkable unyielding -> Vox_union_find_model.node P.heap @ immutable ghost @@ total
  val size : t @ local immutable total ghost forkable unyielding -> Bigint.t @ ghost @@ total
  val ticks : t @ local immutable total ghost forkable unyielding -> Bigint.t @ ghost @@ total
  val account : t @ local immutable total ghost forkable unyielding -> Bigint.t @ ghost @@ total
  val valid : t @ local immutable total ghost forkable unyielding -> bool @ ghost @@ total
  val find_fee : t @ local immutable total ghost forkable unyielding -> Bigint.t @ ghost @@ total
  val union_fee : t @ local immutable total ghost forkable unyielding -> Bigint.t @ ghost @@ total
  val capacity : t @ local immutable total ghost forkable unyielding -> Bigint.t @ ghost @@ total
  val member : M.elem @ immutable -> t @ local immutable total ghost forkable unyielding -> bool @ ghost @@ total
  val representative : M.elem @ immutable -> t @ local immutable total ghost forkable unyielding -> M.elem @ immutable ghost @@ total
  val size_def : (s : t) @ local immutable total forkable unyielding ->
    {u : unit | size s === (ghost_ (F.size (contents s)))} @@ total
  val member_def : (x : M.elem) @ immutable ->
    (s : t) @ local immutable total forkable unyielding ->
    {u : unit | member x s === (ghost_ (F.member x (contents s)))} @@ total
  val representative_def : (x : M.elem) @ immutable ->
    (s : t) @ local immutable total forkable unyielding ->
    {u : unit | representative x s === (ghost_ (F.representative x (contents s)))} @@ total

  val account_bounds :
      (state : t) @ local immutable total ghost forkable unyielding ->
      {u : unit | ticks state <= account state} @ ghost @@ total

  val observations :
      (state : t) @ local immutable total ghost forkable unyielding ->
      {u : unit | if valid state then 0Z <= size state &&
        size state <= Bigint.of_int max_int && find_fee state >= 12Z &&
        union_fee state >= 36Z else true} @ ghost @@ total

  val create : (limit : {n : Bigint.t | 1Z <= n && n <= Bigint.of_int max_int}) @ ghost ->
      (fee : {b : C.token | C.credits b = 1Z}) @ unique total ghost ->
      {s : t | valid s && size s = 0Z && contents s === [] && account s = 1Z && capacity s = limit} @ unique

  val make_set :
      (state : {s : t | valid s && size s < capacity s}) @ unique read_write total ->
      (fee : {b : C.token | C.credits b = 3Z}) @ unique total ghost ->
      {r : result | let state = state in valid r.#state && capacity r.#state = capacity state &&
        contents r.#state === M.Stop r.#value :: contents state &&
        size r.#state = Bigint.add (size state) 1Z && member r.#value r.#state &&
        not (H.mem (heap state) r.#value) &&
        heap r.#state === H.put (heap state) r.#value (M.Root 0) &&
        account r.#state = Bigint.add (account state) 3Z} @ unique

  val find : (x : M.elem) @ immutable ->
      (state : {s : t | valid s && member x s}) @ unique read_write total ->
      (fee : {b : C.token | let state = state in C.credits b = find_fee state})
        @ unique total ghost ->
      {r : result | let state = state in valid r.#state && capacity r.#state = capacity state &&
        contents r.#state === F.refresh (F.lookup x (contents state)) (contents state) &&
        F.addresses (contents r.#state) === F.addresses (contents state) &&
        size r.#state = size state && r.#value === representative x state &&
        account r.#state = Bigint.add (account state) (find_fee state)} @ unique

  val union : (x : M.elem) @ immutable -> (y : M.elem) @ immutable ->
      (state : {s : t | valid s && member x s && member y s}) @ unique read_write total ->
      (fee : {b : C.token | let state = state in C.credits b = union_fee state})
        @ unique total ghost ->
      {r : result | let state = state in valid r.#state && capacity r.#state = capacity state &&
        contents r.#state === S.union_paths (heap state) (contents state) x y &&
        F.addresses (contents r.#state) === F.addresses (contents state) &&
        size r.#state = size state && r.#value === S.union_root (heap state) (contents state) x y &&
        account r.#state = Bigint.add (account state) (union_fee state)} @ unique

  val find_semantics : (x : M.elem) @ immutable ->
      (q : M.elem) @ immutable ->
      (state : t) @ local immutable total ghost forkable unyielding ->
      {u : unit | if valid state && member x state then
        F.representative q (S.find_paths (contents state) x) === representative q state
        else true} @ ghost @@ total

  val union_semantics : (x : M.elem) @ immutable ->
      (y : M.elem) @ immutable -> (q : M.elem) @ immutable ->
      (state : t) @ local immutable total ghost forkable unyielding ->
      {u : unit | if valid state && member x state && member y state && member q state then
        F.representative q (S.union_paths (heap state) (contents state) x y) ===
          (if representative q state === representative x state ||
              representative q state === representative y state then
            S.union_root (heap state) (contents state) x y else representative q state)
        else true} @ ghost @@ total


  val fee_bounds :
      (state : t) @ local immutable total ghost forkable unyielding -> (a : Bigint.t) ->
      {u : unit | if valid state && 1Z <= a &&
        K.iter (capacity state) a 1Z 1Z >= capacity state && K.below (capacity state) a
        then find_fee state = A.find_fee a && union_fee state = A.union_fee a
        else true} @ ghost @@ total
end
