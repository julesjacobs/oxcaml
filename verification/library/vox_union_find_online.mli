module M = Vox_union_find_model
module F = Vox_union_find_forest
module S = Vox_union_find_spec
module A = Vox_union_find_amortized
module K = Vox_ackermann
module P = Ghost_pref
module H = P.Heap

module Make (C : Vox_big_credits.S) : sig
  type t : (void & void & void & void & void) & void & void
  type result = #{value : M.elem @@ aliased; state : t}

  val contents : t @ local immutable total ghost forkable unyielding -> M.path list @ immutable ghost @@ total
  val heap : t @ local immutable total ghost forkable unyielding -> P.heap @ immutable ghost @@ total
  val size : t @ local immutable total ghost forkable unyielding -> Bigint.t @ ghost @@ total
  val ticks : t @ local immutable total ghost forkable unyielding -> Bigint.t @ ghost @@ total
  val account : t @ local immutable total ghost forkable unyielding -> Bigint.t @ ghost @@ total
  val valid : t @ local immutable total ghost forkable unyielding -> bool @ ghost @@ total
  val find_fee : t @ local immutable total ghost forkable unyielding -> Bigint.t @ ghost @@ total
  val union_fee : t @ local immutable total ghost forkable unyielding -> Bigint.t @ ghost @@ total
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

  val fee_bounds :
      (state : t) @ local immutable total ghost forkable unyielding ->
      (population : Bigint.t) -> (a : Bigint.t) ->
      {u : unit | if valid state && 1Z <= population && size state <= population &&
        1Z <= a && K.iter population a 1Z 1Z >= population && K.below population a
        then find_fee state <= Bigint.add (Bigint.mul 4Z a) 12Z &&
          union_fee state <= Bigint.add (Bigint.mul 12Z a) 36Z else true} @ ghost @@ total

  val create : (fee : {b : C.token | C.credits b = 1Z}) @ unique total ghost ->
      {s : t | valid s && size s = 0Z && contents s === [] && account s = 1Z} @ unique

  val make_set :
      (state : {s : t | valid s && size s < Bigint.of_int max_int}) @ unique read_write total ->
      (fee : {b : C.token | C.credits b = 11Z}) @ unique total ghost ->
      {r : result | let refine_ state = state in valid r.#state &&
        contents r.#state === M.Stop r.#value :: contents state &&
        size r.#state = Bigint.add (size state) 1Z && member r.#value r.#state &&
        not (H.mem (heap state) r.#value) &&
        heap r.#state === H.put (heap state) r.#value (M.Root 0) &&
        account r.#state = Bigint.add (account state) 11Z} @ unique

  val find : (x : M.elem) @ immutable ->
      (state : {s : t | valid s && member x s}) @ unique read_write total ->
      (fee : {b : C.token | let refine_ state = state in C.credits b = find_fee state})
        @ unique total ghost ->
      {r : result | let refine_ state = state in valid r.#state &&
        contents r.#state === F.refresh (F.lookup x (contents state)) (contents state) &&
        F.addresses (contents r.#state) === F.addresses (contents state) &&
        size r.#state = size state && r.#value === representative x state &&
        account r.#state = Bigint.add (account state) (find_fee state)} @ unique

  val union : (x : M.elem) @ immutable -> (y : M.elem) @ immutable ->
      (state : {s : t | valid s && member x s && member y s}) @ unique read_write total ->
      (fee : {b : C.token | let refine_ state = state in C.credits b = union_fee state})
        @ unique total ghost ->
      {r : result | let refine_ state = state in valid r.#state &&
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

end
