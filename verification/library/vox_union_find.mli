module M = Vox_union_find_model
module F = Vox_union_find_forest
module R = Vox_union_find_rank
module B = Vox_union_find_bank
module A = Vox_union_find_amortized
module D = Vox_union_find_mass
module J = Vox_union_find_link
module S = Vox_union_find_spec
module K = Vox_ackermann
module P = Ghost_pref
module H = P.Heap

module Make (C : Vox_big_credits.S) : sig
  module W : sig type resource : void end
  type t = private #{
    raw : W.resource;
    paths : M.path list @@ ghost immutable total aliased;
    capacity : Bigint.t @@ ghost;
    alpha : Bigint.t @@ ghost;
    spent : Bigint.t @@ ghost;
  }
  type result = #{value : M.elem @@ aliased; state : t;
    refund : C.token @@ ghost total}

  val heap : t @ local immutable total ghost -> Vox_union_find_model.node P.heap @ immutable ghost @@ total
  val valid : t @ local immutable total ghost -> bool @ ghost @@ total
  val contents : t @ local immutable total ghost -> M.path list @ immutable ghost @@ total
  val capacity : t @ local immutable total ghost -> Bigint.t @ ghost @@ total
  val alpha : t @ local immutable total ghost -> Bigint.t @ ghost @@ total
  val ticks : t @ local immutable total ghost -> Bigint.t @ ghost @@ total
  val account : t @ local immutable total ghost -> Bigint.t @ ghost @@ total
  val member : M.elem @ immutable -> t @ local immutable total ghost -> bool @ ghost @@ total
  val representative : M.elem @ immutable -> t @ local immutable total ghost -> M.elem @ immutable ghost @@ total
  val contents_def : (s : t) @ local immutable total forkable unyielding ->
    {u : unit | contents s === (ghost_ s.#paths)} @@ total
  val capacity_def : (s : t) @ local immutable total forkable unyielding ->
    {u : unit | capacity s === (ghost_ s.#capacity)} @@ total
  val alpha_def : (s : t) @ local immutable total forkable unyielding ->
    {u : unit | alpha s === (ghost_ s.#alpha)} @@ total
  val ticks_def : (s : t) @ local immutable total forkable unyielding ->
    {u : unit | ticks s === (ghost_ s.#spent)} @@ total
  val member_def : (x : M.elem) @ immutable ->
    (s : t) @ local immutable total forkable unyielding ->
    {u : unit | member x s === (ghost_ (F.member x s.#paths))} @@ total
  val representative_def : (x : M.elem) @ immutable ->
    (s : t) @ local immutable total forkable unyielding ->
    {u : unit | representative x s === (ghost_ (F.representative x s.#paths))} @@ total
  val alpha_bounds : (state : t) @ local immutable total ghost forkable unyielding ->
    {u : unit | if valid state then
      1Z <= alpha state && alpha state <= capacity state &&
      K.iter (capacity state) (alpha state) 1Z 1Z >= capacity state &&
      K.below (capacity state) (alpha state) else true} @ ghost @@ total
  val account_bounds : (state : t) @ local immutable total ghost forkable unyielding ->
    {u : unit | ticks state <= account state} @ ghost @@ total
  val size_bounds :
      (state : t) @ local immutable total ghost forkable unyielding ->
      {u : unit | if valid state then 0Z <= F.size state.#paths &&
        F.size state.#paths <= state.#capacity &&
        1Z <= state.#capacity && state.#capacity <= Bigint.of_int max_int
        else true} @ ghost @@ total

  val model_valid :
      (state : t) @ local immutable total ghost forkable unyielding ->
      {u : unit | if valid state then
        F.valid (heap state) (contents state) &&
        F.complete (contents state) (contents state) &&
        R.all_ordered (capacity state) (heap state) (contents state) &&
        capacity state <= Bigint.of_int max_int else true} @ ghost @@ total

  val find_semantics : (x : M.elem) @ immutable ->
      (q : M.elem) @ immutable ->
      (state : t) @ local immutable total ghost forkable unyielding ->
      {u : unit | if valid state && member x state then
        F.representative q (S.find_paths state.#paths x) === representative q state
        else true} @ ghost @@ total

  val union_semantics : (x : M.elem) @ immutable ->
      (y : M.elem) @ immutable -> (q : M.elem) @ immutable ->
      (state : t) @ local immutable total ghost forkable unyielding ->
      {u : unit | if valid state && member x state && member y state && member q state then
        F.representative q (S.union_paths (heap state) state.#paths x y) ===
          (if representative q state === representative x state ||
              representative q state === representative y state then
            S.union_root (heap state) state.#paths x y else representative q state)
        else true} @ ghost @@ total

  type initialized = #{state : t; refund : C.token @@ ghost total}

  val create : (capacity : {n : Bigint.t | 1Z <= n && n <= Bigint.of_int max_int})
      @ ghost -> (fee : {b : C.token | C.credits b >= 1Z}) @ unique total ghost ->
      {r : initialized | let refine_ capacity = capacity in
        valid r.#state && r.#state.#spent = 1Z && account r.#state = 1Z &&
        1Z <= r.#state.#alpha && r.#state.#alpha <= r.#state.#capacity &&
        r.#state.#capacity = capacity && r.#state.#paths === [] &&
        K.below capacity r.#state.#alpha &&
        (let refine_ fee = fee in C.credits r.#refund = Bigint.sub (C.credits fee) 1Z &&
          Bigint.add (account r.#state) (C.credits r.#refund) = C.credits fee)}
      @ unique

  val reparameterize :
      (capacity : Bigint.t) @ ghost ->
      (state : {s : t | valid s && s.#capacity <= capacity &&
        capacity <= Bigint.mul 2Z s.#capacity && capacity <= Bigint.of_int max_int})
        @ unique read_write total ->
      (fee : {b : C.token | let refine_ state = state in
        C.credits b >= (if capacity = state.#capacity then 0Z
          else Bigint.mul 4Z (F.size state.#paths))}) @ unique total ghost ->
      {r : initialized | let refine_ state = state in
        valid r.#state && r.#state.#capacity = capacity &&
        state.#alpha <= r.#state.#alpha && r.#state.#alpha <= Bigint.add state.#alpha 1Z &&
        r.#state.#paths === state.#paths && heap r.#state === heap state &&
        r.#state.#spent = state.#spent &&
        (let refine_ fee = fee in
          C.credits r.#refund = Bigint.sub (C.credits fee)
            (Bigint.mul 4Z (Bigint.mul (Bigint.sub r.#state.#alpha state.#alpha)
              (D.mass (heap state) state.#paths))) &&
          C.credits r.#refund >= Bigint.sub (C.credits fee)
            (if capacity = state.#capacity then 0Z else Bigint.mul 4Z (F.size state.#paths)) &&
          Bigint.add (account r.#state) (C.credits r.#refund) =
            Bigint.add (account state) (C.credits fee))} @ unique

  val make_set :
      (state : {s : t | valid s && F.size s.#paths < s.#capacity})
        @ unique read_write total ->
      (fee : {b : C.token | C.credits b >= 3Z}) @ unique total ghost ->
      {r : result | let refine_ state = state in
        valid r.#state && r.#state.#capacity = state.#capacity &&
        r.#state.#alpha = state.#alpha &&
        r.#state.#paths === M.Stop r.#value :: state.#paths &&
        not (H.mem (heap state) r.#value) &&
        heap r.#state === H.put (heap state) r.#value (M.Root 0) &&
        (let refine_ fee = fee in
          Bigint.add (account r.#state) (C.credits r.#refund) =
            Bigint.add (account state) (C.credits fee) &&
          C.credits r.#refund = Bigint.sub (C.credits fee) 3Z &&
          account r.#state = Bigint.add (account state) 3Z)}
      @ unique

  val find : (x : M.elem) @ immutable ->
      (state : {s : t | valid s && member x s}) @ unique read_write total ->
      (fee : {b : C.token | let refine_ state = state in
        C.credits b >= A.find_fee state.#alpha}) @ unique total ghost ->
      {r : result | let refine_ state = state in
        valid r.#state && F.addresses r.#state.#paths === F.addresses state.#paths &&
        F.size r.#state.#paths = F.size state.#paths &&
        r.#state.#paths === F.refresh
          (F.lookup x state.#paths) state.#paths &&
        r.#state.#capacity = state.#capacity && r.#state.#alpha = state.#alpha &&
        r.#value === representative x state &&
        heap r.#state === M.compressed (heap state) (F.lookup x state.#paths) &&
        (let refine_ fee = fee in
          Bigint.add (account r.#state) (C.credits r.#refund) =
            Bigint.add (account state) (C.credits fee) && C.credits r.#refund >=
          Bigint.sub (C.credits fee) (A.find_fee state.#alpha) &&
          account r.#state <= Bigint.add (account state) (A.find_fee state.#alpha))}
      @ unique

  val union : (x : M.elem) @ immutable -> (y : M.elem) @ immutable ->
      (state : {s : t | valid s && member x s && member y s}) @ unique read_write total ->
      (fee : {b : C.token | let refine_ state = state in
        C.credits b >= A.union_fee state.#alpha}) @ unique total ghost ->
      {r : result | let refine_ state = state in
        valid r.#state && r.#state.#capacity = state.#capacity &&
        r.#state.#alpha = state.#alpha &&
        r.#state.#paths === S.union_paths (heap state) state.#paths x y &&
        F.addresses r.#state.#paths === F.addresses state.#paths &&
        F.size r.#state.#paths = F.size state.#paths &&
        heap r.#state === S.union_heap (heap state) state.#paths x y &&
        r.#value === S.union_root (heap state) state.#paths x y &&
        (let refine_ fee = fee in
          Bigint.add (account r.#state) (C.credits r.#refund) =
            Bigint.add (account state) (C.credits fee) && C.credits r.#refund >=
          Bigint.sub (C.credits fee) (A.union_fee state.#alpha) &&
          account r.#state <= Bigint.add (account state) (A.union_fee state.#alpha))}
      @ unique
end
