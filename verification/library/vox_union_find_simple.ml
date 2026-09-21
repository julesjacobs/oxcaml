module M = Vox_union_find_model
module F = Vox_union_find_forest
module S = Vox_union_find_spec
module A = Vox_union_find_amortized
module K = Vox_ackermann
module P = Ghost_pref
module H = P.Heap

module Make (C : Vox_big_credits.S) = struct
  module U = Vox_union_find.Make (C)
  type t = #{core : U.t; savings : C.token @@ ghost total}
  type result = #{value : M.elem @@ aliased; state : t}
  let[@def] contents (s : t @ local immutable total ghost forkable unyielding) = ghost_ (U.contents s.#core)
  let[@def] heap (s : t @ local immutable total ghost forkable unyielding) = ghost_ (U.heap s.#core)
  let[@def] size (s : t @ local immutable total ghost forkable unyielding) = ghost_ (F.size (contents s))
  let[@def] member (x : M.elem @ immutable) (s : t @ local immutable total ghost forkable unyielding) =
    ghost_ (F.member x (contents s))
  let[@def] representative (x : M.elem @ immutable) (s : t @ local immutable total ghost forkable unyielding) =
    ghost_ (F.representative x (contents s))
  let[@def] ticks (s : t @ local immutable total ghost forkable unyielding) = ghost_ (U.ticks s.#core)
  let[@def] account (s : t @ local immutable total ghost forkable unyielding) =
    ghost_ (Bigint.add (U.account s.#core) (C.credits s.#savings))
  let[@def] find_fee (s : t @ local immutable total ghost forkable unyielding) =
    ghost_ (A.find_fee (U.alpha s.#core))
  let[@def] union_fee (s : t @ local immutable total ghost forkable unyielding) =
    ghost_ (A.union_fee (U.alpha s.#core))
  let[@def] capacity (s : t @ local immutable total ghost forkable unyielding) = ghost_ (U.capacity s.#core)
  let[@def] valid (s : t @ local immutable total ghost forkable unyielding) = ghost_ (U.valid s.#core)

  let (account_bounds @ total) :
      (state : t) @ local immutable total ghost forkable unyielding ->
      {u : unit | ticks state <= account state} @ ghost = fun state -> ghost_ (
    ticks_def (borrow_ state); account_def (borrow_ state);
    U.account_bounds (borrow_ state.#core); C.nonnegative (borrow_ state.#savings);
    let u = () in u)

  let (observations @ total) :
      (state : t) @ local immutable total ghost forkable unyielding ->
      {u : unit | if valid state then 0Z <= size state &&
        size state <= Bigint.of_int max_int && find_fee state >= 12Z &&
        union_fee state >= 36Z else true} @ ghost = fun state -> ghost_ (
    valid_def (borrow_ state); size_def (borrow_ state); contents_def (borrow_ state);
    U.contents_def (borrow_ state.#core); U.size_bounds (borrow_ state.#core);
    U.alpha_bounds (borrow_ state.#core); U.alpha_def (borrow_ state.#core);
    find_fee_def (borrow_ state); union_fee_def (borrow_ state);
    A.find_fee_def (U.alpha state.#core); A.union_fee_def (U.alpha state.#core);
    let u = () in u)
  let create : (limit : {n : Bigint.t | 1Z <= n && n <= Bigint.of_int max_int}) @ ghost ->
      (fee : {b : C.token | C.credits b = 1Z}) @ unique total ghost ->
      {s : t | valid s && size s = 0Z && contents s === [] && account s = 1Z && capacity s = limit} @ unique =
      fun limit fee ->
    let cap = limit in
    let payment : {b : C.token | C.credits b >= 1Z} = fee in
    let r = U.create cap payment in
    let #{U.state = core; refund = savings} = r in
    let state = #{core; savings} in
    ghost_ (capacity_def (borrow_ state); valid_def (borrow_ state); contents_def (borrow_ state);
      size_def (borrow_ state); account_def (borrow_ state);
      U.contents_def (borrow_ state.#core); U.capacity_def (borrow_ state.#core);
      F.size_def []);
    state

  let make_set :
      (state : {s : t | valid s && size s < capacity s}) @ unique read_write total ->
      (fee : {b : C.token | C.credits b = 3Z}) @ unique total ghost ->
      {r : result | valid r.#state && capacity r.#state = capacity state &&
        contents r.#state === M.Stop r.#value :: contents state &&
        size r.#state = Bigint.add (size state) 1Z && member r.#value r.#state &&
        not (H.mem (heap state) r.#value) &&
        heap r.#state === H.put (heap state) r.#value (M.Root 0) &&
        account r.#state = Bigint.add (account state) 3Z} @ unique =
      fun state fee ->
    ghost_ (capacity_def (borrow_ state); valid_def (borrow_ state); contents_def (borrow_ state);
      heap_def (borrow_ state); size_def (borrow_ state); account_def (borrow_ state);
      U.contents_def (borrow_ state.#core); U.capacity_def (borrow_ state.#core);
      U.alpha_def (borrow_ state.#core));
    let savings = state.#savings in
    let core = state.#core in
    let input : {s : U.t | U.valid s && F.size s.#U.paths < s.#U.capacity} = core in
    let payment : {b : C.token | C.credits b >= 3Z} = fee in
    let r = U.make_set input payment in
    let #{U.value; state = core; refund} = r in
    ghost_ (C.nonnegative (borrow_ savings); C.nonnegative (borrow_ refund));
    let right : {t : C.token | 0Z <= C.credits savings && 0Z <= C.credits t} =
      refund in
    let savings = C.merge savings right in
    let state = #{core; savings} in
    ghost_ (capacity_def (borrow_ state); valid_def (borrow_ state); contents_def (borrow_ state);
      heap_def (borrow_ state); size_def (borrow_ state); account_def (borrow_ state);
      U.contents_def (borrow_ state.#core); U.capacity_def (borrow_ state.#core));
    ghost_ (size_def (borrow_ state); contents_def (borrow_ state);
      F.size_def (contents (borrow_ state));
      member_def value (borrow_ state); F.member_def value (contents (borrow_ state)); M.head_def (M.Stop value));
    let result = #{value; state} in result

  let find : (x : M.elem) @ immutable ->
      (state : {s : t | valid s && member x s}) @ unique read_write total ->
      (fee : {b : C.token | C.credits b = find_fee state})
        @ unique total ghost ->
      {r : result | valid r.#state && capacity r.#state = capacity state &&
        contents r.#state === F.refresh (F.lookup x (contents state)) (contents state) &&
        F.addresses (contents r.#state) === F.addresses (contents state) &&
        size r.#state = size state && r.#value === representative x state &&
        account r.#state = Bigint.add (account state) (find_fee state)} @ unique =
      fun x state fee ->
    ghost_ (capacity_def (borrow_ state); valid_def (borrow_ state); contents_def (borrow_ state);
      heap_def (borrow_ state); size_def (borrow_ state); account_def (borrow_ state);
      U.contents_def (borrow_ state.#core); U.capacity_def (borrow_ state.#core);
      U.alpha_def (borrow_ state.#core));
    ghost_ (member_def x (borrow_ state); U.member_def x (borrow_ state.#core);
      find_fee_def (borrow_ state); representative_def x (borrow_ state);
      U.representative_def x (borrow_ state.#core));
    let savings = state.#savings in
    let core = state.#core in
    let input : {s : U.t | U.valid s && U.member x s} = core in
    let payment : {b : C.token | C.credits b >= A.find_fee input.#U.alpha} = fee in
    let r = U.find x input payment in
    let #{U.value; state = core; refund} = r in
    ghost_ (C.nonnegative (borrow_ savings); C.nonnegative (borrow_ refund));
    let right : {t : C.token | 0Z <= C.credits savings && 0Z <= C.credits t} =
      refund in
    let savings = C.merge savings right in
    let state = #{core; savings} in
    ghost_ (capacity_def (borrow_ state); valid_def (borrow_ state); contents_def (borrow_ state);
      heap_def (borrow_ state); size_def (borrow_ state); account_def (borrow_ state);
      U.contents_def (borrow_ state.#core); U.capacity_def (borrow_ state.#core));
    let result = #{value; state} in result

  let union : (x : M.elem) @ immutable -> (y : M.elem) @ immutable ->
      (state : {s : t | valid s && member x s && member y s}) @ unique read_write total ->
      (fee : {b : C.token | C.credits b = union_fee state})
        @ unique total ghost ->
      {r : result | valid r.#state && capacity r.#state = capacity state &&
        contents r.#state === S.union_paths (heap state) (contents state) x y &&
        F.addresses (contents r.#state) === F.addresses (contents state) &&
        size r.#state = size state && r.#value === S.union_root (heap state) (contents state) x y &&
        account r.#state = Bigint.add (account state) (union_fee state)} @ unique =
      fun x y state fee ->
    ghost_ (capacity_def (borrow_ state); valid_def (borrow_ state); contents_def (borrow_ state);
      heap_def (borrow_ state); size_def (borrow_ state); account_def (borrow_ state);
      U.contents_def (borrow_ state.#core); U.capacity_def (borrow_ state.#core);
      U.alpha_def (borrow_ state.#core));
    ghost_ (member_def x (borrow_ state); U.member_def x (borrow_ state.#core);
      union_fee_def (borrow_ state); representative_def x (borrow_ state);
      U.representative_def x (borrow_ state.#core);
      member_def y (borrow_ state); U.member_def y (borrow_ state.#core));
    let savings = state.#savings in
    let core = state.#core in
    let input : {s : U.t | U.valid s && U.member x s && U.member y s} = core in
    let payment : {b : C.token | C.credits b >= A.union_fee input.#U.alpha} = fee in
    let r = U.union x y input payment in
    let #{U.value; state = core; refund} = r in
    ghost_ (C.nonnegative (borrow_ savings); C.nonnegative (borrow_ refund));
    let right : {t : C.token | 0Z <= C.credits savings && 0Z <= C.credits t} =
      refund in
    let savings = C.merge savings right in
    let state = #{core; savings} in
    ghost_ (capacity_def (borrow_ state); valid_def (borrow_ state); contents_def (borrow_ state);
      heap_def (borrow_ state); size_def (borrow_ state); account_def (borrow_ state);
      U.contents_def (borrow_ state.#core); U.capacity_def (borrow_ state.#core));
    let result = #{value; state} in result

  let (find_semantics @ total) : (x : M.elem) @ immutable ->
      (q : M.elem) @ immutable ->
      (state : t) @ local immutable total ghost forkable unyielding ->
      {u : unit | if valid state && member x state then
        F.representative q (S.find_paths (contents state) x) === representative q state
        else true} @ ghost = fun x q state -> ghost_ (
    valid_def (borrow_ state); contents_def (borrow_ state);
    member_def x (borrow_ state); representative_def q (borrow_ state);
    U.contents_def (borrow_ state.#core); U.member_def x (borrow_ state.#core);
    U.representative_def q (borrow_ state.#core);
    U.find_semantics x q (borrow_ state.#core);
    let u = () in u)

  let (union_semantics @ total) : (x : M.elem) @ immutable ->
      (y : M.elem) @ immutable -> (q : M.elem) @ immutable ->
      (state : t) @ local immutable total ghost forkable unyielding ->
      {u : unit | if valid state && member x state && member y state && member q state then
        F.representative q (S.union_paths (heap state) (contents state) x y) ===
          (if representative q state === representative x state ||
              representative q state === representative y state then
            S.union_root (heap state) (contents state) x y else representative q state)
        else true} @ ghost = fun x y q state -> ghost_ (
    valid_def (borrow_ state); contents_def (borrow_ state); heap_def (borrow_ state);
    member_def x (borrow_ state); member_def y (borrow_ state); member_def q (borrow_ state);
    representative_def x (borrow_ state); representative_def y (borrow_ state);
    representative_def q (borrow_ state);
    U.contents_def (borrow_ state.#core); U.member_def x (borrow_ state.#core);
    U.member_def y (borrow_ state.#core); U.member_def q (borrow_ state.#core);
    U.representative_def x (borrow_ state.#core); U.representative_def y (borrow_ state.#core);
    U.representative_def q (borrow_ state.#core);
    U.union_semantics x y q (borrow_ state.#core);
    let u = () in u)

  let (fee_bounds @ total) :
      (state : t) @ local immutable total ghost forkable unyielding -> (a : Bigint.t) ->
      {u : unit | if valid state && 1Z <= a &&
        K.iter (capacity state) a 1Z 1Z >= capacity state && K.below (capacity state) a
        then find_fee state = A.find_fee a && union_fee state = A.union_fee a
        else true} @ ghost = fun state a -> ghost_ (
    valid_def (borrow_ state); capacity_def (borrow_ state);
    U.alpha_bounds (borrow_ state.#core); U.size_bounds (borrow_ state.#core);
    U.capacity_def (borrow_ state.#core);
    let cap = capacity (borrow_ state) in let b = U.alpha (borrow_ state.#core) in
    if valid state && 1Z <= a && K.iter cap a 1Z 1Z >= cap && K.below cap a then (
      let u = () in K.inverse_order cap cap a b (u);
      let u = () in K.inverse_order cap cap b a (u);
      find_fee_def (borrow_ state); union_fee_def (borrow_ state);
      let u = () in u)
    else let u = () in u)
end
