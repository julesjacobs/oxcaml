module M = Vox_union_find_model
module F = Vox_union_find_forest
module S = Vox_union_find_spec
module A = Vox_union_find_amortized
module K = Vox_ackermann
module P = Ghost_pref
module H = P.Heap

module Make (C : Vox_big_credits.S) = struct
  module U = Vox_union_find.Make (C)
  type t = #{core : U.t; savings : C.token @@ ghost total; epoch : Bigint.t @@ ghost}
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
  let[@def] valid (s : t @ local immutable total ghost forkable unyielding) = ghost_ (
    U.valid s.#core && s.#epoch >= 1Z &&
    U.capacity s.#core = K.minimum s.#epoch (Bigint.of_int max_int) &&
    (if size s = 0Z then s.#epoch = 1Z else s.#epoch < Bigint.mul 2Z (size s)) &&
    C.credits s.#savings >= Bigint.sub (Bigint.mul 8Z (size s))
      (Bigint.mul 4Z (Bigint.sub s.#epoch 1Z)))

  let (account_bounds @ total) :
      (state : t) @ local immutable total ghost forkable unyielding ->
      {u : unit | ticks state <= account state} @ ghost = fun state -> ghost_ (
    ticks_def (borrow_ state); account_def (borrow_ state);
    U.account_bounds (borrow_ state.#core); C.nonnegative (borrow_ state.#savings);
    let u = () in refine_ u)

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
    let u = () in refine_ u)
  let create : (fee : {b : C.token | C.credits b = 1Z}) @ unique total ghost ->
      {s : t | valid s && size s = 0Z && contents s === [] && account s = 1Z} @ unique =
      fun fee ->
    (* The imported max_int has no refinement; check it once at creation. *)
    let checked : {u : unit | 1Z <= Bigint.of_int max_int} =
      if max_int >= 1 then (let u = () in refine_ u)
      else invalid_arg "Vox_union_find_online.create" in
    let refine_ checked = checked in
    let refine_ fee = fee in
    let capacity = ghost_ 1Z in
    let cap : {n : Bigint.t | 1Z <= n && n <= Bigint.of_int max_int} = refine_ capacity in
    let payment : {b : C.token | C.credits b >= 1Z} = refine_ fee in
    let refine_ r = U.create cap payment in
    let #{U.state = core; refund = savings} = r in
    let epoch = ghost_ 1Z in
    let state = #{core; savings; epoch} in
    ghost_ (valid_def (borrow_ state); contents_def (borrow_ state);
      size_def (borrow_ state); account_def (borrow_ state);
      U.contents_def (borrow_ state.#core); U.capacity_def (borrow_ state.#core);
      F.size_def []; K.minimum_def 1Z (Bigint.of_int max_int));
    refine_ state

  let make_set :
      (state : {s : t | valid s && size s < Bigint.of_int max_int}) @ unique read_write total ->
      (fee : {b : C.token | C.credits b = 11Z}) @ unique total ghost ->
      {r : result | let refine_ state = state in valid r.#state &&
        contents r.#state === M.Stop r.#value :: contents state &&
        size r.#state = Bigint.add (size state) 1Z && member r.#value r.#state &&
        not (H.mem (heap state) r.#value) &&
        heap r.#state === H.put (heap state) r.#value (M.Root 0) &&
        account r.#state = Bigint.add (account state) 11Z} @ unique =
      fun state fee ->
    let refine_ state = state in let refine_ fee = fee in
    ghost_ (valid_def (borrow_ state); contents_def (borrow_ state);
      heap_def (borrow_ state); size_def (borrow_ state); account_def (borrow_ state);
      U.contents_def (borrow_ state.#core); U.capacity_def (borrow_ state.#core);
      U.alpha_def (borrow_ state.#core));
    let n = ghost_ (size (borrow_ state)) in
    let old_epoch = ghost_ state.#epoch in
    let old_cap = ghost_ (U.capacity (borrow_ state.#core)) in
    let epoch = ghost_ (if n = old_cap then Bigint.mul 2Z old_epoch else old_epoch) in
    let cap = ghost_ (K.minimum epoch (Bigint.of_int max_int)) in
    let unlocked = ghost_ (if n = old_cap then Bigint.mul 4Z old_epoch else 0Z) in
    ghost_ (U.size_bounds (borrow_ state.#core);
      K.minimum_def old_epoch (Bigint.of_int max_int);
      K.minimum_def epoch (Bigint.of_int max_int);
      C.nonnegative (borrow_ state.#savings));
    let eight = ghost_ 8Z in
    let available : {b : C.token | 0Z <= eight && eight <= C.credits b} = refine_ fee in
    let refine_ deposit = C.split eight available in
    let savings = state.#savings in let core = state.#core in
    let right : {b : C.token | 0Z <= C.credits savings && 0Z <= C.credits b} =
      refine_ deposit.C.left in
    let refine_ savings = C.merge savings right in
    let available : {b : C.token | 0Z <= unlocked && unlocked <= C.credits b} = refine_ savings in
    let refine_ funds = C.split unlocked available in
    let input : {s : U.t | U.valid s && s.#U.capacity <= cap &&
      cap <= Bigint.mul 2Z s.#U.capacity && cap <= Bigint.of_int max_int} = refine_ core in
    let payment : {b : C.token | let refine_ input = input in
      C.credits b >= (if cap = input.#U.capacity then 0Z
        else Bigint.mul 4Z (F.size input.#U.paths))} = refine_ funds.C.left in
    let refine_ grown = U.reparameterize cap input payment in
    let core = grown.#U.state in let savings = funds.C.right in
    ghost_ (C.nonnegative (borrow_ savings); C.nonnegative (borrow_ grown.#U.refund));
    let right : {b : C.token | 0Z <= C.credits savings && 0Z <= C.credits b} =
      refine_ grown.#U.refund in
    let refine_ savings = C.merge savings right in
    let input : {s : U.t | U.valid s && F.size s.#U.paths < s.#U.capacity} = refine_ core in
    let payment : {b : C.token | C.credits b >= 3Z} = refine_ deposit.C.right in
    let refine_ r = U.make_set input payment in
    let #{U.value; state = core; refund} = r in
    ghost_ (C.nonnegative (borrow_ savings); C.nonnegative (borrow_ refund));
    let right : {t : C.token | 0Z <= C.credits savings && 0Z <= C.credits t} =
      refine_ refund in
    let refine_ savings = C.merge savings right in
    let state = #{core; savings; epoch} in
    ghost_ (valid_def (borrow_ state); contents_def (borrow_ state);
      heap_def (borrow_ state); size_def (borrow_ state); account_def (borrow_ state);
      U.contents_def (borrow_ state.#core); U.capacity_def (borrow_ state.#core));
    ghost_ (size_def (borrow_ state); contents_def (borrow_ state);
      F.size_def (contents (borrow_ state));
      member_def value (borrow_ state); F.member_def value (contents (borrow_ state)); M.head_def (M.Stop value));
    let result = #{value; state} in refine_ result

  let find : (x : M.elem) @ immutable ->
      (state : {s : t | valid s && member x s}) @ unique read_write total ->
      (fee : {b : C.token | let refine_ state = state in C.credits b = find_fee state})
        @ unique total ghost ->
      {r : result | let refine_ state = state in valid r.#state &&
        contents r.#state === F.refresh (F.lookup x (contents state)) (contents state) &&
        F.addresses (contents r.#state) === F.addresses (contents state) &&
        size r.#state = size state && r.#value === representative x state &&
        account r.#state = Bigint.add (account state) (find_fee state)} @ unique =
      fun x state fee ->
    let refine_ state = state in let refine_ fee = fee in
    ghost_ (valid_def (borrow_ state); contents_def (borrow_ state);
      heap_def (borrow_ state); size_def (borrow_ state); account_def (borrow_ state);
      U.contents_def (borrow_ state.#core); U.capacity_def (borrow_ state.#core);
      U.alpha_def (borrow_ state.#core));
    ghost_ (member_def x (borrow_ state); U.member_def x (borrow_ state.#core);
      find_fee_def (borrow_ state); representative_def x (borrow_ state);
      U.representative_def x (borrow_ state.#core));
    let epoch = ghost_ state.#epoch in
    let savings = state.#savings in
    let core = state.#core in
    let input : {s : U.t | U.valid s && U.member x s} = refine_ core in
    let payment : {b : C.token | let refine_ input = input in
      C.credits b >= A.find_fee input.#U.alpha} = refine_ fee in
    let refine_ r = U.find x input payment in
    let #{U.value; state = core; refund} = r in
    ghost_ (C.nonnegative (borrow_ savings); C.nonnegative (borrow_ refund));
    let right : {t : C.token | 0Z <= C.credits savings && 0Z <= C.credits t} =
      refine_ refund in
    let refine_ savings = C.merge savings right in
    let state = #{core; savings; epoch} in
    ghost_ (valid_def (borrow_ state); contents_def (borrow_ state);
      heap_def (borrow_ state); size_def (borrow_ state); account_def (borrow_ state);
      U.contents_def (borrow_ state.#core); U.capacity_def (borrow_ state.#core));
    let result = #{value; state} in refine_ result

  let union : (x : M.elem) @ immutable -> (y : M.elem) @ immutable ->
      (state : {s : t | valid s && member x s && member y s}) @ unique read_write total ->
      (fee : {b : C.token | let refine_ state = state in C.credits b = union_fee state})
        @ unique total ghost ->
      {r : result | let refine_ state = state in valid r.#state &&
        contents r.#state === S.union_paths (heap state) (contents state) x y &&
        F.addresses (contents r.#state) === F.addresses (contents state) &&
        size r.#state = size state && r.#value === S.union_root (heap state) (contents state) x y &&
        account r.#state = Bigint.add (account state) (union_fee state)} @ unique =
      fun x y state fee ->
    let refine_ state = state in let refine_ fee = fee in
    ghost_ (valid_def (borrow_ state); contents_def (borrow_ state);
      heap_def (borrow_ state); size_def (borrow_ state); account_def (borrow_ state);
      U.contents_def (borrow_ state.#core); U.capacity_def (borrow_ state.#core);
      U.alpha_def (borrow_ state.#core));
    ghost_ (member_def x (borrow_ state); U.member_def x (borrow_ state.#core);
      union_fee_def (borrow_ state); representative_def x (borrow_ state);
      U.representative_def x (borrow_ state.#core);
      member_def y (borrow_ state); U.member_def y (borrow_ state.#core));
    let epoch = ghost_ state.#epoch in
    let savings = state.#savings in
    let core = state.#core in
    let input : {s : U.t | U.valid s && U.member x s && U.member y s} = refine_ core in
    let payment : {b : C.token | let refine_ input = input in
      C.credits b >= A.union_fee input.#U.alpha} = refine_ fee in
    let refine_ r = U.union x y input payment in
    let #{U.value; state = core; refund} = r in
    ghost_ (C.nonnegative (borrow_ savings); C.nonnegative (borrow_ refund));
    let right : {t : C.token | 0Z <= C.credits savings && 0Z <= C.credits t} =
      refine_ refund in
    let refine_ savings = C.merge savings right in
    let state = #{core; savings; epoch} in
    ghost_ (valid_def (borrow_ state); contents_def (borrow_ state);
      heap_def (borrow_ state); size_def (borrow_ state); account_def (borrow_ state);
      U.contents_def (borrow_ state.#core); U.capacity_def (borrow_ state.#core));
    let result = #{value; state} in refine_ result

  let (fee_bounds @ total) :
      (state : t) @ local immutable total ghost forkable unyielding ->
      (population : Bigint.t) -> (a : Bigint.t) ->
      {u : unit | if valid state && 1Z <= population && size state <= population &&
        1Z <= a && K.iter population a 1Z 1Z >= population && K.below population a
        then find_fee state <= Bigint.add (Bigint.mul 4Z a) 12Z &&
          union_fee state <= Bigint.add (Bigint.mul 12Z a) 36Z else true} @ ghost =
      fun state population a -> ghost_ (
    valid_def (borrow_ state); size_def (borrow_ state); contents_def (borrow_ state);
    U.alpha_bounds (borrow_ state.#core); U.size_bounds (borrow_ state.#core);
    U.alpha_def (borrow_ state.#core); U.capacity_def (borrow_ state.#core);
    U.contents_def (borrow_ state.#core);
    let cap = U.capacity (borrow_ state.#core) in
    let b = U.alpha (borrow_ state.#core) in
    K.minimum_def state.#epoch (Bigint.of_int max_int);
    if valid state && 1Z <= population && size state <= population &&
      1Z <= a && K.iter population a 1Z 1Z >= population && K.below population a then (
      if cap <= population then (
        let u = () in K.inverse_order cap population b a (refine_ u))
      else (
        let u = () in K.inverse_doubling population cap a b (refine_ u));
      find_fee_def (borrow_ state); union_fee_def (borrow_ state);
      A.find_fee_def b; A.union_fee_def b;
      let u = () in refine_ u)
    else let u = () in refine_ u)

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
    let u = () in refine_ u)

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
    let u = () in refine_ u)
end
