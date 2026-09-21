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

module Make (C : Vox_big_credits.S) = struct
  module W = Vox_union_find_worker.Make (C)
  type t = #{
    raw : W.resource;
    paths : M.path list @@ ghost immutable total aliased;
    capacity : Bigint.t @@ ghost;
    alpha : Bigint.t @@ ghost;
    spent : Bigint.t @@ ghost;
  }
  type result = #{value : M.elem @@ aliased; state : t;
    refund : C.token @@ ghost total}

  let[@def] heap (s : t @ local immutable total ghost) =
    ghost_ (W.heap s.#raw)
  let[@def] valid (s : t @ local immutable total ghost) = ghost_ (
    s.#spent >= 0Z && s.#capacity >= 1Z && s.#capacity <= Bigint.of_int max_int &&
    F.size s.#paths <= s.#capacity &&
    D.mass (heap s) s.#paths <= Bigint.sub (F.size s.#paths)
      (D.components (heap s) s.#paths) && s.#alpha >= 1Z && s.#alpha <= s.#capacity &&
    K.iter s.#capacity s.#alpha 1Z 1Z >= s.#capacity &&
    K.below s.#capacity s.#alpha &&
    F.valid (heap s) s.#paths && F.complete s.#paths s.#paths &&
    R.all_ordered s.#capacity (heap s) s.#paths &&
    W.balance s.#raw = Bigint.mul 4Z (B.potential s.#capacity s.#alpha (heap s) s.#paths))
  let[@def] member (x : M.elem @ immutable) (s : t @ local immutable total ghost) =
    ghost_ (F.member x s.#paths)
  let[@def] representative (x : M.elem @ immutable)
      (s : t @ local immutable total ghost) = ghost_ (F.representative x s.#paths)

  let[@def] contents (s : t @ local immutable total ghost) = ghost_ s.#paths

  let[@def] capacity (s : t @ local immutable total ghost) = ghost_ s.#capacity
  let[@def] alpha (s : t @ local immutable total ghost) = ghost_ s.#alpha
  let[@def] ticks (s : t @ local immutable total ghost) = ghost_ s.#spent

  let[@def] account (s : t @ local immutable total ghost) =
    ghost_ (Bigint.add s.#spent (W.balance s.#raw))

  let (alpha_bounds @ total) :
      (state : t) @ local immutable total ghost forkable unyielding ->
      {u : unit | if valid state then
        1Z <= alpha state && alpha state <= capacity state &&
        K.iter (capacity state) (alpha state) 1Z 1Z >= capacity state &&
        K.below (capacity state) (alpha state) else true} @ ghost =
      fun state -> ghost_ (
    valid_def (borrow_ state); alpha_def (borrow_ state); capacity_def (borrow_ state);
    let u = () in refine_ u)

  let (account_bounds @ total) :
      (state : t) @ local immutable total ghost forkable unyielding ->
      {u : unit | ticks state <= account state} @ ghost =
      fun state -> ghost_ (
    ticks_def (borrow_ state); account_def (borrow_ state);
    W.balance_def (borrow_ state.#raw); C.nonnegative (borrow_ state.#raw.W.bank);
    let u = () in refine_ u)

  let (size_bounds @ total) :
      (state : t) @ local immutable total ghost forkable unyielding ->
      {u : unit | if valid state then 0Z <= F.size state.#paths &&
        F.size state.#paths <= state.#capacity &&
        1Z <= state.#capacity && state.#capacity <= Bigint.of_int max_int
        else true} @ ghost = fun state -> ghost_ (
    valid_def (borrow_ state); D.population_bounds state.#capacity (heap state) state.#paths;
    let u = () in refine_ u)

  let (model_valid @ total) :
      (state : t) @ local immutable total ghost forkable unyielding ->
      {u : unit | if valid state then
        F.valid (heap state) (contents state) &&
        F.complete (contents state) (contents state) &&
        R.all_ordered (capacity state) (heap state) (contents state) &&
        capacity state <= Bigint.of_int max_int else true} @ ghost =
      fun state -> ghost_ (
    valid_def (borrow_ state); contents_def (borrow_ state);
    capacity_def (borrow_ state);
    let u = () in refine_ u)

  let (find_semantics @ total) : (x : M.elem) @ immutable ->
      (q : M.elem) @ immutable ->
      (state : t) @ local immutable total ghost forkable unyielding ->
      {u : unit | if valid state && member x state then
        F.representative q (S.find_paths state.#paths x) === representative q state
        else true} @ ghost = fun x q state -> ghost_ (
    valid_def (borrow_ state); member_def x (borrow_ state);
    representative_def q (borrow_ state);
    let h = heap (borrow_ state) in let paths = contents (borrow_ state) in
    contents_def (borrow_ state);
    F.lookup_valid h x paths; F.refresh_representative h (F.lookup x paths) paths q;
    S.find_paths_def paths x;
    let u = () in refine_ u)

  let (union_semantics @ total) : (x : M.elem) @ immutable ->
      (y : M.elem) @ immutable -> (q : M.elem) @ immutable ->
      (state : t) @ local immutable total ghost forkable unyielding ->
      {u : unit | if valid state && member x state && member y state && member q state then
        F.representative q (S.union_paths (heap state) state.#paths x y) ===
          (if representative q state === representative x state ||
              representative q state === representative y state then
            S.union_root (heap state) state.#paths x y else representative q state)
        else true} @ ghost = fun x y q state -> ghost_ (
    valid_def (borrow_ state); member_def x (borrow_ state);
    member_def y (borrow_ state); member_def q (borrow_ state);
    representative_def x (borrow_ state); representative_def y (borrow_ state);
    representative_def q (borrow_ state);
    let h = heap (borrow_ state) in let paths = contents (borrow_ state) in
    let cap = capacity (borrow_ state) in
    contents_def (borrow_ state); capacity_def (borrow_ state);
    let px = F.lookup x paths in
    F.lookup_valid h x paths; R.lookup_ordered cap h paths x; R.bounds cap h px;
    M.terminal h px; M.is_root_def h (M.root px);
    R.weight_def h (M.root px); M.rank_def h (M.root px);
    F.representative_def x paths;
    S.find_paths_def paths x; S.find_heap_def h paths x;
    let first = S.find_paths paths x in let middle = S.find_heap h paths x in
    let py = F.lookup y first in
    M.compressed_rank h px (M.root px);
    M.compressed_rank middle py (M.root px);
    S.find_heap_def middle first y;
    S.union_representative h paths x y q;
    let u = () in refine_ u)

  type initialized = #{state : t; refund : C.token @@ ghost total}

  let create : (capacity : {n : Bigint.t | 1Z <= n && n <= Bigint.of_int max_int})
      @ ghost -> (fee : {b : C.token | C.credits b >= 1Z}) @ unique total ghost ->
      {r : initialized | let refine_ capacity = capacity in
        valid r.#state && r.#state.#spent = 1Z && account r.#state = 1Z &&
        1Z <= r.#state.#alpha && r.#state.#alpha <= r.#state.#capacity &&
        r.#state.#capacity = capacity && r.#state.#paths === [] &&
        K.below capacity r.#state.#alpha &&
        (let refine_ fee = fee in C.credits r.#refund = Bigint.sub (C.credits fee) 1Z &&
          Bigint.add (account r.#state) (C.credits r.#refund) = C.credits fee)}
      @ unique = fun capacity fee ->
    let refine_ capacity = capacity in
    let refine_ fee = fee in
    let input : {n : Bigint.t | 1Z <= n} = refine_ capacity in
    let refine_ alpha = ghost_ (K.inverse input) in
    let positive : {b : C.token | C.credits b > 0Z} = refine_ fee in
    let refine_ refund = C.tick positive in
    let refine_ memory = P.empty () in
    let refine_ bank = C.empty () in
    let raw = {W.memory; bank} in
    ghost_ (W.heap_def (borrow_ raw); W.balance_def (borrow_ raw));
    let state = #{raw; paths = []; capacity; alpha; spent = ghost_ 1Z} in
    ghost_ (heap_def (borrow_ state); valid_def (borrow_ state); contents_def (borrow_ state);
      capacity_def (borrow_ state);
        alpha_def (borrow_ state);
        ticks_def (borrow_ state);
        account_def (borrow_ state);
      let h = heap (borrow_ state) in
      F.valid_def h []; F.complete_def [] []; F.size_def [];
      R.all_ordered_def capacity h []; B.potential_def capacity alpha h [];
      D.mass_def h []; D.components_def h []);
    let result = #{state; refund} in refine_ result

  let reparameterize :
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
            Bigint.add (account state) (C.credits fee))} @ unique = fun capacity state fee ->
    let refine_ state = state in let refine_ fee = fee in
    let old_cap = ghost_ state.#capacity in let old_a = ghost_ state.#alpha in
    let paths = ghost_ state.#paths in let spent = ghost_ state.#spent in
    let before = ghost_ (heap (borrow_ state)) in
    ghost_ (valid_def (borrow_ state));
    let input : {n : Bigint.t | n >= 1Z} = refine_ capacity in
    let refine_ alpha = ghost_ (K.inverse input) in
    ghost_ (
      let u = () in K.inverse_order old_cap capacity old_a alpha (refine_ u);
      let u = () in K.inverse_doubling old_cap capacity old_a alpha (refine_ u);
      if capacity = old_cap then (
        let u = () in K.inverse_order capacity old_cap alpha old_a (refine_ u));
      D.population_bounds old_cap before paths);
    let needed = ghost_ (Bigint.mul 4Z (Bigint.mul (Bigint.sub alpha old_a)
      (D.mass before paths))) in
    ghost_ (
      B.reparameterize old_cap capacity old_a alpha before paths;
      R.weaken_all old_cap capacity before paths;
      account_def (borrow_ state); heap_def (borrow_ state);
      W.heap_def (borrow_ state.#raw);
      W.balance_def (borrow_ state.#raw); C.nonnegative (borrow_ state.#raw.W.bank));
    let available : {t : C.token | 0Z <= needed && needed <= C.credits t} = refine_ fee in
    let refine_ divided = C.split needed available in
    let raw = state.#raw in let memory = raw.W.memory in let bank = raw.W.bank in
    let right : {t : C.token | 0Z <= C.credits bank && 0Z <= C.credits t} =
      refine_ divided.C.left in
    let refine_ bank = C.merge bank right in
    let raw = {W.memory; bank} in
    ghost_ (W.heap_def (borrow_ raw); W.balance_def (borrow_ raw));
    let state = #{raw; paths; capacity; alpha; spent} in
    ghost_ (heap_def (borrow_ state); valid_def (borrow_ state);
      account_def (borrow_ state));
    ghost_ (let u = () in
      let _ : {u : unit | valid state} = refine_ u in ());
    let result = #{state; refund = divided.C.right} in refine_ result

  let make_set :
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
      @ unique = fun state fee ->
    let refine_ state = state in let refine_ fee = fee in
    let spent = ghost_ (ticks (borrow_ state)) in
    let cap = ghost_ (capacity (borrow_ state)) in let alpha = ghost_ (alpha (borrow_ state)) in
    let paths = ghost_ (contents (borrow_ state)) in
    let before = ghost_ (heap (borrow_ state)) in
    let target = ghost_ (W.balance (borrow_ state.#raw)) in
    ghost_ (valid_def (borrow_ state); contents_def (borrow_ state);
      capacity_def (borrow_ state);
        alpha_def (borrow_ state);
        ticks_def (borrow_ state);
        account_def (borrow_ state);
        heap_def (borrow_ state);
      W.heap_def (borrow_ state.#raw); W.balance_def (borrow_ state.#raw);
      C.nonnegative (borrow_ state.#raw.W.bank); C.nonnegative (borrow_ fee));
    let raw = state.#raw in
    let memory = raw.W.memory in let bank = raw.W.bank in
    let right : {t : C.token | 0Z <= C.credits bank && 0Z <= C.credits t} =
      refine_ fee in
    let refine_ bank = C.merge bank right in
    let raw = {W.memory; bank} in
    ghost_ (W.heap_def (borrow_ raw); W.balance_def (borrow_ raw));
    let input : {s : W.resource | W.balance s >= 3Z} = refine_ raw in
    let refine_ allocated = W.allocate input in
    let #{W.value; state = raw} = allocated in
    ghost_ (W.heap_def (borrow_ raw); W.balance_def (borrow_ raw);
      F.allocate_valid before paths value; F.allocate_complete paths value;
      R.fresh_all_ordered cap before value paths;
      B.allocate_potential cap alpha before paths value;
      D.allocate_sums before paths value;
      let after = H.put before value (M.Root 0) in
      R.all_ordered_def cap after (M.Stop value :: paths);
      R.ordered_def cap after (M.Stop value); M.head_def (M.Stop value);
      R.weight_def after value; M.rank_def after value);
    let memory = raw.W.memory in
    let available : {t : C.token | 0Z <= target && target <= C.credits t} =
      refine_ raw.W.bank in
    let refine_ divided = C.split target available in
    let raw = {W.memory; bank = divided.C.left} in
    ghost_ (W.heap_def (borrow_ raw); W.balance_def (borrow_ raw));
    let state = #{raw; paths = M.Stop value :: paths; capacity = cap; alpha;
      spent = ghost_ (Bigint.add spent 3Z)} in
    ghost_ (heap_def (borrow_ state); valid_def (borrow_ state); contents_def (borrow_ state);
      capacity_def (borrow_ state);
        alpha_def (borrow_ state);
        ticks_def (borrow_ state);
        account_def (borrow_ state));
    let result = #{value; state; refund = divided.C.right} in refine_ result

  let find : (x : M.elem) @ immutable ->
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
      @ unique = fun x state fee ->
    let refine_ state = state in
    let refine_ fee = fee in
    let spent = ghost_ (ticks (borrow_ state)) in
    let cap = ghost_ (capacity (borrow_ state)) in
    let alpha = ghost_ (alpha (borrow_ state)) in
    let paths = ghost_ (contents (borrow_ state)) in
    let before = ghost_ (heap (borrow_ state)) in
    let path = ghost_ (F.lookup x paths) in
    let depth = ghost_ (M.depth path) in
    ghost_ (
      valid_def (borrow_ state); contents_def (borrow_ state);
      capacity_def (borrow_ state);
        alpha_def (borrow_ state);
        ticks_def (borrow_ state);
        account_def (borrow_ state);
        member_def x (borrow_ state);
      heap_def (borrow_ state); representative_def x (borrow_ state);
      F.representative_def x paths;
      F.lookup_valid before x paths; F.lookup_closed paths paths x;
      R.lookup_ordered cap before paths x;
      A.find_bound cap alpha before path; M.depth_nonnegative path;
      Vox_union_find_worker.cost_def depth;
      B.compression_potential cap alpha before path paths;
      F.refresh_valid before path paths;
        F.refresh_addresses before path paths;
        D.compression before path paths;
      F.refresh_complete before path paths paths;
      R.refresh_all_ordered cap before path paths;
      B.nonnegative cap alpha (M.compressed before path) (F.refresh path paths);
      W.heap_def (borrow_ state.#raw); W.balance_def (borrow_ state.#raw);
      C.nonnegative (borrow_ state.#raw.bank); C.nonnegative (borrow_ fee));
    let raw = state.#raw in
    let memory = raw.W.memory in
    let bank = raw.W.bank in
    let right : {t : C.token | 0Z <= C.credits bank && 0Z <= C.credits t} =
      refine_ fee in
    let refine_ bank = C.merge bank right in
    let raw = {W.memory; bank} in
    ghost_ (W.heap_def (borrow_ raw); W.balance_def (borrow_ raw));
    let input : {s : W.resource | M.valid (W.heap s) path &&
      x === M.head path && depth = M.depth path &&
      W.balance s >= Vox_union_find_worker.cost depth} = refine_ raw in
    let refine_ found = W.find depth path x input in
    let #{W.value; state = raw} = found in
    ghost_ (W.heap_def (borrow_ raw); W.balance_def (borrow_ raw));
    let after_paths = ghost_ (F.refresh path paths) in
    let target = ghost_ (Bigint.mul 4Z
      (B.potential cap alpha (M.compressed before path) after_paths)) in
    let memory = raw.W.memory in
    let available : {t : C.token | 0Z <= target && target <= C.credits t} =
      refine_ raw.W.bank in
    let refine_ divided = C.split target available in
    let raw = {W.memory; bank = divided.C.left} in
    ghost_ (W.heap_def (borrow_ raw); W.balance_def (borrow_ raw));
    let state = #{raw; paths = after_paths; capacity = cap; alpha;
      spent = ghost_ (Bigint.add spent (Vox_union_find_worker.cost depth))} in
    ghost_ (heap_def (borrow_ state); valid_def (borrow_ state); contents_def (borrow_ state);
      capacity_def (borrow_ state);
        alpha_def (borrow_ state);
        ticks_def (borrow_ state);
        account_def (borrow_ state));
    let result = #{value; state; refund = divided.C.right} in
    refine_ result

  let link : (x : M.elem) @ immutable -> (y : M.elem) @ immutable ->
      (state : {s : t | valid s && member x s && member y s &&
        M.is_root (heap s) x && M.is_root (heap s) y}) @ unique read_write total ->
      (fee : {b : C.token | let refine_ state = state in
        C.credits b >= A.link_fee state.#alpha}) @ unique total ghost ->
      {r : result | let refine_ state = state in
        valid r.#state && r.#state.#capacity = state.#capacity &&
        r.#state.#alpha = state.#alpha &&
        r.#state.#paths === F.join (heap state) x y state.#paths &&
        F.addresses r.#state.#paths === F.addresses state.#paths &&
        F.size r.#state.#paths = F.size state.#paths &&
        heap r.#state === M.linked (heap state) x y &&
        r.#value === M.winner (heap state) x y &&
        (let refine_ fee = fee in
          Bigint.add (account r.#state) (C.credits r.#refund) =
            Bigint.add (account state) (C.credits fee) && C.credits r.#refund >=
          Bigint.sub (C.credits fee) (A.link_fee state.#alpha) &&
          account r.#state <= Bigint.add (account state) (A.link_fee state.#alpha))}
      @ unique = fun x y state fee ->
    let refine_ state = state in let refine_ fee = fee in
    let spent = ghost_ (ticks (borrow_ state)) in
    let cap = ghost_ (capacity (borrow_ state)) in let alpha = ghost_ (alpha (borrow_ state)) in
    let paths = ghost_ (contents (borrow_ state)) in
    let before = ghost_ (heap (borrow_ state)) in
    ghost_ (
      valid_def (borrow_ state); contents_def (borrow_ state);
      capacity_def (borrow_ state);
        alpha_def (borrow_ state);
        ticks_def (borrow_ state);
        account_def (borrow_ state);
        heap_def (borrow_ state);
      member_def x (borrow_ state); member_def y (borrow_ state);
      W.heap_def (borrow_ state.#raw); W.balance_def (borrow_ state.#raw);
      R.lookup_ordered cap before paths x; F.lookup_valid before x paths;
      R.bounds cap before (F.lookup x paths);
      R.weight_def before x; M.is_root_def before x; M.rank_def before x;
      J.admissible_def cap before x y paths;
      let u = () in
      let proof : {u : unit | J.admissible cap before x y paths} = refine_ u in
      let refine_ proof = proof in
      F.join_valid before x y paths;
        F.join_addresses before x y paths;
        F.join_complete before x y paths paths;
      J.all_ordered cap before x y paths paths;
      D.link_sums before x y paths;
      B.link_potential cap alpha before x y paths paths;
      B.nonnegative cap alpha (M.linked before x y) (F.join before x y paths);
      A.link_fee_def alpha;
      C.nonnegative (borrow_ state.#raw.W.bank); C.nonnegative (borrow_ fee));
    let raw = state.#raw in
    let memory = raw.W.memory in let bank = raw.W.bank in
    let right : {t : C.token | 0Z <= C.credits bank && 0Z <= C.credits t} =
      refine_ fee in
    let refine_ bank = C.merge bank right in
    let raw = {W.memory; bank} in
    ghost_ (W.heap_def (borrow_ raw); W.balance_def (borrow_ raw));
    let input : {s : W.resource | M.is_root (W.heap s) x &&
      M.is_root (W.heap s) y && M.rank (W.heap s) x + 1 >= 0 && W.balance s >= 7Z}
      = refine_ raw in
    let refine_ linked = W.link x y input in
    let #{W.value; state = raw} = linked in
    ghost_ (W.heap_def (borrow_ raw); W.balance_def (borrow_ raw));
    let after_paths = ghost_ (F.join before x y paths) in
    let target = ghost_ (Bigint.mul 4Z
      (B.potential cap alpha (M.linked before x y) after_paths)) in
    let memory = raw.W.memory in
    let available : {t : C.token | 0Z <= target && target <= C.credits t} =
      refine_ raw.W.bank in
    let refine_ divided = C.split target available in
    let raw = {W.memory; bank = divided.C.left} in
    ghost_ (W.heap_def (borrow_ raw); W.balance_def (borrow_ raw));
    let state = #{raw; paths = after_paths; capacity = cap; alpha;
      spent = ghost_ (Bigint.add spent 7Z)} in
    ghost_ (heap_def (borrow_ state); valid_def (borrow_ state); contents_def (borrow_ state);
      capacity_def (borrow_ state);
        alpha_def (borrow_ state);
        ticks_def (borrow_ state);
        account_def (borrow_ state));
    let result = #{value; state; refund = divided.C.right} in refine_ result


  let union : (x : M.elem) @ immutable -> (y : M.elem) @ immutable ->
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
      @ unique = fun x y state fee ->
    let refine_ state = state in let refine_ fee = fee in
    let alpha = ghost_ (alpha (borrow_ state)) in
    let paths = ghost_ (contents (borrow_ state)) in let before = ghost_ (heap (borrow_ state)) in
    ghost_ (valid_def (borrow_ state); contents_def (borrow_ state);
      capacity_def (borrow_ state);
        alpha_def (borrow_ state);
        ticks_def (borrow_ state);
        account_def (borrow_ state);
        member_def x (borrow_ state);
      member_def y (borrow_ state); representative_def x (borrow_ state);
      A.find_fee_def alpha; A.link_fee_def alpha; A.union_fee_def alpha;
      F.lookup_valid before x paths; F.lookup_closed paths paths x;
      F.refresh_member before (F.lookup x paths) paths y;
      S.find_root before paths x;
      S.find_paths_def paths x; S.find_heap_def before paths x);
    let positive : {b : C.token | C.credits b > 0Z} = refine_ fee in
    let refine_ fee = C.tick positive in
    let input : {s : t | valid s && member x s} = refine_ state in
    let payment : {b : C.token | let refine_ input = input in
      C.credits b >= A.find_fee input.#alpha} = refine_ fee in
    let refine_ first = find x input payment in
    let #{value = root_x; state; refund} = first in
    let middle = ghost_ (heap (borrow_ state)) in
    let first_paths = ghost_ (S.find_paths paths x) in
    ghost_ (valid_def (borrow_ state); contents_def (borrow_ state);
      capacity_def (borrow_ state);
        alpha_def (borrow_ state);
        ticks_def (borrow_ state);
        account_def (borrow_ state);
        member_def y (borrow_ state);
      representative_def y (borrow_ state);
      F.lookup_valid middle y first_paths;
      F.refresh_member middle (F.lookup y first_paths) first_paths root_x;
      D.compressed_root middle (F.lookup y first_paths) root_x;
      S.find_root middle first_paths y;
      S.find_paths_def first_paths y; S.find_heap_def middle first_paths y);
    let input : {s : t | valid s && member y s} = refine_ state in
    let payment : {b : C.token | let refine_ input = input in
      C.credits b >= A.find_fee input.#alpha} = refine_ refund in
    let refine_ second = find y input payment in
    let #{value = root_y; state; refund} = second in
    ghost_ (valid_def (borrow_ state); contents_def (borrow_ state);
      capacity_def (borrow_ state);
        alpha_def (borrow_ state);
        ticks_def (borrow_ state);
        account_def (borrow_ state);
        member_def root_x (borrow_ state);
      member_def root_y (borrow_ state));
    let input : {s : t | valid s && member root_x s && member root_y s &&
      M.is_root (heap s) root_x && M.is_root (heap s) root_y} = refine_ state in
    let payment : {b : C.token | let refine_ input = input in
      C.credits b >= A.link_fee input.#alpha} = refine_ refund in
    let refine_ result = link root_x root_y input payment in
    ghost_ (S.union_paths_def before paths x y; S.union_heap_def before paths x y;
      S.union_root_def before paths x y);
    let #{value; state; refund} = result in
    let capacity = ghost_ (capacity (borrow_ state)) in
    let paths = ghost_ (contents (borrow_ state)) in
    let spent = ghost_ (ticks (borrow_ state)) in
    ghost_ (valid_def (borrow_ state); heap_def (borrow_ state);
      contents_def (borrow_ state); capacity_def (borrow_ state);
      alpha_def (borrow_ state); ticks_def (borrow_ state); account_def (borrow_ state));
    let raw = state.#raw in
    let state = #{raw; paths; capacity; alpha; spent = ghost_ (Bigint.add spent 1Z)} in
    ghost_ (valid_def (borrow_ state); heap_def (borrow_ state); account_def (borrow_ state));
    let result = #{value; state; refund} in
    refine_ result

end
