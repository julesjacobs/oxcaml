module M = Vox_union_find_model
module F = Vox_union_find_forest
module S = Vox_union_find_spec
module A = Vox_union_find_amortized
module K = Vox_ackermann
module P = Ghost_pref
module H = P.Heap

module Make (C : Vox_big_credits.S) = struct
  type elem = M.elem
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

  module R = Vox_union_find_rank
  let[@def] events (s : t @ local immutable total ghost forkable unyielding) =
    ghost_ (U.events s.#core)
  let (event_cost @ total)
      (s : t @ local immutable total ghost forkable unyielding) :
      {u : unit | if valid s then
        ticks s = Vox_union_find_events.total (events s) else true} @ ghost =
    ghost_ (valid_def s; ticks_def s; events_def s; U.event_cost s.#core; ())

  type snapshot = { paths : M.path list; memory : Vox_union_find_model.node P.heap; capacity : Bigint.t }
  let[@def] snapshot (s : t @ local immutable total ghost forkable unyielding) =
    ghost_ { paths = contents s; memory = heap s; capacity = U.capacity s.#core
      }
  let[@def] contains (p : snapshot @ immutable) (x : M.elem @ immutable) =
    ghost_ (F.member x p.paths)
  let[@def] root (p : snapshot @ immutable) (x : M.elem @ immutable) =
    ghost_ (F.representative x p.paths)
  let[@def] connected (p : snapshot @ immutable)
      (x : M.elem @ immutable) (y : M.elem @ immutable) =
    ghost_ (contains p x && contains p y && root p x === root p y)
  let[@def] sound (p : snapshot @ immutable) = ghost_ (
    F.valid p.memory p.paths && F.complete p.paths p.paths &&
    R.all_ordered p.capacity p.memory p.paths &&
    p.capacity <= Bigint.of_int max_int)
  let (snapshot_valid @ total) :
      (state : t) @ local immutable total ghost forkable unyielding ->
      {u : unit | if valid state then sound (snapshot state) else true}
      @ ghost = fun state -> ghost_ (
    valid_def (borrow_ state); snapshot_def (borrow_ state);
    contents_def (borrow_ state); heap_def (borrow_ state);
    sound_def (snapshot state); U.model_valid (borrow_ state.#core);
    ())
  let[@def] added (before : snapshot @ immutable) (after : snapshot @ immutable)
      (x : M.elem @ immutable) = ghost_ (
    sound before && not (H.mem before.memory x) &&
    after.paths === M.Stop x :: before.paths)
  let[@def] found (before : snapshot @ immutable) (after : snapshot @ immutable)
      (x : M.elem @ immutable) = ghost_ (
    sound before && contains before x &&
    after.paths === F.refresh (F.lookup x before.paths) before.paths &&
    F.addresses after.paths === F.addresses before.paths)
  let[@def] joined (before : snapshot @ immutable) (after : snapshot @
    immutable)
      (x : M.elem @ immutable) (y : M.elem @ immutable) (r : M.elem @ immutable)
        =
    ghost_ (sound before && contains before x && contains before y &&
    after.paths === S.union_paths before.memory before.paths x y &&
    F.addresses after.paths === F.addresses before.paths &&
    r === S.union_root before.memory before.paths x y)

  let (observe @ total) : (x : M.elem) @ immutable ->
      (state : t) @ local immutable total ghost forkable unyielding ->
      {u : unit | contains (snapshot state) x = member x state &&
        root (snapshot state) x === representative x state} @ ghost =
      fun x state -> ghost_ (
    snapshot_def (borrow_ state); contains_def (snapshot state) x;
    root_def (snapshot state) x; member_def x (borrow_ state);
    representative_def x (borrow_ state); ())

  let rec (empty_paths @ total) : (paths : M.path list) @ immutable ->
      (x : M.elem) @ immutable ->
      {u : unit | F.size paths >= 0Z &&
        (if F.size paths = 0Z then not (F.member x paths) else true)} @ ghost =
      fun paths x -> ghost_ (
    F.size_def paths; F.member_def x paths;
    (match paths with [] -> () | _ :: rest -> empty_paths rest x);
    ())

  let (empty_law @ total) :
      (state : t) @ local immutable total ghost forkable unyielding ->
      (x : M.elem) @ immutable ->
      {u : unit | if size state = 0Z then
        not (contains (snapshot state) x) else true} @ ghost =
      fun state x -> ghost_ (
    size_def (borrow_ state); snapshot_def (borrow_ state);
    contains_def (snapshot state) x; empty_paths (contents state) x;
    ())

  let (added_law @ total) : (before : snapshot) @ immutable ->
      (after : snapshot) @ immutable -> (x : M.elem) @ immutable ->
      (q : M.elem) @ immutable ->
      {u : unit | if added before after x then
        not (contains before x) &&
        contains after q = (q === x || contains before q) &&
        root after x === x &&
        (if contains before q then root after q === root before q &&
          not (root before q === x) else true)
        else true} @ ghost = fun before after x q -> ghost_ (
    added_def before after x; sound_def before;
    contains_def before x; contains_def before q; contains_def after q;
    root_def after x; root_def after q; root_def before q;
    F.lookup_valid before.memory q before.paths;
    F.lookup_valid before.memory x before.paths;
    M.terminal before.memory (F.lookup q before.paths);
    M.is_root_def before.memory (M.root (F.lookup q before.paths));
    F.member_def q (M.Stop x :: before.paths); M.head_def (M.Stop x);
    F.representative_def x (M.Stop x :: before.paths);
    F.representative_def q (M.Stop x :: before.paths);
    F.representative_def q before.paths;
    F.lookup_def x (M.Stop x :: before.paths);
    F.lookup_def q (M.Stop x :: before.paths); M.root_def (M.Stop x);
    ())

  let (found_law @ total) : (before : snapshot) @ immutable ->
      (after : snapshot) @ immutable -> (x : M.elem) @ immutable ->
      (q : M.elem) @ immutable ->
      {u : unit | if found before after x then
        contains after q = contains before q && root after q === root before q
        else true} @ ghost = fun before after x q -> ghost_ (
    found_def before after x; sound_def before;
    contains_def before x; contains_def before q; contains_def after q;
    root_def before q; root_def after q;
    F.lookup_valid before.memory x before.paths;
    F.refresh_representative before.memory (F.lookup x before.paths)
      before.paths q;
    F.member_same before.paths after.paths q;
    ())

  let (joined_law @ total) : (before : snapshot) @ immutable ->
      (after : snapshot) @ immutable -> (x : M.elem) @ immutable ->
      (y : M.elem) @ immutable -> (r : M.elem) @ immutable ->
      (q : M.elem) @ immutable ->
      {u : unit | if joined before after x y r then
        (r === root before x || r === root before y) &&
        contains after q = contains before q &&
        (if contains before q then root after q ===
          (if root before q === root before x || root before q === root before y
           then r else root before q) else true) else true} @ ghost =
      fun before after x y r q -> ghost_ (
    joined_def before after x y r; sound_def before;
    contains_def before x; contains_def before y; contains_def before q;
    contains_def after q; root_def before x; root_def before y;
    root_def before q; root_def after q;
    let h = before.memory in let paths = before.paths in
    let cap = before.capacity in let px = F.lookup x paths in
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
    F.refresh_representative h px paths y;
    F.representative_def y paths;
    S.union_root_def h paths x y;
    M.winner_def (S.find_heap middle first y)
      (F.representative x paths) (F.representative y first);
    F.member_same before.paths after.paths q;
    ())

  let (account_bounds @ total) :
      (state : t) @ local immutable total ghost forkable unyielding ->
      {u : unit | ticks state <= account state} @ ghost = fun state -> ghost_ (
    ticks_def (borrow_ state); account_def (borrow_ state);
    U.account_bounds (borrow_ state.#core); C.nonnegative (borrow_ state.#savings);
    ())

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
    ())
  let create : (fee : {b : C.token | C.credits b = 1Z}) @ unique total ghost ->
      {s : t | valid s && size s = 0Z && contents s === [] && account s = 1Z} @ unique =
      fun fee ->
    (* The imported max_int has no refinement; check it once at creation. *)
    let checked : {u : unit | 1Z <= Bigint.of_int max_int} =
      if max_int >= 1 then (())
      else invalid_arg "Vox_union_find_online.create" in
    let _ = checked in
    let fee = fee in
    let capacity = ghost_ 1Z in
    let cap : {n : Bigint.t | 1Z <= n && n <= Bigint.of_int max_int} = capacity in
    let payment : {b : C.token | C.credits b >= 1Z} = fee in
    let r = U.create cap payment in
    let #{U.state = core; refund = savings} = r in
    let epoch = ghost_ 1Z in
    let state = #{core; savings; epoch} in
    ghost_ (valid_def (borrow_ state); contents_def (borrow_ state);
      size_def (borrow_ state); account_def (borrow_ state);
      U.contents_def (borrow_ state.#core); U.capacity_def (borrow_ state.#core);
      F.size_def []; K.minimum_def 1Z (Bigint.of_int max_int));
    state

  let make_set :
      (state : {s : t | valid s && size s < Bigint.of_int max_int}) @ unique read_write total ->
      (fee : {b : C.token | C.credits b = 11Z}) @ unique total ghost ->
      {r : result | let state = state in valid r.#state &&
        added (snapshot state) (snapshot r.#state) r.#value &&
        contents r.#state === M.Stop r.#value :: contents state &&
        size r.#state = Bigint.add (size state) 1Z && member r.#value r.#state &&
        not (H.mem (heap state) r.#value) &&
        heap r.#state === H.put (heap state) r.#value (M.Root 0) &&
        account r.#state = Bigint.add (account state) 11Z} @ unique =
      fun state fee ->
    let state = state in
    let previous = ghost_ (snapshot (borrow_ state)) in
    ghost_ (snapshot_valid (borrow_ state); snapshot_def (borrow_ state)); let
      fee = fee in
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
    let available : {b : C.token | 0Z <= eight && eight <= C.credits b} = fee in
    let deposit = C.split eight available in
    let savings = state.#savings in let core = state.#core in
    let right : {b : C.token | 0Z <= C.credits savings && 0Z <= C.credits b} =
      deposit.C.left in
    let savings = C.merge savings right in
    let available : {b : C.token | 0Z <= unlocked && unlocked <= C.credits b} = savings in
    let funds = C.split unlocked available in
    let input : {s : U.t | U.valid s && s.#U.capacity <= cap &&
      cap <= Bigint.mul 2Z s.#U.capacity && cap <= Bigint.of_int max_int} = core in
    let payment : {b : C.token | let input = input in
      C.credits b >= (if cap = input.#U.capacity then 0Z
        else Bigint.mul 4Z (F.size input.#U.paths))} = funds.C.left in
    let grown = U.reparameterize cap input payment in
    let core = grown.#U.state in let savings = funds.C.right in
    ghost_ (C.nonnegative (borrow_ savings); C.nonnegative (borrow_ grown.#U.refund));
    let right : {b : C.token | 0Z <= C.credits savings && 0Z <= C.credits b} =
      grown.#U.refund in
    let savings = C.merge savings right in
    let input : {s : U.t | U.valid s && F.size s.#U.paths < s.#U.capacity} = core in
    let payment : {b : C.token | C.credits b >= 3Z} = deposit.C.right in
    let r = U.make_set input payment in
    let #{U.value; state = core; refund} = r in
    ghost_ (C.nonnegative (borrow_ savings); C.nonnegative (borrow_ refund));
    let right : {t : C.token | 0Z <= C.credits savings && 0Z <= C.credits t} =
      refund in
    let savings = C.merge savings right in
    let state = #{core; savings; epoch} in
    ghost_ (valid_def (borrow_ state); contents_def (borrow_ state);
      heap_def (borrow_ state); size_def (borrow_ state); account_def (borrow_ state);
      U.contents_def (borrow_ state.#core); U.capacity_def (borrow_ state.#core));
    ghost_ (size_def (borrow_ state); contents_def (borrow_ state);
      F.size_def (contents (borrow_ state));
      member_def value (borrow_ state); F.member_def value (contents (borrow_ state)); M.head_def (M.Stop value));
    ghost_ (snapshot_def (borrow_ state);
      added_def previous (snapshot (borrow_ state)) value);
    let result = #{value; state} in result

  let find : (x : M.elem) @ immutable ->
      (state : {s : t | valid s && member x s}) @ unique read_write total ->
      (fee : {b : C.token | let state = state in C.credits b = find_fee state})
        @ unique total ghost ->
      {r : result | let state = state in valid r.#state &&
        found (snapshot state) (snapshot r.#state) x &&
        contents r.#state === F.refresh (F.lookup x (contents state)) (contents state) &&
        F.addresses (contents r.#state) === F.addresses (contents state) &&
        size r.#state = size state && r.#value === representative x state &&
        account r.#state = Bigint.add (account state) (find_fee state)}
      @ unique = fun x state fee ->
    let state = state in
    let previous = ghost_ (snapshot (borrow_ state)) in
    ghost_ (snapshot_valid (borrow_ state); snapshot_def (borrow_ state)); let
      fee = fee in
    ghost_ (valid_def (borrow_ state); contents_def (borrow_ state);
      heap_def (borrow_ state); size_def (borrow_ state); account_def (borrow_ state);
      U.contents_def (borrow_ state.#core); U.capacity_def (borrow_ state.#core);
      U.alpha_def (borrow_ state.#core));
    ghost_ (member_def x (borrow_ state); U.member_def x (borrow_ state.#core);
      find_fee_def (borrow_ state); representative_def x (borrow_ state);
      U.representative_def x (borrow_ state.#core));
    ghost_ (contains_def previous x);
    let epoch = ghost_ state.#epoch in
    let savings = state.#savings in
    let core = state.#core in
    let input : {s : U.t | U.valid s && U.member x s} = core in
    let payment : {b : C.token | let input = input in
      C.credits b >= A.find_fee input.#U.alpha} = fee in
    let r = U.find x input payment in
    let #{U.value; state = core; refund} = r in
    ghost_ (C.nonnegative (borrow_ savings); C.nonnegative (borrow_ refund));
    let right : {t : C.token | 0Z <= C.credits savings && 0Z <= C.credits t} =
      refund in
    let savings = C.merge savings right in
    let state = #{core; savings; epoch} in
    ghost_ (valid_def (borrow_ state); contents_def (borrow_ state);
      heap_def (borrow_ state); size_def (borrow_ state); account_def (borrow_ state);
      U.contents_def (borrow_ state.#core); U.capacity_def (borrow_ state.#core));
    ghost_ (snapshot_def (borrow_ state);
      found_def previous (snapshot (borrow_ state)) x);
    let result = #{value; state} in result

  let union : (x : M.elem) @ immutable -> (y : M.elem) @ immutable ->
      (state : {s : t | valid s && member x s && member y s}) @ unique read_write total ->
      (fee : {b : C.token | let state = state in C.credits b = union_fee state})
        @ unique total ghost ->
      {r : result | let state = state in valid r.#state &&
        joined (snapshot state) (snapshot r.#state) x y r.#value &&
        contents r.#state === S.union_paths (heap state) (contents state) x y &&
        F.addresses (contents r.#state) === F.addresses (contents state) &&
        size r.#state = size state && r.#value === S.union_root (heap state) (contents state) x y &&
        account r.#state = Bigint.add (account state) (union_fee state)} @ unique =
      fun x y state fee ->
    let state = state in
    let previous = ghost_ (snapshot (borrow_ state)) in
    ghost_ (snapshot_valid (borrow_ state); snapshot_def (borrow_ state)); let
      fee = fee in
    ghost_ (valid_def (borrow_ state); contents_def (borrow_ state);
      heap_def (borrow_ state); size_def (borrow_ state); account_def (borrow_ state);
      U.contents_def (borrow_ state.#core); U.capacity_def (borrow_ state.#core);
      U.alpha_def (borrow_ state.#core));
    ghost_ (member_def x (borrow_ state); U.member_def x (borrow_ state.#core);
      union_fee_def (borrow_ state); representative_def x (borrow_ state);
      U.representative_def x (borrow_ state.#core);
      member_def y (borrow_ state); U.member_def y (borrow_ state.#core));
    ghost_ (contains_def previous x; contains_def previous y);
    let epoch = ghost_ state.#epoch in
    let savings = state.#savings in
    let core = state.#core in
    let input : {s : U.t | U.valid s && U.member x s && U.member y s} = core in
    let payment : {b : C.token | let input = input in
      C.credits b >= A.union_fee input.#U.alpha} = fee in
    let r = U.union x y input payment in
    let #{U.value; state = core; refund} = r in
    ghost_ (C.nonnegative (borrow_ savings); C.nonnegative (borrow_ refund));
    let right : {t : C.token | 0Z <= C.credits savings && 0Z <= C.credits t} =
      refund in
    let savings = C.merge savings right in
    let state = #{core; savings; epoch} in
    ghost_ (valid_def (borrow_ state); contents_def (borrow_ state);
      heap_def (borrow_ state); size_def (borrow_ state); account_def (borrow_ state);
      U.contents_def (borrow_ state.#core); U.capacity_def (borrow_ state.#core));
    ghost_ (snapshot_def (borrow_ state);
      joined_def previous (snapshot (borrow_ state)) x y value);
    let result = #{value; state} in result

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
        let u = () in K.inverse_order cap population b a (u))
      else (
        let u = () in K.inverse_doubling population cap a b (u));
      find_fee_def (borrow_ state); union_fee_def (borrow_ state);
      A.find_fee_def b; A.union_fee_def b;
      ())
    else ())

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
    ())

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
    ())

  let create_connectivity : (fee : {b : C.token | C.credits b = 1Z}) @ unique
    total ghost ->
      {s : t | valid s && size s = 0Z && account s = 1Z} @ unique = fun fee ->
    let result = create fee in
    result

  let make_set_connectivity :
      (state : {s : t | valid s && size s < Bigint.of_int max_int}) @ unique
        read_write total ->
      (fee : {b : C.token | C.credits b = 11Z}) @ unique total ghost ->
      {r : result | let state = state in valid r.#state &&
        added (snapshot state) (snapshot r.#state) r.#value &&
        size r.#state = Bigint.add (size state) 1Z &&
        contains (snapshot r.#state) r.#value &&
        account r.#state = Bigint.add (account state) 11Z} @ unique =
      fun state fee ->
    let state = state in
    let fee = fee in
    let result = make_set state fee in
    let #{value; state} = result in
    ghost_ (observe value (borrow_ state));
    #{value; state}

  let find_connectivity : (x : elem) @ immutable ->
      (state : {s : t | valid s && contains (snapshot s) x}) @ unique read_write total ->
      (fee : {b : C.token | let state = state in
        C.credits b = find_fee state})
        @ unique total ghost ->
      {r : result | let state = state in valid r.#state &&
        found (snapshot state) (snapshot r.#state) x &&
        size r.#state = size state && r.#value === root (snapshot state) x &&
        account r.#state = Bigint.add (account state) (find_fee state)}
      @ unique = fun x state fee ->
    ghost_ (observe x (borrow_ state));
    let state = state in
    let fee = fee in
    let input : {s : t | valid s && contains (snapshot s) x} = state in
    let payment : {b : C.token | let input = input in
      C.credits b = find_fee input} = fee in
    let result = find x input payment in
    result

  let union_connectivity : (x : elem) @ immutable -> (y : elem) @ immutable ->
      (state : {s : t | valid s && contains (snapshot s) x && contains (snapshot s) y}) @ unique
        read_write total ->
      (fee : {b : C.token | let state = state in
        C.credits b = union_fee state})
        @ unique total ghost ->
      {r : result | let state = state in valid r.#state &&
        joined (snapshot state) (snapshot r.#state) x y r.#value &&
        size r.#state = size state &&
        account r.#state = Bigint.add (account state) (union_fee state)}
      @ unique = fun x y state fee ->
    ghost_ (observe x (borrow_ state); observe y (borrow_ state));
    let state = state in
    let fee = fee in
    let input : {s : t | valid s && contains (snapshot s) x && contains (snapshot s) y} = state in
    let payment : {b : C.token | let input = input in
      C.credits b = union_fee input} = fee in
    let result = union x y input payment in
    result

end
