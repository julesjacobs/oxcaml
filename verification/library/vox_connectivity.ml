module Make (C : Vox_big_credits.S) = struct
  module U = Vox_union_find_online.Make (C)
  type elem = U.elem
  type snapshot = U.snapshot
  type t = #{inner : {state : U.t | U.valid state}}
  type result = #{value : elem @@ aliased; state : t}

  let[@def] (contains @ total) : snapshot @ immutable -> elem @ immutable ->
    bool @ ghost =
    fun s x -> ghost_ (U.contains s x)

  let[@def] (root @ total) : snapshot @ immutable -> elem @ immutable ->
    elem @ immutable ghost =
    fun s x -> ghost_ (U.root s x)

  let[@def] (connected @ total) : snapshot @ immutable -> elem @ immutable ->
    elem @ immutable -> bool @ ghost =
    fun s x y -> ghost_ (U.connected s x y)

  let[@def] (added @ total) : snapshot @ immutable -> snapshot @ immutable ->
    elem @ immutable -> bool @ ghost =
    fun before after x -> ghost_ (U.added before after x)

  let[@def] (found @ total) : snapshot @ immutable -> snapshot @ immutable ->
    elem @ immutable -> bool @ ghost =
    fun before after x -> ghost_ (U.found before after x)

  let[@def] (joined @ total) : snapshot @ immutable -> snapshot @ immutable ->
    elem @ immutable -> elem @ immutable -> elem @ immutable ->
    bool @ ghost =
    fun before after x y r -> ghost_ (U.joined before after x y r)

  let (added_law @ total) : (before : snapshot) @ immutable ->
      (after : snapshot) @ immutable -> (x : elem) @ immutable ->
      (q : elem) @ immutable ->
      {u : unit | if added before after x then
        not (contains before x) &&
        contains after q = (q === x || contains before q) &&
        root after x === x &&
        (if contains before q then root after q === root before q &&
          not (root before q === x) else true)
        else true} @ ghost =
    fun before after x q ->
    ghost_ (added_def before after x;
      contains_def before x;
      contains_def before q;
      contains_def after x;
      contains_def after q;
      root_def before x;
      root_def before q;
      root_def after x;
      root_def after q;
      U.added_law before after x q;
      ())

  let (found_law @ total) : (before : snapshot) @ immutable ->
      (after : snapshot) @ immutable -> (x : elem) @ immutable ->
      (q : elem) @ immutable ->
      {u : unit | if found before after x then
        contains after q = contains before q && root after q === root before q
        else true} @ ghost =
    fun before after x q ->
    ghost_ (found_def before after x;
      contains_def before x;
      contains_def before q;
      contains_def after x;
      contains_def after q;
      root_def before x;
      root_def before q;
      root_def after x;
      root_def after q;
      U.found_law before after x q;
      ())

  let (joined_law @ total) : (before : snapshot) @ immutable ->
      (after : snapshot) @ immutable -> (x : elem) @ immutable ->
      (y : elem) @ immutable -> (r : elem) @ immutable ->
      (q : elem) @ immutable ->
      {u : unit | if joined before after x y r then
        (r === root before x || r === root before y) &&
        contains after q = contains before q &&
        (if contains before q then root after q ===
          (if root before q === root before x || root before q === root before y
           then r else root before q) else true) else true} @ ghost =
    fun before after x y r q ->
    ghost_ (joined_def before after x y r;
      contains_def before x;
      contains_def before y;
      contains_def before r;
      contains_def before q;
      contains_def after x;
      contains_def after y;
      contains_def after r;
      contains_def after q;
      root_def before x;
      root_def before y;
      root_def before r;
      root_def before q;
      root_def after x;
      root_def after y;
      root_def after r;
      root_def after q;
      U.joined_law before after x y r q;
      ())

  let (connected_def @ total) : (p : snapshot) @ immutable ->
    (x : elem) @ immutable -> (y : elem) @ immutable ->
    {u : unit | connected p x y ===
      (ghost_ (contains p x && contains p y && root p x === root p y))} =
    fun s x y ->
    ghost_ (connected_def s x y;
      contains_def s x;
      contains_def s y;
      root_def s x;
      root_def s y;
      U.connected_def s x y);
      ()

  let[@def] snapshot (state : t @ local immutable total ghost forkable unyielding) =
    ghost_ (U.snapshot state.#inner)

  let[@def] size (state : t @ local immutable total ghost forkable unyielding) =
    ghost_ (U.size state.#inner)

  let[@def] ticks (state : t @ local immutable total ghost forkable unyielding) =
    ghost_ (U.ticks state.#inner)

  let[@def] account (state : t @ local immutable total ghost forkable unyielding) =
    ghost_ (U.account state.#inner)

  let[@def] find_fee (state : t @ local immutable total ghost forkable unyielding) =
    ghost_ (U.find_fee state.#inner)

  let[@def] union_fee (state : t @ local immutable total ghost forkable unyielding) =
    ghost_ (U.union_fee state.#inner)

  let[@def] events (state : t @ local immutable total ghost forkable unyielding) =
    ghost_ (U.events state.#inner)

  let (empty_law @ total) : (state : t) @ local immutable total ghost forkable unyielding ->
      (x : elem) @ immutable ->
      {u : unit | if size state = 0Z then
        not (contains (snapshot state) x) else true} @ ghost =
    fun state x ->
    ghost_ (size_def state;
      snapshot_def state;
      U.empty_law state.#inner x;
      contains_def (snapshot state) x;
      ())

  let (account_bounds @ total) : (state : t) @ local immutable total ghost forkable unyielding ->
      {u : unit | ticks state <= account state} @ ghost =
    fun state ->
    ghost_ (ticks_def state; account_def state; U.account_bounds state.#inner; ())

  let (observations @ total) : (state : t) @ local immutable total ghost forkable unyielding ->
      {u : unit | 0Z <= size state &&
        size state <= Bigint.of_int max_int && find_fee state >= 12Z &&
        union_fee state >= 36Z} @ ghost =
    fun state ->
    ghost_ (size_def state;
      find_fee_def state;
      union_fee_def state;
      U.observations state.#inner;
      ())

  let (fee_bounds @ total) : (state : t) @ local immutable total ghost forkable unyielding ->
      (population : Bigint.t) -> (a : Bigint.t) ->
      {u : unit | if 1Z <= population &&
        size state <= population &&
        1Z <= a && Vox_ackermann.iter population a 1Z 1Z >= population &&
          Vox_ackermann.below population a
        then find_fee state <= Bigint.add (Bigint.mul 4Z a) 12Z &&
          union_fee state <= Bigint.add (Bigint.mul 12Z a) 36Z else true}
      @ ghost =
    fun state population a ->
    ghost_ (size_def state;
      find_fee_def state;
      union_fee_def state;
      U.fee_bounds state.#inner population a;
      ())

  let (event_cost @ total) : (s : t) @ local immutable total ghost forkable unyielding ->
    {u : unit | ticks s = Vox_union_find_events.total (events s)} @ ghost =
    fun state ->
    ghost_ (ticks_def state; events_def state; U.event_cost state.#inner; ())

  let create : (fee : {b : C.token | C.credits b = 1Z}) @ unique total ghost ->
      {s : t | size s = 0Z && account s = 1Z} @ unique =
    fun fee ->
    let state = U.create_connectivity fee in
    let state : t = #{inner = state} in
    ghost_ (size_def (borrow_ state); account_def (borrow_ state));
    state

  let make_set : (state : {s : t | size s < Bigint.of_int max_int}) @ unique
        read_write total ->
      (fee : {b : C.token | C.credits b = 11Z}) @ unique total ghost ->
      {r : result | let state = state in added (snapshot state) (snapshot r.#state) r.#value &&
        size r.#state = Bigint.add (size state) 1Z &&
        contains (snapshot r.#state) r.#value &&
        account r.#state = Bigint.add (account state) 11Z} @ unique =
    fun state fee ->
    ghost_ (size_def (borrow_ state); account_def (borrow_ state); snapshot_def (borrow_ state));
    let before = ghost_ (snapshot (borrow_ state)) in
    let result = U.make_set_connectivity state.#inner fee in
    let #{U.value; state = next} = result in
    let next : t = #{inner = next} in
    ghost_ (size_def (borrow_ next); account_def (borrow_ next);
      snapshot_def (borrow_ next);
      added_def before (snapshot (borrow_ next)) value;
      contains_def (snapshot (borrow_ next)) value);
    #{value; state = next}

  let find : (x : elem) @ immutable ->
      (state : {s : t | contains (snapshot s) x}) @ unique read_write total ->
      (fee : {b : C.token | let state = state in
        C.credits b = find_fee state})
        @ unique total ghost ->
      {r : result | let state = state in found (snapshot state) (snapshot r.#state) x &&
        size r.#state = size state && r.#value === root (snapshot state) x &&
        account r.#state = Bigint.add (account state) (find_fee state)} @ unique =
    fun x state fee ->
    ghost_ (size_def (borrow_ state);
      account_def (borrow_ state);
      snapshot_def (borrow_ state);
      find_fee_def (borrow_ state));
    ghost_ (contains_def (snapshot (borrow_ state)) x);
    let before = ghost_ (snapshot (borrow_ state)) in
    let result = U.find_connectivity x state.#inner fee in
    let #{U.value; state = next} = result in
    let next : t = #{inner = next} in
    ghost_ (size_def (borrow_ next); account_def (borrow_ next);
      snapshot_def (borrow_ next); found_def before (snapshot (borrow_ next)) x; root_def before x);
    #{value; state = next}

  let union : (x : elem) @ immutable -> (y : elem) @ immutable ->
      (state : {s : t | contains (snapshot s) x && contains (snapshot s) y}) @ unique
        read_write total ->
      (fee : {b : C.token | let state = state in
        C.credits b = union_fee state})
        @ unique total ghost ->
      {r : result | let state = state in joined (snapshot state) (snapshot r.#state) x y r.#value &&
        size r.#state = size state &&
        account r.#state = Bigint.add (account state) (union_fee state)}
      @ unique =
    fun x y state fee ->
    ghost_ (size_def (borrow_ state);
      account_def (borrow_ state);
      snapshot_def (borrow_ state);
      union_fee_def (borrow_ state));
    ghost_ (contains_def (snapshot (borrow_ state)) x; contains_def (snapshot (borrow_ state)) y);
    let before = ghost_ (snapshot (borrow_ state)) in
    let result = U.union_connectivity x y state.#inner fee in
    let #{U.value; state = next} = result in
    let next : t = #{inner = next} in
    ghost_ (size_def (borrow_ next); account_def (borrow_ next);
      snapshot_def (borrow_ next); joined_def before (snapshot (borrow_ next)) x y value);
    #{value; state = next}
end
