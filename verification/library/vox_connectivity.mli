module Make (C : Vox_big_credits.S) : sig
  type elem : immutable_data
  type t : (void & void & void & void & void) & void & void
  type result = #{value : elem @@ aliased; state : t}

  type snapshot : immutable_data
  val snapshot : t @ local immutable total ghost forkable unyielding ->
    snapshot @ immutable ghost @@ total
  val contains : snapshot @ immutable -> elem @ immutable ->
    bool @ ghost @@ total
  val root : snapshot @ immutable -> elem @ immutable ->
    elem @ immutable ghost @@ total
  val connected : snapshot @ immutable -> elem @ immutable ->
    elem @ immutable -> bool @ ghost @@ total
  val added : snapshot @ immutable -> snapshot @ immutable ->
    elem @ immutable -> bool @ ghost @@ total
  val found : snapshot @ immutable -> snapshot @ immutable ->
    elem @ immutable -> bool @ ghost @@ total
  val joined : snapshot @ immutable -> snapshot @ immutable ->
    elem @ immutable -> elem @ immutable -> elem @ immutable ->
    bool @ ghost @@ total
  val added_law : (before : snapshot) @ immutable ->
      (after : snapshot) @ immutable -> (x : elem) @ immutable ->
      (q : elem) @ immutable ->
      {u : unit | if added before after x then
        not (contains before x) &&
        contains after q = (q === x || contains before q) &&
        root after x === x &&
        (if contains before q then root after q === root before q &&
          not (root before q === x) else true)
        else true} @ ghost @@ total
  val found_law : (before : snapshot) @ immutable ->
      (after : snapshot) @ immutable -> (x : elem) @ immutable ->
      (q : elem) @ immutable ->
      {u : unit | if found before after x then
        contains after q = contains before q && root after q === root before q
        else true} @ ghost @@ total
  val joined_law : (before : snapshot) @ immutable ->
      (after : snapshot) @ immutable -> (x : elem) @ immutable ->
      (y : elem) @ immutable -> (r : elem) @ immutable ->
      (q : elem) @ immutable ->
      {u : unit | if joined before after x y r then
        (r === root before x || r === root before y) &&
        contains after q = contains before q &&
        (if contains before q then root after q ===
          (if root before q === root before x || root before q === root before y
           then r else root before q) else true) else true} @ ghost @@ total
  val connected_def : (p : snapshot) @ immutable ->
    (x : elem) @ immutable -> (y : elem) @ immutable ->
    {u : unit | connected p x y ===
      (ghost_ (contains p x && contains p y && root p x === root p y))} @@ total

  val size :
    t @ local immutable total ghost forkable unyielding ->
    Bigint.t @ ghost @@ total
  val ticks :
    t @ local immutable total ghost forkable unyielding ->
    Bigint.t @ ghost @@ total
  val account :
    t @ local immutable total ghost forkable unyielding ->
    Bigint.t @ ghost @@ total
  val valid :
    t @ local immutable total ghost forkable unyielding ->
    bool @ ghost @@ total
  val find_fee :
    t @ local immutable total ghost forkable unyielding ->
    Bigint.t @ ghost @@ total
  val union_fee :
    t @ local immutable total ghost forkable unyielding ->
    Bigint.t @ ghost @@ total
  val member : elem @ immutable ->
    t @ local immutable total ghost forkable unyielding -> bool @ ghost @@ total
  val representative : elem @ immutable ->
    t @ local immutable total ghost forkable unyielding ->
    elem @ immutable ghost @@ total
  val observe : (x : elem) @ immutable ->
      (state : t) @ local immutable total ghost forkable unyielding ->
      {u : unit | contains (snapshot state) x = member x state &&
        root (snapshot state) x === representative x state} @ ghost @@ total

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
      {u : unit | if valid state && 1Z <= population &&
        size state <= population &&
        1Z <= a && Vox_ackermann.iter population a 1Z 1Z >= population &&
          Vox_ackermann.below population a
        then find_fee state <= Bigint.add (Bigint.mul 4Z a) 12Z &&
          union_fee state <= Bigint.add (Bigint.mul 12Z a) 36Z else true}
      @ ghost @@ total

  val create : (fee : {b : C.token | C.credits b = 1Z}) @ unique total ghost ->
      {s : t | valid s && size s = 0Z && account s = 1Z} @ unique

  val make_set :
      (state : {s : t | valid s && size s < Bigint.of_int max_int}) @ unique
        read_write total ->
      (fee : {b : C.token | C.credits b = 11Z}) @ unique total ghost ->
      {r : result | let refine_ state = state in valid r.#state &&
        added (snapshot state) (snapshot r.#state) r.#value &&
        size r.#state = Bigint.add (size state) 1Z &&
        member r.#value r.#state &&
        account r.#state = Bigint.add (account state) 11Z} @ unique

  val find : (x : elem) @ immutable ->
      (state : {s : t | valid s && member x s}) @ unique read_write total ->
      (fee : {b : C.token | let refine_ state = state in
        C.credits b = find_fee state})
        @ unique total ghost ->
      {r : result | let refine_ state = state in valid r.#state &&
        found (snapshot state) (snapshot r.#state) x &&
        size r.#state = size state && r.#value === representative x state &&
        account r.#state = Bigint.add (account state) (find_fee state)} @ unique

  val union : (x : elem) @ immutable -> (y : elem) @ immutable ->
      (state : {s : t | valid s && member x s && member y s}) @ unique
        read_write total ->
      (fee : {b : C.token | let refine_ state = state in
        C.credits b = union_fee state})
        @ unique total ghost ->
      {r : result | let refine_ state = state in valid r.#state &&
        joined (snapshot state) (snapshot r.#state) x y r.#value &&
        size r.#state = size state &&
        account r.#state = Bigint.add (account state) (union_fee state)}
      @ unique

end
