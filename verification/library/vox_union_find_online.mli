module M = Vox_union_find_model
module F = Vox_union_find_forest
module S = Vox_union_find_spec
module A = Vox_union_find_amortized
module K = Vox_ackermann
module P = Ghost_pref
module H = P.Heap

module Make (C : Vox_big_credits.S) : sig
  type elem = M.elem
  type t : (void & void & void & void & void) & void & void
  type result = #{value : M.elem @@ aliased; state : t}

  type snapshot : immutable_data
  val snapshot : t @ local immutable total ghost forkable unyielding ->
    snapshot @ immutable ghost @@ total
  val contains : snapshot @ immutable -> M.elem @ immutable -> bool @ ghost @@
    total
  val root : snapshot @ immutable -> M.elem @ immutable -> M.elem @ immutable
    ghost @@ total
  val connected : snapshot @ immutable -> M.elem @ immutable -> M.elem @
    immutable -> bool @ ghost @@ total
  val added : snapshot @ immutable -> snapshot @ immutable -> M.elem @ immutable
    -> bool @ ghost @@ total
  val found : snapshot @ immutable -> snapshot @ immutable -> M.elem @ immutable
    -> bool @ ghost @@ total
  val joined : snapshot @ immutable -> snapshot @ immutable -> M.elem @
    immutable -> M.elem @ immutable -> M.elem @ immutable -> bool @ ghost @@
    total
  val added_law : (before : snapshot) @ immutable ->
      (after : snapshot) @ immutable -> (x : M.elem) @ immutable ->
      (q : M.elem) @ immutable ->
      {u : unit | if added before after x then
        not (contains before x) &&
        contains after q = (q === x || contains before q) &&
        root after x === x &&
        (if contains before q then root after q === root before q &&
          not (root before q === x) else true)
        else true} @ ghost @@ total
  val found_law : (before : snapshot) @ immutable ->
      (after : snapshot) @ immutable -> (x : M.elem) @ immutable ->
      (q : M.elem) @ immutable ->
      {u : unit | if found before after x then
        contains after q = contains before q && root after q === root before q
        else true} @ ghost @@ total
  val joined_law : (before : snapshot) @ immutable ->
      (after : snapshot) @ immutable -> (x : M.elem) @ immutable ->
      (y : M.elem) @ immutable -> (r : M.elem) @ immutable ->
      (q : M.elem) @ immutable ->
      {u : unit | if joined before after x y r then
        (r === root before x || r === root before y) &&
        contains after q = contains before q &&
        (if contains before q then root after q ===
          (if root before q === root before x || root before q === root before y
           then r else root before q) else true) else true} @ ghost @@ total
  val connected_def : (p : snapshot) @ immutable ->
    (x : M.elem) @ immutable -> (y : M.elem) @ immutable ->
    {u : unit | connected p x y ===
      (ghost_ (contains p x && contains p y && root p x === root p y))} @@ total

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
  val observe : (x : M.elem) @ immutable ->
      (state : t) @ local immutable total ghost forkable unyielding ->
      {u : unit | contains (snapshot state) x = member x state &&
        root (snapshot state) x === representative x state} @ ghost @@ total
  val size_def : (s : t) @ local immutable total forkable unyielding ->
    {u : unit | size s === (ghost_ (F.size (contents s)))} @@ total
  val member_def : (x : M.elem) @ immutable ->
    (s : t) @ local immutable total forkable unyielding ->
    {u : unit | member x s === (ghost_ (F.member x (contents s)))} @@ total
  val representative_def : (x : M.elem) @ immutable ->
    (s : t) @ local immutable total forkable unyielding ->
    {u : unit | representative x s === (ghost_ (F.representative x (contents s)))} @@ total

  val empty_law :
      (state : t) @ local immutable total ghost forkable unyielding ->
      (x : M.elem) @ immutable ->
      {u : unit | if size state = 0Z then
        not (contains (snapshot state) x) else true} @ ghost @@ total

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
      {r : result | let state = state in valid r.#state &&
        added (snapshot state) (snapshot r.#state) r.#value &&
        contents r.#state === M.Stop r.#value :: contents state &&
        size r.#state = Bigint.add (size state) 1Z && member r.#value r.#state &&
        not (H.mem (heap state) r.#value) &&
        heap r.#state === H.put (heap state) r.#value (M.Root 0) &&
        account r.#state = Bigint.add (account state) 11Z} @ unique

  val find : (x : M.elem) @ immutable ->
      (state : {s : t | valid s && member x s}) @ unique read_write total ->
      (fee : {b : C.token | let state = state in C.credits b = find_fee state})
        @ unique total ghost ->
      {r : result | let state = state in valid r.#state &&
        found (snapshot state) (snapshot r.#state) x &&
        contents r.#state === F.refresh (F.lookup x (contents state)) (contents state) &&
        F.addresses (contents r.#state) === F.addresses (contents state) &&
        size r.#state = size state && r.#value === representative x state &&
        account r.#state = Bigint.add (account state) (find_fee state)} @ unique

  val union : (x : M.elem) @ immutable -> (y : M.elem) @ immutable ->
      (state : {s : t | valid s && member x s && member y s}) @ unique read_write total ->
      (fee : {b : C.token | let state = state in C.credits b = union_fee state})
        @ unique total ghost ->
      {r : result | let state = state in valid r.#state &&
        joined (snapshot state) (snapshot r.#state) x y r.#value &&
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

  val create_connectivity : (fee : {b : C.token | C.credits b = 1Z}) @ unique
    total ghost ->
      {s : t | valid s && size s = 0Z && account s = 1Z} @ unique

  val make_set_connectivity :
      (state : {s : t | valid s && size s < Bigint.of_int max_int}) @ unique
        read_write total ->
      (fee : {b : C.token | C.credits b = 11Z}) @ unique total ghost ->
      {r : result | let state = state in valid r.#state &&
        added (snapshot state) (snapshot r.#state) r.#value &&
        size r.#state = Bigint.add (size state) 1Z &&
        member r.#value r.#state &&
        account r.#state = Bigint.add (account state) 11Z} @ unique

  val find_connectivity : (x : elem) @ immutable ->
      (state : {s : t | valid s && member x s}) @ unique read_write total ->
      (fee : {b : C.token | let state = state in
        C.credits b = find_fee state})
        @ unique total ghost ->
      {r : result | let state = state in valid r.#state &&
        found (snapshot state) (snapshot r.#state) x &&
        size r.#state = size state && r.#value === representative x state &&
        account r.#state = Bigint.add (account state) (find_fee state)} @ unique

  val union_connectivity : (x : elem) @ immutable -> (y : elem) @ immutable ->
      (state : {s : t | valid s && member x s && member y s}) @ unique
        read_write total ->
      (fee : {b : C.token | let state = state in
        C.credits b = union_fee state})
        @ unique total ghost ->
      {r : result | let state = state in valid r.#state &&
        joined (snapshot state) (snapshot r.#state) x y r.#value &&
        size r.#state = size state &&
        account r.#state = Bigint.add (account state) (union_fee state)}
      @ unique

end
