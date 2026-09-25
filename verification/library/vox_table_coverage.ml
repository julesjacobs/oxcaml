module P = Vox_table_probe
module W = Vox_table_wrap
module M = Vox_table_model

let (addmod_associative @ total)
    (modulus : {m : int | 0 < m && m <= 1073741824})
    (left : {n : int | 0 <= n && n < modulus})
    (middle : {n : int | 0 <= n && n < modulus})
    (right : {n : int | 0 <= n && n <= modulus}) :
    {u : unit | P.addmod modulus (P.addmod modulus left middle) right =
      P.addmod modulus left (P.addmod modulus middle right)} @ ghost = ghost_ (
  P.addmod_def modulus left middle; P.addmod_def modulus middle right;
  P.addmod_def modulus (P.addmod modulus left middle) right;
  P.addmod_def modulus left (P.addmod modulus middle right);
  ())

let (addmod_range @ total)
    (modulus : {m : int | 0 < m && m <= 1073741824})
    (value : {v : int | 0 <= v && v < modulus})
    (delta : {d : int | 0 <= d && d <= modulus}) :
    {u : unit | 0 <= P.addmod modulus value delta &&
      P.addmod modulus value delta < modulus} @ ghost = ghost_ (
  P.addmod_def modulus value delta; ())

module Make (Key : Vox_table_map.Key)
    (I : module type of Vox_table_invariant.Make (Key)) = struct
  let rec (probe_triangle @ total) :
      (capacity : W.capacity) ->
      (groups : {n : int | 0 < n && n <= 67108864 && capacity = W.scale16 n}) ->
      (hash : int) -> (rank : {r : int | 0 <= r && r <= groups}) ->
      {u : unit | I.probe capacity hash rank ===
        (P.addmod capacity (I.wrap capacity (hash lsr 7))
          (W.scale16 (P.triangle groups rank)), W.scale16 (rank + 1))}
      @ ghost = fun capacity groups hash rank -> ghost_ (
    I.probe_def capacity hash rank;
    P.triangle_def groups rank;
    I.wrap_def capacity (hash lsr 7);
    W.wrap_range capacity (hash lsr 7);
    W.scale16_def groups; W.scale16_def rank; W.scale16_def (rank + 1);
    if rank = 0 then begin
      W.scale16_def 0; W.scale16_def 1;
      P.addmod_def capacity (I.wrap capacity (hash lsr 7)) 0;
      ()
    end else begin
      probe_triangle capacity groups hash (rank - 1);
      W.scale16_def (rank - 1 + 1);
      P.triangle_range groups (rank - 1);
      W.scale16_def (P.triangle groups (rank - 1));
      W.scale16_def (P.triangle groups rank);
      W.scale_addmod groups (P.triangle groups (rank - 1)) rank;
      addmod_range capacity (I.wrap capacity (hash lsr 7))
        (W.scale16 (P.triangle groups (rank - 1)));
      W.wrap_add capacity
        (P.addmod capacity (I.wrap capacity (hash lsr 7))
          (W.scale16 (P.triangle groups (rank - 1))))
        (W.scale16 rank);
      I.wrap_def capacity
        (P.addmod capacity (I.wrap capacity (hash lsr 7))
          (W.scale16 (P.triangle groups (rank - 1))) + W.scale16 rank);
      addmod_associative capacity (I.wrap capacity (hash lsr 7))
        (W.scale16 (P.triangle groups (rank - 1))) (W.scale16 rank);
      ()
    end)
    [@@decreases rank]
  let (groups_capacity @ total) (capacity : int) (plan : P.plan @ immutable) :
      {u : unit | not (16 <= capacity && capacity <= 1073741824 &&
        P.valid plan && capacity = W.scale16 (P.groups plan)) ||
        0 < P.groups plan && P.groups plan <= 67108864 &&
        capacity lsr 4 = P.groups plan} @ ghost = ghost_ (
    P.groups_range plan; W.scale16_def (P.groups plan); ())

  let (distance_bounds @ total) (capacity : int) (start : int) (index : int) :
      {u : unit | not (16 <= capacity && capacity <= 1073741824 &&
        0 <= start && start < capacity && 0 <= index && index < capacity) ||
        (let distance = if start <= index then index - start else capacity -
          start + index in
         0 <= distance && distance < capacity && P.addmod capacity start
           distance = index)}
      @ ghost = ghost_ (
    let distance = if start <= index then index - start else capacity - start
      + index in
    P.addmod_def capacity start distance; ())

  let (block_bounds @ total) (capacity : int) (groups : int) (distance : int) :
      {u : unit | not (16 <= capacity && capacity <= 1073741824 &&
        0 < groups && groups <= 67108864 && capacity = W.scale16 groups &&
        0 <= distance && distance < capacity) ||
        0 <= distance lsr 4 && distance lsr 4 < groups &&
        0 <= W.scale16 (distance lsr 4) && W.scale16 (distance lsr 4) <
          capacity}
      @ ghost = ghost_ (
    W.scale16_def groups; W.scale16_def (distance lsr 4); ())

  let (cover @ total) (capacity : W.capacity)
      (plan : {p : P.plan | P.valid p && capacity = W.scale16 (P.groups p)}
        @ immutable)
      (hash : int) (index : {i : int | 0 <= i && i < capacity}) :
      {path : int * int | let rank, lane = path in
        0 <= rank && rank < (capacity lsr 4) && 0 <= lane && lane < 16 &&
        index = I.wrap capacity (I.group capacity hash rank + lane)} @ ghost =
          ghost_ (
    groups_capacity capacity plan;
    let start = I.wrap capacity (hash lsr 7) in
    I.wrap_def capacity (hash lsr 7); W.wrap_range capacity (hash lsr 7);
    let distance = if start <= index then index - start else capacity - start
      + index in
    distance_bounds capacity start index;
    block_bounds capacity (P.groups plan) distance;
    W.split16 distance;
    let block = distance lsr 4 in
    let lane = distance land 15 in
    P.inverse_at plan block;
    let rank = P.inverse plan block in
    probe_triangle capacity (P.groups plan) hash rank;
    I.group_def capacity hash rank;
    let group = I.group capacity hash rank in
    addmod_range capacity start (W.scale16 block);
    W.wrap_add capacity group lane;
    I.wrap_def capacity (group + lane);
    addmod_associative capacity start (W.scale16 block) lane;
    P.addmod_def capacity (W.scale16 block) lane;
    (rank, lane))

  let rec (empty_free_at @ total) : ('a : immutable_data).
      (model : (Key.t, 'a) M.state) @ immutable -> (hash : int) ->
      (count : int) -> (rank : int) ->
      {u : unit | not (I.empty_free model hash count && 0 <= rank && rank <
        count) ||
        M.matching model (I.group model.capacity hash rank) 128 16 = 0}
      @ ghost = fun model hash count rank -> ghost_ (
    I.empty_free_def model hash count;
    if count > 0 && rank < count - 1 then empty_free_at model hash (count - 1)
      rank;
    ())
    [@@decreases if count > 0 then count else 0]

end
