module M = Vox_union_find_model
module P = Ghost_pref
module H = P.Heap

let[@def] cost (depth : Bigint.t) = Bigint.add (Bigint.mul 4Z depth) 2Z

module Make (C : Vox_big_credits.S) = struct
  type resource = {
    memory : P.token @@ ghost;
    bank : C.token @@ ghost total;
  }
  type result = #{ value : M.elem @@ aliased; state : resource }

  let[@def] heap (s : resource @ local immutable total ghost) =
    ghost_ (P.own s.memory)
  let[@def] balance (s : resource @ local immutable total ghost) =
    ghost_ (C.credits s.bank)

  let rec (charge @ total) : (amount : int) ->
      (bank : {t : C.token | 0 <= amount &&
        Bigint.of_int amount <= C.credits t}) @ unique total ghost ->
      {t : C.token | let refine_ bank = bank in
        C.credits t = Bigint.sub (C.credits bank) (Bigint.of_int amount)}
        @ unique total ghost = fun amount bank ->
    let refine_ bank = bank in
    if amount <= 0 then refine_ bank
    else (
      let positive : {t : C.token | C.credits t > 0Z} = refine_ bank in
      let refine_ bank = C.tick positive in
      let rest = amount - 1 in
      let input : {t : C.token | 0 <= rest &&
        Bigint.of_int rest <= C.credits t} = refine_ bank in
      let refine_ result = charge rest input in
      refine_ result)
  [@@decreases amount]

  let link : (x : M.elem) @ immutable -> (y : M.elem) @ immutable ->
      (state : {s : resource | M.is_root (heap s) x &&
        M.is_root (heap s) y && M.rank (heap s) x + 1 >= 0 &&
        balance s >= 7Z}) @ unique read_write total ->
      {r : result | let refine_ state = state in
        r.#value === M.winner (heap state) x y &&
        heap r.#state === M.linked (heap state) x y &&
        balance r.#state = Bigint.sub (balance state) 7Z} @ unique =
      fun x y state ->
    let refine_ state = state in
    let before = ghost_ (heap (borrow_ state)) in
    ghost_ (heap_def (borrow_ state); balance_def (borrow_ state);
      M.is_root_def before x; M.is_root_def before y;
      M.rank_def before x; M.rank_def before y;
      M.winner_def before x y; M.linked_def before x y);
    let memory = state.memory in
    let seven = 7 in
    let input : {t : C.token | 0 <= seven &&
      Bigint.of_int seven <= C.credits t} = refine_ state.bank in
    let refine_ bank = charge seven input in
    let refine_ same = Pref.equal x y in
    if same then (
      let state = {memory; bank} in
      ghost_ (heap_def (borrow_ state); balance_def (borrow_ state));
      let result = #{ value = x; state } in refine_ result)
    else (
      let readable : {t : P.token | H.mem (P.own t) x &&
        H.mem (P.own t) y} = refine_ memory in
      let refine_ left = P.read x (borrow_ readable) in
      let refine_ right = P.read y (borrow_ readable) in
      match left, right with
      | M.Root rx, M.Root ry ->
        if rx < ry then (
          let refine_ memory = P.write x (M.Link (rx, y)) readable in
          let state = {memory; bank} in
          ghost_ (heap_def (borrow_ state); balance_def (borrow_ state));
          let result = #{value = y; state} in refine_ result)
        else if ry < rx then (
          let refine_ memory = P.write y (M.Link (ry, x)) readable in
          let state = {memory; bank} in
          ghost_ (heap_def (borrow_ state); balance_def (borrow_ state));
          let result = #{value = x; state} in refine_ result)
        else (
          let refine_ memory = P.write y (M.Link (ry, x)) readable in
          let writable : {t : P.token | H.mem (P.own t) x} = refine_ memory in
          let refine_ memory = P.write x (M.Root (rx + 1)) writable in
          let state = {memory; bank} in
          ghost_ (heap_def (borrow_ state); balance_def (borrow_ state));
          let result = #{value = x; state} in refine_ result)
      | _ -> assert false)

  let empty () =
    let refine_ memory = P.empty () in
    let refine_ bank = C.empty () in
    { memory; bank }

  let allocate :
      (state : {s : resource | balance s >= 3Z})
        @ unique read_write total ->
      {r : result | let refine_ state = state in
        not (H.mem (heap state) r.#value) &&
        heap r.#state === H.put (heap state) r.#value (M.Root 0) &&
        balance r.#state = Bigint.sub (balance state) 3Z} @ unique =
      fun state ->
    let refine_ state = state in
    ghost_ (heap_def (borrow_ state); balance_def (borrow_ state));
    let memory = state.memory in let bank = state.bank in
    let first : {t : C.token | C.credits t > 0Z} = refine_ bank in
    let refine_ bank = C.tick first in
    let second : {t : C.token | C.credits t > 0Z} = refine_ bank in
    let refine_ bank = C.tick second in
    let value = M.Root 0 in
    let third : {t : C.token | C.credits t > 0Z} = refine_ bank in
    let refine_ bank = C.tick third in
    let refine_ allocated = P.alloc value memory in
    let state = { memory = allocated.P.state; bank } in
    ghost_ (heap_def (borrow_ state); balance_def (borrow_ state));
    let result = #{ value = allocated.P.value; state } in
    refine_ result

  let rec find : (depth : Bigint.t) @ ghost ->
      (path : M.path) @ immutable total ghost ->
      (x : M.elem) @ immutable ->
      (state : {s : resource | M.valid (heap s) path &&
        x === M.head path && depth = M.depth path &&
        balance s >= cost depth}) @ unique read_write total ->
      {r : result | let refine_ state = state in
        r.#value === M.root path &&
        heap r.#state === M.compressed (heap state) path &&
        balance r.#state = Bigint.sub (balance state) (cost depth)}
        @ unique = fun depth path x state ->
    let refine_ state = state in
    let before = ghost_ (heap (borrow_ state)) in
    ghost_ (heap_def (borrow_ state); balance_def (borrow_ state);
      cost_def depth; M.depth_nonnegative path; M.valid_def before path);
    let memory = state.memory in
    let bank = state.bank in
    let first : {t : C.token | C.credits t > 0Z} = refine_ bank in
    let refine_ bank = C.tick first in
    let second : {t : C.token | C.credits t > 0Z} = refine_ bank in
    let refine_ bank = C.tick second in
    let readable : {t : P.token | H.mem (P.own t) x} = refine_ memory in
    let refine_ observed = P.read x (borrow_ readable) in
    ghost_ (M.observe before path x observed; M.rank_def before x);
    match observed with
    | M.Root _ ->
      let state = { memory = readable; bank } in
      ghost_ (heap_def (borrow_ state); balance_def (borrow_ state));
      let result = #{ value = x; state } in
      refine_ result
    | M.Link (rank, y) ->
      let tail = ghost_ (M.tail path) in
      let next = ghost_ (Bigint.sub depth 1Z) in
      ghost_ (cost_def next);
      let state = { memory = readable; bank } in
      ghost_ (heap_def (borrow_ state); balance_def (borrow_ state));
      let input : {s : resource | M.valid (heap s) tail &&
        y === M.head tail && next = M.depth tail &&
        balance s >= cost next} = refine_ state in
      let refine_ found = find next tail y input in
      let #{ value = root; state } = found in
      ghost_ (heap_def (borrow_ state); balance_def (borrow_ state);
        M.compressed_mem before tail x);
      let memory = state.memory in
      let bank = state.bank in
      let third : {t : C.token | C.credits t > 0Z} = refine_ bank in
      let refine_ bank = C.tick third in
      let replacement = M.Link (rank, root) in
      let fourth : {t : C.token | C.credits t > 0Z} = refine_ bank in
      let refine_ bank = C.tick fourth in
      let writable : {t : P.token | H.mem (P.own t) x} = refine_ memory in
      let refine_ memory = P.write x replacement writable in
      let state = { memory; bank } in
      ghost_ (heap_def (borrow_ state); balance_def (borrow_ state));
      let result = #{ value = root; state } in
      refine_ result
  [@@decreases let depth : Bigint.t = depth in depth]
end
