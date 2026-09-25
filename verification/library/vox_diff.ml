open Vox_diff_spec
module M = Vox_diff_metric

type entry = { x : int; old_tail : int list;
  new_tail : int list; rev : script }

let[@def] valid (old : int list) (fresh : int list) (k : Bigint.t)
    (depth : Bigint.t) (e : entry) =
  ghost_ (restore_old e.rev e.old_tail === old
    && restore_new e.rev e.new_tail === fresh
    && suffix old e.old_tail && suffix fresh e.new_tail
    && size old <= 1000000Z && size fresh <= 1000000Z
    && 0 <= e.x && e.x <= 1000000
    && Bigint.of_int e.x = Bigint.sub (size old) (size e.old_tail)
    && Bigint.sub (size e.old_tail) (size e.new_tail) =
       Bigint.sub (Bigint.sub (size old) (size fresh)) k
    && cost e.rev = depth)

let[@def] optional_valid old fresh k depth candidate =
  match candidate with None -> true | Some e -> valid old fresh k depth e

let[@def] settled (e : entry) =
  match e.old_tail, e.new_tail with
  | a :: _, b :: _ -> a <> b
  | _ -> true

let[@def] optional_settled candidate =
  match candidate with None -> true | Some e -> settled e

let[@def] good candidate budget =
  match candidate with None -> false | Some e -> M.metric e.old_tail e.new_tail
    <= budget

let[@def] unfinished candidate =
  match candidate with Some {old_tail = []; new_tail = []; _} -> false | _ ->
    true

let[@def] rec frontier_valid old fresh k depth frontier =
  ghost_ (match frontier with
  | [] -> true
  | candidate :: rest -> optional_valid old fresh k depth candidate
    && optional_settled candidate
    && frontier_valid old fresh (Bigint.add k 2Z) depth rest)

let[@def] rec frontier_good frontier budget =
  match frontier with [] -> false | e :: rest -> good e budget || frontier_good
    rest budget

let[@def] rec frontier_unfinished frontier =
  match frontier with [] -> true | e :: rest -> unfinished e &&
    frontier_unfinished rest

let (dominance @ total) : (old : int list) -> (fresh : int list) ->
    (k : Bigint.t) -> (depth : Bigint.t) -> (a : entry) -> (b : entry) ->
    {u : unit | if valid old fresh k depth a && valid old fresh k depth b
      && b.x <= a.x then M.metric a.old_tail a.new_tail <= M.metric b.old_tail
        b.new_tail
      else true} = fun old fresh k depth a b ->
  valid_def old fresh k depth a;
  valid_def old fresh k depth b;
  suffix_compare old a.old_tail b.old_tail;
  suffix_compare fresh a.new_tail b.new_tail;
  M.crop b.old_tail b.new_tail a.old_tail a.new_tail;
  let u = () in refine_ u

let rec (snake @ total) : (remaining : Bigint.t) @ ghost ->
    (old : int list) @ ghost -> (fresh : int list) @ ghost ->
    (k : Bigint.t) @ ghost -> (depth : Bigint.t) @ ghost ->
    (candidate : {e : entry | valid old fresh k depth e && remaining = size
      e.old_tail}) ->
    {r : entry | let refine_ e = candidate in valid old fresh k depth r
      && settled r && M.metric r.old_tail r.new_tail = M.metric e.old_tail
        e.new_tail} =
    fun remaining old fresh k depth candidate ->
  let refine_ e = candidate in
  ghost_ (valid_def old fresh k depth e);
  ghost_ (size_def e.old_tail);
  ghost_ (size_def e.new_tail);
  ghost_ (size_nonnegative e.old_tail);
  ghost_ (settled_def e);
  match e.old_tail, e.new_tail with
  | a :: ats, b :: bts when a = b ->
    let rev = Keep a :: e.rev in
    let next = {x = e.x + 1; old_tail = ats; new_tail = bts; rev} in
    ghost_ (size_nonnegative ats);
    ghost_ (restore_old_def rev ats);
    ghost_ (restore_new_def rev bts);
    ghost_ (suffix_step old a ats);
    ghost_ (suffix_step fresh b bts);
    ghost_ (cost_def rev);
    ghost_ (valid_def old fresh k depth next);
    ghost_ (M.equation e.old_tail e.new_tail);
    let smaller = ghost_ (Bigint.sub remaining 1Z) in
    let next : {e : entry | valid old fresh k depth e && smaller = size
      e.old_tail} = refine_ next in
    let refine_ result = snake smaller old fresh k depth next in
    refine_ result
  | _ -> refine_ e
[@@decreases remaining]

let[@def] deleted candidate result = ghost_ (
  match candidate with
  | None -> result === None
  | Some e -> match e.old_tail with
    | [] -> result === None
    | _ :: tail -> match result with
      | None -> false
      | Some r -> r.old_tail === tail && r.new_tail === e.new_tail)

let[@def] inserted candidate result = ghost_ (
  match candidate with
  | None -> result === None
  | Some e -> match e.new_tail with
    | [] -> result === None
    | _ :: tail -> match result with
      | None -> false
      | Some r -> r.new_tail === tail && r.old_tail === e.old_tail)

let (step_delete @ total) : (old : int list) @ ghost -> (fresh : int list) @
  ghost ->
    (k : Bigint.t) @ ghost -> (depth : Bigint.t) @ ghost ->
    (candidate : {c : entry option | optional_valid old fresh k depth c}) ->
    {r : entry option | let refine_ c = candidate in deleted c r
      && optional_valid old fresh (Bigint.add k 1Z) (Bigint.add depth 1Z) r} =
    fun old fresh k depth candidate ->
  let refine_ candidate = candidate in
  ghost_ (optional_valid_def old fresh k depth candidate);
  let result = match candidate with
    | None -> None
    | Some e ->
      ghost_ (valid_def old fresh k depth e);
      ghost_ (size_def e.old_tail);
      match e.old_tail with
      | [] -> None
      | a :: ats ->
        let rev = Delete a :: e.rev in
        let next = {x = e.x + 1; old_tail = ats; new_tail = e.new_tail; rev} in
        ghost_ (size_nonnegative ats);
        ghost_ (restore_old_def rev ats);
        ghost_ (restore_new_def rev e.new_tail);
        ghost_ (suffix_step old a ats);
        ghost_ (cost_def rev);
        ghost_ (valid_def old fresh (Bigint.add k 1Z) (Bigint.add depth 1Z)
          next);
        Some next
  in
  ghost_ (deleted_def candidate result);
  ghost_ (optional_valid_def old fresh (Bigint.add k 1Z) (Bigint.add depth 1Z)
    result);
  refine_ result

let (step_insert @ total) : (old : int list) @ ghost -> (fresh : int list) @
  ghost ->
    (k : Bigint.t) @ ghost -> (depth : Bigint.t) @ ghost ->
    (candidate : {c : entry option | optional_valid old fresh k depth c}) ->
    {r : entry option | let refine_ c = candidate in inserted c r
      && optional_valid old fresh (Bigint.sub k 1Z) (Bigint.add depth 1Z) r} =
    fun old fresh k depth candidate ->
  let refine_ candidate = candidate in
  ghost_ (optional_valid_def old fresh k depth candidate);
  let result = match candidate with
    | None -> None
    | Some e ->
      ghost_ (valid_def old fresh k depth e);
      ghost_ (size_def e.new_tail);
      match e.new_tail with
      | [] -> None
      | b :: bts ->
        let rev = Insert b :: e.rev in
        let next = {x = e.x; old_tail = e.old_tail; new_tail = bts; rev} in
        ghost_ (restore_old_def rev e.old_tail);
        ghost_ (restore_new_def rev bts);
        ghost_ (suffix_step fresh b bts);
        ghost_ (cost_def rev);
        ghost_ (valid_def old fresh (Bigint.sub k 1Z) (Bigint.add depth 1Z)
          next);
        Some next
  in
  ghost_ (inserted_def candidate result);
  ghost_ (optional_valid_def old fresh (Bigint.sub k 1Z) (Bigint.add depth 1Z)
    result);
  refine_ result

let (branches @ total) (candidate : entry option) (deletion : entry option)
    (insertion : entry option) (budget : Bigint.t) :
    {u : unit | if optional_settled candidate && unfinished candidate
      && good candidate budget && deleted candidate deletion && inserted
        candidate insertion
      then good deletion (Bigint.sub budget 1Z) || good insertion (Bigint.sub
        budget 1Z)
      else true} =
  optional_settled_def candidate;
  unfinished_def candidate;
  good_def candidate budget;
  deleted_def candidate deletion;
  inserted_def candidate insertion;
  good_def deletion (Bigint.sub budget 1Z);
  good_def insertion (Bigint.sub budget 1Z);
  (match candidate with
   | None -> ()
   | Some e ->
     settled_def e;
     M.equation e.old_tail e.new_tail;
     size_def e.old_tail;
     size_def e.new_tail;
     match e.old_tail, e.new_tail with
     | _ :: ats, _ :: bts -> M.min_def (M.metric ats e.new_tail) (M.metric
       e.old_tail bts)
     | [], _ :: bts -> M.equation [] bts
     | _ :: ats, [] -> M.equation ats []
     | [], [] -> ());
  let u = () in refine_ u

let (choose @ total) : (old : int list) @ ghost -> (fresh : int list) @ ghost
  ->
    (k : Bigint.t) @ ghost -> (depth : Bigint.t) @ ghost -> (budget : Bigint.t)
      @ ghost ->
    (deletion : {c : entry option | optional_valid old fresh k depth c}) ->
    (insertion : {c : entry option | optional_valid old fresh k depth c}) ->
    {r : entry option | let refine_ a = deletion in let refine_ b = insertion
      in
      optional_valid old fresh k depth r && optional_settled r
      && (if good a budget || good b budget then good r budget else true)} =
    fun old fresh k depth budget deletion insertion ->
  let refine_ deletion = deletion in
  let refine_ insertion = insertion in
  ghost_ (optional_valid_def old fresh k depth deletion);
  ghost_ (optional_valid_def old fresh k depth insertion);
  ghost_ (good_def deletion budget);
  ghost_ (good_def insertion budget);
  let chosen = match deletion, insertion with
    | None, other | other, None -> other
    | Some a, Some b ->
      ghost_ (dominance old fresh k depth a b);
      ghost_ (dominance old fresh k depth b a);
      if a.x > b.x then deletion else insertion
  in
  ghost_ (optional_valid_def old fresh k depth chosen);
  ghost_ (good_def chosen budget);
  let result = match chosen with
    | None -> None
    | Some e ->
      let remaining = ghost_ (size e.old_tail) in
      let candidate : {e : entry | valid old fresh k depth e && remaining =
        size e.old_tail} = refine_ e in
      let refine_ e = snake remaining old fresh k depth candidate in Some e
  in
  ghost_ (optional_valid_def old fresh k depth result);
  ghost_ (good_def result budget);
  ghost_ (optional_settled_def result);
  refine_ result

let[@def] rec width (frontier : entry option list) =
  match frontier with [] -> 0Z | _ :: rest -> Bigint.add 1Z (width rest)

let rec (advance @ total) : (old : int list) @ ghost -> (fresh : int list) @
  ghost ->
    (k : Bigint.t) @ ghost -> (depth : Bigint.t) @ ghost -> (budget : Bigint.t)
      @ ghost ->
    (left : {c : entry option | optional_valid old fresh k (Bigint.add depth
      1Z) c}) ->
    (frontier : {f : entry option list |
      frontier_valid old fresh (Bigint.add k 1Z) depth f && frontier_unfinished
        f}) ->
    {r : entry option list | let refine_ c = left in let refine_ f = frontier
      in
      frontier_valid old fresh k (Bigint.add depth 1Z) r
      && width r = Bigint.add 1Z (width f)
      && (if good c (Bigint.sub budget 1Z) || frontier_good f budget
          then frontier_good r (Bigint.sub budget 1Z) else true)} =
    fun old fresh k depth budget left frontier ->
  let refine_ frontier = frontier in
  let lower = ghost_ (Bigint.sub budget 1Z) in
  let next_depth = ghost_ (Bigint.add depth 1Z) in
  let right_k = ghost_ (Bigint.add k 1Z) in
  ghost_ (frontier_valid_def old fresh right_k depth frontier);
  ghost_ (frontier_good_def frontier budget);
  ghost_ (frontier_unfinished_def frontier);
  ghost_ (width_def frontier);
  match frontier with
  | [] ->
    ghost_ (optional_valid_def old fresh k next_depth None);
    ghost_ (good_def None lower);
    let absent : {c : entry option | optional_valid old fresh k next_depth c} =
      refine_ None in
    let refine_ last = choose old fresh k next_depth lower left absent in
    let result = [last] in
    ghost_ (width_def result);
    ghost_ (width_def []);
    ghost_ (frontier_valid_def old fresh (Bigint.add k 2Z) next_depth []);
    ghost_ (frontier_valid_def old fresh k next_depth result);
    ghost_ (frontier_good_def result lower);
    refine_ result
  | right :: rest ->
    let candidate : {c : entry option | optional_valid old fresh right_k depth
      c} = refine_ right in
    let refine_ deletion = step_delete old fresh right_k depth candidate in
    let refine_ insertion = step_insert old fresh right_k depth candidate in
    ghost_ (branches right deletion insertion budget);
    let insertion : {c : entry option | optional_valid old fresh k next_depth
      c} = refine_ insertion in
    let refine_ next = choose old fresh k next_depth lower left insertion in
    let next_k = ghost_ (Bigint.add k 2Z) in
    let deletion : {c : entry option | optional_valid old fresh next_k
      next_depth c} = refine_ deletion in
    let rest : {f : entry option list |
      frontier_valid old fresh (Bigint.add next_k 1Z) depth f &&
        frontier_unfinished f} = refine_ rest in
    let refine_ tail = advance old fresh next_k depth budget deletion rest in
    let result = next :: tail in
    ghost_ (width_def result);
    ghost_ (frontier_valid_def old fresh k next_depth result);
    ghost_ (frontier_good_def result lower);
    refine_ result

let rec (finished @ total) : (old : int list) @ ghost -> (fresh : int list) @
  ghost ->
    (k : Bigint.t) @ ghost -> (depth : Bigint.t) @ ghost ->
    (frontier : {f : entry option list | frontier_valid old fresh k depth f})
      ->
    {s : script option | let refine_ f = frontier in match s with
      | None -> frontier_unfinished f
      | Some s -> source s === old && target s === fresh && cost s = depth} =
    fun old fresh k depth frontier ->
  let refine_ frontier = frontier in
  ghost_ (frontier_valid_def old fresh k depth frontier);
  ghost_ (frontier_unfinished_def frontier);
  match frontier with
  | [] -> refine_ None
  | candidate :: rest ->
    ghost_ (optional_valid_def old fresh k depth candidate);
    ghost_ (unfinished_def candidate);
    match candidate with
    | Some ({old_tail = []; new_tail = []; _} as e) ->
      ghost_ (valid_def old fresh k depth e);
      ghost_ (source_def []);
      ghost_ (target_def []);
      ghost_ (cost_def []);
      let refine_ result = reverse_into e.rev [] in
      let result = Some result in refine_ result
    | _ ->
      let next_k = ghost_ (Bigint.add k 2Z) in
      let rest : {f : entry option list | frontier_valid old fresh next_k depth
        f} = refine_ rest in
      let refine_ result = finished old fresh next_k depth rest in
      refine_ result

let rec (frontier_budget @ total) : (old : int list) -> (fresh : int list) ->
    (k : Bigint.t) -> (depth : Bigint.t) -> (frontier : entry option list) ->
      (budget : Bigint.t) ->
    {u : unit | if frontier_valid old fresh k depth frontier && frontier_good
      frontier budget
      then 0Z <= budget && (if frontier_unfinished frontier then 0Z < budget
        else true)
      else true} = fun old fresh k depth frontier budget ->
  frontier_valid_def old fresh k depth frontier;
  frontier_good_def frontier budget;
  frontier_unfinished_def frontier;
  (match frontier with
   | [] -> ()
   | candidate :: rest ->
     optional_settled_def candidate;
     good_def candidate budget;
     unfinished_def candidate;
     (match candidate with None -> () | Some e ->
       settled_def e;
       M.properties e.old_tail e.new_tail);
     frontier_budget old fresh (Bigint.add k 2Z) depth rest budget);
  let u = () in refine_ u

let rec (search @ total) : (old : int list) @ ghost -> (fresh : int list) @
  ghost ->
    (k : Bigint.t) @ ghost -> (depth : Bigint.t) @ ghost -> (budget : Bigint.t)
      @ ghost ->
    (fuel : int) ->
    (frontier : {f : entry option list | frontier_valid old fresh k depth f
      && frontier_good f budget && budget <= Bigint.of_int fuel
      && 0Z <= depth && width f = Bigint.add depth 1Z
      && Bigint.add depth (Bigint.of_int fuel) <= Bigint.add (size old) (size
        fresh)}) ->
    {s : script | source s === old && target s === fresh && cost s <=
      Bigint.add depth budget} =
    fun old fresh k depth budget fuel frontier ->
  let refine_ f = frontier in
  ghost_ (frontier_budget old fresh k depth f budget);
  let current : {f : entry option list | frontier_valid old fresh k depth f} =
    refine_ f in
  let refine_ result = finished old fresh k depth current in
  match result with
  | Some s -> refine_ s
  | None ->
    if fuel <= 0 then unreachable_ () else (
      let next_k = ghost_ (Bigint.sub k 1Z) in
      let next_depth = ghost_ (Bigint.add depth 1Z) in
      let lower = ghost_ (Bigint.sub budget 1Z) in
      let remaining = fuel - 1 in
      ghost_ (optional_valid_def old fresh next_k next_depth None);
      let left : {c : entry option | optional_valid old fresh next_k next_depth
        c} = refine_ None in
      let current : {f : entry option list |
        frontier_valid old fresh (Bigint.add next_k 1Z) depth f &&
          frontier_unfinished f} = refine_ f in
      let refine_ next = advance old fresh next_k depth budget left current in
      let next : {f : entry option list | frontier_valid old fresh next_k
        next_depth f
        && frontier_good f lower && lower <= Bigint.of_int remaining
        && 0Z <= next_depth && width f = Bigint.add next_depth 1Z
        && Bigint.add next_depth (Bigint.of_int remaining) <= Bigint.add (size
          old) (size fresh)} = refine_ next in
      let refine_ result = search old fresh next_k next_depth lower remaining
        next in
      refine_ result)
[@@decreases fuel]

let rec (bounded_length @ total) : (bound : int) -> (values : int list) ->
    {r : int option | match r with
      | None -> Bigint.of_int bound < size values
      | Some n -> 0 <= n && n <= bound && Bigint.of_int n = size values} = fun
        bound values ->
  ghost_ (size_def values);
  ghost_ (size_nonnegative values);
  match values with
  | [] -> if bound < 0 then refine_ None else refine_ (Some 0)
  | _ :: rest ->
    ghost_ (size_nonnegative rest);
    if bound <= 0 then refine_ None else
    let refine_ result = bounded_length (bound - 1) rest in
    match result with
    | None -> refine_ None
    | Some n -> refine_ (Some (n + 1))

type error = Input_too_large [@@inductive]

let (diff @ total) : (old : int list) -> (fresh : int list) ->
    {r : (script, error) result | match r with
      | Error Input_too_large -> 1000000Z < size old || 1000000Z < size fresh
      | Ok s -> size old <= 1000000Z && size fresh <= 1000000Z
        && source s === old && target s === fresh
        && apply old s === Some fresh && cost s = M.metric old fresh
        && 0Z <= cost s && script_size s <= Bigint.add (size old) (size fresh)}
          = fun old fresh ->
  let refine_ n = bounded_length 1000000 old in
  let refine_ m = bounded_length 1000000 fresh in
  match n, m with
  | Some n, Some m ->
    let initial = {x = 0; old_tail = old; new_tail = fresh; rev = []} in
    ghost_ (restore_old_def [] old);
    ghost_ (restore_new_def [] fresh);
    ghost_ (suffix_def old old);
    ghost_ (suffix_def fresh fresh);
    ghost_ (cost_def []);
    ghost_ (valid_def old fresh 0Z 0Z initial);
    let remaining = ghost_ (size old) in
    let candidate : {e : entry | valid old fresh 0Z 0Z e && remaining = size
      e.old_tail} = refine_ initial in
    let refine_ first = snake remaining (ghost_ old) (ghost_ fresh) (ghost_ 0Z)
      (ghost_ 0Z) candidate in
    let frontier = [Some first] in
    let budget = ghost_ (M.metric old fresh) in
    let fuel = n + m in
    ghost_ (optional_valid_def old fresh 0Z 0Z (Some first));
    ghost_ (optional_settled_def (Some first));
    ghost_ (frontier_valid_def old fresh 2Z 0Z []);
    ghost_ (frontier_valid_def old fresh 0Z 0Z frontier);
    ghost_ (good_def (Some first) budget);
    ghost_ (frontier_good_def frontier budget);
    ghost_ (M.properties old fresh);
    ghost_ (width_def frontier);
    ghost_ (width_def []);
    let frontier : {f : entry option list | frontier_valid old fresh 0Z 0Z f
      && frontier_good f budget && budget <= Bigint.of_int fuel
      && width f = 1Z
      && Bigint.of_int fuel <= Bigint.add (size old) (size fresh)} = refine_
        frontier in
    let refine_ script = search (ghost_ old) (ghost_ fresh) (ghost_ 0Z) (ghost_
      0Z) budget fuel frontier in
    ghost_ (M.lower_bound script);
    ghost_ (apply_source script);
    ghost_ (script_bounds script);
    let result : (script, error) result = Ok script in refine_ result
  | _ ->
    let result : (script, error) result = Error Input_too_large in refine_
      result

let (optimal_at @ total) : (old : int list) -> (fresh : int list) ->
    (computed : {s : script | cost s = M.metric old fresh}) -> (other : script)
      ->
    {u : unit | let refine_ s = computed in
      if apply old other === Some fresh then cost s <= cost other else true} =
    fun old fresh computed other ->
  let refine_ computed = computed in
  apply_characterization old other;
  M.lower_bound other;
  let u = () in refine_ u
