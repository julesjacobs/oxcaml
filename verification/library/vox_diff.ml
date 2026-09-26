open Vox_diff_spec

module Proof = struct
  let (apply_source @ total) (script : script) :
      {u : unit | apply (source script) script === Some (target script)} =
    apply_characterization (source script) script

  let rec (invert_correct @ total) (script : script) :
      {u : unit | source (invert script) === target script
        && target (invert script) === source script
        && cost (invert script) = cost script} =
    invert_def script;
    source_def script;
    target_def script;
    cost_def script;
    source_def (invert script);
    target_def (invert script);
    cost_def (invert script);
    (match script with [] -> () | _ :: rest -> invert_correct rest);
    ()

  let[@def] rec restore_old (rev : script) (suffix : int list) =
    match rev with
    | [] -> suffix
    | Keep x :: rest | Delete x :: rest -> restore_old rest (x :: suffix)
    | Insert _ :: rest -> restore_old rest suffix

  let[@def] rec restore_new (rev : script) (suffix : int list) =
    match rev with
    | [] -> suffix
    | Keep x :: rest | Insert x :: rest -> restore_new rest (x :: suffix)
    | Delete _ :: rest -> restore_new rest suffix

  let rec (reverse_into @ total) : (rev : script) -> (suffix : script) ->
      {s : script | source s === restore_old rev (source suffix)
        && target s === restore_new rev (target suffix)
        && cost s = Bigint.add (cost rev) (cost suffix)} = fun rev suffix ->
    ghost_ (cost_def rev);
    ghost_ (restore_old_def rev (source suffix));
    ghost_ (restore_new_def rev (target suffix));
    match rev with
    | [] -> suffix
    | op :: rest ->
      let next = op :: suffix in
      ghost_ (cost_def next);
      ghost_ (source_def next);
      ghost_ (target_def next);
      reverse_into rest next

  let rec (size_nonnegative @ total) (values : int list) :
      {u : unit | 0Z <= size values} =
    size_def values;
    (match values with [] -> () | _ :: rest -> size_nonnegative rest);
    ()

  let[@def] rec suffix (whole : int list) (tail : int list) =
    ghost_ (whole === tail || match whole with
      | [] -> false | _ :: rest -> suffix rest tail)

  let rec (suffix_bounds @ total) : (whole : int list) -> (tail : int list) ->
      {u : unit | if suffix whole tail then
        size tail <= size whole && (if size tail = size whole then tail ===
          whole
          else true)
        else true} = fun whole tail ->
    suffix_def whole tail;
    size_def whole;
    (match whole with
     | [] -> ()
     | _ :: rest -> suffix_bounds rest tail);
    ()

  let rec (suffix_step @ total) : (whole : int list) -> (head : int) ->
      (tail : int list) ->
      {u : unit | if suffix whole (head :: tail) then suffix whole tail else
        true} =
      fun whole head tail ->
    suffix_def whole (head :: tail);
    suffix_def whole tail;
    suffix_def tail tail;
    match whole with
    | [] -> ()
    | _ :: rest ->
      suffix_step rest head tail;
      ()

  let rec (suffix_compare @ total) : (whole : int list) ->
      (shorter : int list) -> (longer : int list) ->
      {u : unit | if suffix whole shorter && suffix whole longer
        && size shorter <= size longer then suffix longer shorter else true} =
      fun whole shorter longer ->
    suffix_def whole shorter;
    suffix_def whole longer;
    suffix_def longer shorter;
    suffix_bounds whole shorter;
    suffix_bounds whole longer;
    (match whole with
     | [] -> ()
     | _ :: rest -> suffix_compare rest shorter longer);
    ()

  let (size_zero @ total) (values : int list) :
      {u : unit | if size values = 0Z then values === [] else true} =
    size_def values;
    (match values with [] -> () | _ :: rest -> size_nonnegative rest);
    ()

  let (inverse_patch @ total) (script : script) :
      {u : unit | apply (target script) (invert script) === Some (source
        script)}
        =
    invert_correct script;
    apply_source (invert script);
    ()

  let rec (script_bounds @ total) (script : script) :
      {u : unit | 0Z <= cost script && cost script <= script_size script
        && script_size script <= Bigint.add (size (source script)) (size
          (target
          script))} =
    source_def script;
    target_def script;
    cost_def script;
    script_size_def script;
    size_def (source script);
    size_def (target script);
    (match script with [] -> () | _ :: rest -> script_bounds rest);
    ()
end

module M = struct
  open Proof

  let rec (neighbors_aux @ total) : (fuel : Bigint.t) ->
      (old : int list) -> (fresh : int list) -> (a : int) -> (b : int) ->
      {u : unit | if fuel = Bigint.add (size old) (size fresh) then
        minimum_cost old fresh <= Bigint.add 1Z (minimum_cost (a :: old) fresh)
        && minimum_cost (a :: old) fresh <= Bigint.add 1Z (minimum_cost old
          fresh)
        && minimum_cost old fresh <= Bigint.add 1Z (minimum_cost old (b ::
          fresh))
        && minimum_cost old (b :: fresh) <= Bigint.add 1Z (minimum_cost old
          fresh)
        else true} = fun fuel old fresh a b ->
    if fuel <> Bigint.add (size old) (size fresh) then
      ()
    else (
      size_def old;
      size_def fresh;
      size_nonnegative old;
      size_nonnegative fresh;
      minimum_cost_equation old fresh;
      minimum_cost_equation (a :: old) fresh;
      minimum_cost_equation old (b :: fresh);
      size_def (a :: old);
      size_def (b :: fresh);
      (match fresh with
       | [] -> ()
       | head :: tail ->
         size_nonnegative tail;
         neighbors_aux (Bigint.sub fuel 1Z) old tail a head);
      (match old with
       | [] -> ()
       | head :: tail ->
         size_nonnegative tail;
         neighbors_aux (Bigint.sub fuel 1Z) tail fresh head b);
      ())
  [@@decreases fuel]

  let (neighbors @ total) (old : int list) (fresh : int list) (a : int) (b :
    int)
    :
      {u : unit |
        minimum_cost old fresh <= Bigint.add 1Z (minimum_cost (a :: old) fresh)
        && minimum_cost (a :: old) fresh <= Bigint.add 1Z (minimum_cost old
          fresh)
        && minimum_cost old fresh <= Bigint.add 1Z (minimum_cost old (b ::
          fresh))
        && minimum_cost old (b :: fresh) <= Bigint.add 1Z (minimum_cost old
          fresh)} =
    neighbors_aux (Bigint.add (size old) (size fresh)) old fresh a b;
    ()

  let (strip @ total) (old : int list) (fresh : int list) (a : int) (b : int) :
      {u : unit | minimum_cost old fresh <= minimum_cost (a :: old) (b ::
        fresh)} =
    minimum_cost_equation (a :: old) (b :: fresh);
    neighbors old fresh a b;
    ()

  let rec (lower_bound @ total) (script : script) :
      {u : unit | minimum_cost (source script) (target script) <= cost script}
        =
    source_def script;
    target_def script;
    cost_def script;
    match script with
    | [] ->
      minimum_cost_equation [] [];
      size_def [];
      ()
    | op :: rest ->
      lower_bound rest;
      (match op with
       | Keep a -> minimum_cost_equation (a :: source rest) (a :: target rest)
       | Delete a -> neighbors (source rest) (target rest) a a
       | Insert b -> neighbors (source rest) (target rest) b b);
      ()

  let rec (crop @ total) : (old : int list) -> (fresh : int list) ->
      (old_tail : int list) -> (new_tail : int list) ->
      {u : unit | if suffix old old_tail && suffix fresh new_tail
        && Bigint.sub (size old) (size old_tail) =
           Bigint.sub (size fresh) (size new_tail)
        then minimum_cost old_tail new_tail <= minimum_cost old fresh else
          true} =
      fun old fresh old_tail new_tail ->
    suffix_bounds old old_tail;
    suffix_bounds fresh new_tail;
    suffix_def old old_tail;
    suffix_def fresh new_tail;
    size_def old;
    size_def fresh;
    size_nonnegative old_tail;
    size_nonnegative new_tail;
    (match old, fresh with
     | a :: ats, b :: bts ->
       crop ats bts old_tail new_tail;
       strip ats bts a b
     | _ -> ());
    ()

  let rec (properties_aux @ total) : (fuel : Bigint.t) ->
      (old : int list) -> (fresh : int list) ->
      {u : unit | if fuel = Bigint.add (size old) (size fresh) then
        0Z <= minimum_cost old fresh && minimum_cost old fresh <= fuel
        && (if minimum_cost old fresh = 0Z then old === fresh else true)
        else true} = fun fuel old fresh ->
    if fuel <> Bigint.add (size old) (size fresh) then ()
    else (
      minimum_cost_equation old fresh;
      size_def old;
      size_def fresh;
      size_nonnegative old;
      size_nonnegative fresh;
      size_zero old;
      size_zero fresh;
      (match old, fresh with
       | a :: ats, b :: bts ->
         size_nonnegative ats;
         size_nonnegative bts;
         properties_aux (Bigint.sub fuel 2Z) ats bts;
         properties_aux (Bigint.sub fuel 1Z) ats fresh;
         properties_aux (Bigint.sub fuel 1Z) old bts;
       | _ -> ());
      ())
  [@@decreases fuel]

  let (properties @ total) (old : int list) (fresh : int list) :
      {u : unit | 0Z <= minimum_cost old fresh
        && minimum_cost old fresh <= Bigint.add (size old) (size fresh)
        && (if minimum_cost old fresh = 0Z then old === fresh else true)} =
    properties_aux (Bigint.add (size old) (size fresh)) old fresh;
    ()
end

open Proof

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
  match candidate with None -> false | Some e -> minimum_cost e.old_tail
    e.new_tail
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
      && b.x <= a.x then minimum_cost a.old_tail a.new_tail <= minimum_cost
        b.old_tail
        b.new_tail
      else true} = fun old fresh k depth a b ->
  valid_def old fresh k depth a;
  valid_def old fresh k depth b;
  suffix_compare old a.old_tail b.old_tail;
  suffix_compare fresh a.new_tail b.new_tail;
  M.crop b.old_tail b.new_tail a.old_tail a.new_tail;
  ()

module Entry_proof = struct
  let (initial_valid @ total) (old : int list) (fresh : int list)
      (e : entry) :
      {u : unit | if size old <= 1000000Z && size fresh <= 1000000Z
        && e.x = 0 && e.old_tail === old && e.new_tail === fresh && e.rev === []
        then valid old fresh 0Z 0Z e else true} =
    restore_old_def [] old;
    restore_new_def [] fresh;
    suffix_def old old;
    suffix_def fresh fresh;
    cost_def [];
    valid_def old fresh 0Z 0Z e;
    ()

  let (keep_valid @ total) (old : int list) (fresh : int list)
      (k : Bigint.t) (depth : Bigint.t) (e : entry) (next : entry)
      (a : int) (ats : int list) (bts : int list) :
      {u : unit | if valid old fresh k depth e && e.old_tail === a :: ats
        && e.new_tail === a :: bts
        && next.x = e.x + 1 && next.old_tail === ats
        && next.new_tail === bts && next.rev === Keep a :: e.rev
        then valid old fresh k depth next && 0Z <= size next.old_tail
          && minimum_cost next.old_tail next.new_tail =
            minimum_cost e.old_tail e.new_tail
        else true} =
    valid_def old fresh k depth e;
    size_def e.old_tail;
    size_def e.new_tail;
    size_nonnegative ats;
    restore_old_def next.rev ats;
    restore_new_def next.rev bts;
    suffix_step old a ats;
    suffix_step fresh a bts;
    cost_def next.rev;
    valid_def old fresh (k) depth next;
    minimum_cost_equation e.old_tail e.new_tail;
    ()

  let (delete_valid @ total) (old : int list) (fresh : int list)
      (k : Bigint.t) (depth : Bigint.t) (e : entry) (next : entry)
      (a : int) (ats : int list) :
      {u : unit | if valid old fresh k depth e && e.old_tail === a :: ats
        && next.x = e.x + 1 && next.old_tail === ats
        && next.new_tail === e.new_tail && next.rev === Delete a :: e.rev
        then valid old fresh (Bigint.add k 1Z) (Bigint.add depth 1Z) next
        else true} =
    valid_def old fresh k depth e;
    size_def e.old_tail;
    size_def e.new_tail;
    size_nonnegative ats;
    restore_old_def next.rev ats;
    restore_new_def next.rev e.new_tail;
    suffix_step old a ats;
    cost_def next.rev;
    valid_def old fresh (Bigint.add k 1Z) (Bigint.add depth 1Z) next;
    ()

  let (insert_valid @ total) (old : int list) (fresh : int list)
      (k : Bigint.t) (depth : Bigint.t) (e : entry) (next : entry)
      (a : int) (bts : int list) :
      {u : unit | if valid old fresh k depth e && e.new_tail === a :: bts
        && next.x = e.x && next.old_tail === e.old_tail
        && next.new_tail === bts && next.rev === Insert a :: e.rev
        then valid old fresh (Bigint.sub k 1Z) (Bigint.add depth 1Z) next
        else true} =
    valid_def old fresh k depth e;
    size_def e.old_tail;
    size_def e.new_tail;
    restore_old_def next.rev e.old_tail;
    restore_new_def next.rev bts;
    suffix_step fresh a bts;
    cost_def next.rev;
    valid_def old fresh (Bigint.sub k 1Z) (Bigint.add depth 1Z) next;
    ()
end

let rec (snake @ total) : (remaining : Bigint.t) @ ghost ->
    (old : int list) @ ghost -> (fresh : int list) @ ghost ->
    (k : Bigint.t) @ ghost -> (depth : Bigint.t) @ ghost ->
    (candidate : {e : entry | valid old fresh k depth e && remaining = size
      e.old_tail}) ->
    {r : entry | let e = candidate in valid old fresh k depth r
      && settled r && minimum_cost r.old_tail r.new_tail = minimum_cost
        e.old_tail
        e.new_tail} =
    fun remaining old fresh k depth candidate ->
  let e = candidate in
  ghost_ (valid_def old fresh k depth e);
  ghost_ (size_def e.old_tail);
  ghost_ (size_def e.new_tail);
  ghost_ (size_nonnegative e.old_tail);
  ghost_ (settled_def e);
  match e.old_tail, e.new_tail with
  | a :: ats, b :: bts when a = b ->
    let rev = Keep a :: e.rev in
    let next = {x = e.x + 1; old_tail = ats; new_tail = bts; rev} in
    ghost_ (Entry_proof.keep_valid old fresh k depth e next a ats bts);
    let smaller = ghost_ (Bigint.sub remaining 1Z) in
    let next : {e : entry | valid old fresh k depth e && smaller = size
      e.old_tail} = next in
    snake smaller old fresh k depth next
  | _ -> e
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
    {r : entry option | let c = candidate in deleted c r
      && optional_valid old fresh (Bigint.add k 1Z) (Bigint.add depth 1Z) r} =
    fun old fresh k depth candidate ->
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
        ghost_ (Entry_proof.delete_valid old fresh k depth e next a ats);
        Some next
  in
  ghost_ (deleted_def candidate result);
  ghost_ (optional_valid_def old fresh (Bigint.add k 1Z) (Bigint.add depth 1Z)
    result);
  result

let (step_insert @ total) : (old : int list) @ ghost -> (fresh : int list) @
  ghost ->
    (k : Bigint.t) @ ghost -> (depth : Bigint.t) @ ghost ->
    (candidate : {c : entry option | optional_valid old fresh k depth c}) ->
    {r : entry option | let c = candidate in inserted c r
      && optional_valid old fresh (Bigint.sub k 1Z) (Bigint.add depth 1Z) r} =
    fun old fresh k depth candidate ->
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
        ghost_ (Entry_proof.insert_valid old fresh k depth e next b bts);
        Some next
  in
  ghost_ (inserted_def candidate result);
  ghost_ (optional_valid_def old fresh (Bigint.sub k 1Z) (Bigint.add depth 1Z)
    result);
  result

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
     minimum_cost_equation e.old_tail e.new_tail;
     size_def e.old_tail;
     size_def e.new_tail;
     match e.old_tail, e.new_tail with
     | _ :: _, _ :: _ -> ()
     | [], _ :: bts -> minimum_cost_equation [] bts
     | _ :: ats, [] -> minimum_cost_equation ats []
     | [], [] -> ());
  ()

module Choice_proof = struct
  let (selected @ total) (old : int list) (fresh : int list)
      (k : Bigint.t) (depth : Bigint.t) (budget : Bigint.t)
      (deletion : entry option) (insertion : entry option)
      (chosen : entry option) :
      {u : unit | if optional_valid old fresh k depth deletion
        && optional_valid old fresh k depth insertion
        && chosen === (match deletion, insertion with
          | None, other | other, None -> other
          | Some a, Some b -> if a.x > b.x then deletion else insertion)
        then optional_valid old fresh k depth chosen
          && (match chosen with
              | None -> true | Some e -> valid old fresh k depth e)
          && (if good deletion budget || good insertion budget
              then good chosen budget else true)
        else true} =
    optional_valid_def old fresh k depth deletion;
    optional_valid_def old fresh k depth insertion;
    good_def deletion budget;
    good_def insertion budget;
    (match deletion, insertion with
     | Some a, Some b ->
       dominance old fresh k depth a b;
       dominance old fresh k depth b a
     | _ -> ());
    optional_valid_def old fresh k depth chosen;
    good_def chosen budget;
    ()

  let (settled_result @ total) (old : int list) (fresh : int list)
      (k : Bigint.t) (depth : Bigint.t) (budget : Bigint.t)
      (chosen : entry option) (result : entry option) :
      {u : unit | if (match chosen, result with
        | None, None -> true
        | Some e, Some r -> valid old fresh k depth r && settled r
          && minimum_cost r.old_tail r.new_tail =
            minimum_cost e.old_tail e.new_tail
        | _ -> false)
        then optional_valid old fresh k depth result && optional_settled result
          && (if good chosen budget then good result budget else true)
        else true} =
    optional_valid_def old fresh k depth result;
    good_def chosen budget;
    good_def result budget;
    optional_settled_def result;
    ()
end

let (choose @ total) : (old : int list) @ ghost -> (fresh : int list) @ ghost
  ->
    (k : Bigint.t) @ ghost -> (depth : Bigint.t) @ ghost -> (budget : Bigint.t)
      @ ghost ->
    (deletion : {c : entry option | optional_valid old fresh k depth c}) ->
    (insertion : {c : entry option | optional_valid old fresh k depth c}) ->
    {r : entry option | let a = deletion in let b = insertion
      in
      optional_valid old fresh k depth r && optional_settled r
      && (if good a budget || good b budget then good r budget else true)} =
    fun old fresh k depth budget deletion insertion ->
  let chosen = match deletion, insertion with
    | None, other | other, None -> other
    | Some a, Some b ->
      if a.x > b.x then deletion else insertion
  in
  ghost_ (Choice_proof.selected old fresh k depth budget deletion insertion
    chosen);
  let result = match chosen with
    | None -> None
    | Some e ->
      let remaining = ghost_ (size e.old_tail) in
      let candidate : {e : entry | valid old fresh k depth e && remaining =
        size e.old_tail} = e in
      let e = snake remaining old fresh k depth candidate in Some e
  in
  ghost_ (Choice_proof.settled_result old fresh k depth budget chosen result);
  result

let[@def] rec width (frontier : entry option list) =
  match frontier with [] -> 0Z | _ :: rest -> Bigint.add 1Z (width rest)

module Frontier_proof = struct
  let (singleton @ total) (old : int list) (fresh : int list)
      (k : Bigint.t) (depth : Bigint.t) (budget : Bigint.t)
      (last : entry option) :
      {u : unit | width [last] = 1Z
        && (if optional_valid old fresh k depth last && optional_settled last
            then frontier_valid old fresh k depth [last] else true)
        && frontier_good [last] budget === good last budget} =
    width_def [last];
    width_def [];
    frontier_valid_def old fresh (Bigint.add k 2Z) depth [];
    frontier_valid_def old fresh k depth [last];
    frontier_good_def [last] budget;
    frontier_good_def [] budget;
    ()

  let (cons @ total) (old : int list) (fresh : int list)
      (k : Bigint.t) (depth : Bigint.t) (budget : Bigint.t)
      (next : entry option) (tail : entry option list) :
      {u : unit | width (next :: tail) = Bigint.add 1Z (width tail)
        && (if optional_valid old fresh k depth next && optional_settled next
              && frontier_valid old fresh (Bigint.add k 2Z) depth tail
            then frontier_valid old fresh k depth (next :: tail) else true)
        && frontier_good (next :: tail) budget ===
          (good next budget || frontier_good tail budget)} =
    width_def (next :: tail);
    frontier_valid_def old fresh k depth (next :: tail);
    frontier_good_def (next :: tail) budget;
    ()

  let (initial @ total) (old : int list) (fresh : int list) (first : entry) :
      {u : unit | if valid old fresh 0Z 0Z first && settled first
        && minimum_cost first.old_tail first.new_tail = minimum_cost old fresh
        then frontier_valid old fresh 0Z 0Z [Some first]
          && frontier_good [Some first] (minimum_cost old fresh)
          && width [Some first] = 1Z
          && minimum_cost old fresh <= Bigint.add (size old) (size fresh)
        else true} =
    optional_valid_def old fresh 0Z 0Z (Some first);
    optional_settled_def (Some first);
    good_def (Some first) (minimum_cost old fresh);
    singleton old fresh 0Z 0Z (minimum_cost old fresh) (Some first);
    M.properties old fresh;
    ()
end

let rec (advance @ total) : (old : int list) @ ghost -> (fresh : int list) @
  ghost ->
    (k : Bigint.t) @ ghost -> (depth : Bigint.t) @ ghost -> (budget : Bigint.t)
      @ ghost ->
    (left : {c : entry option | optional_valid old fresh k (Bigint.add depth
      1Z) c}) ->
    (frontier : {f : entry option list |
      frontier_valid old fresh (Bigint.add k 1Z) depth f && frontier_unfinished
        f}) ->
    {r : entry option list | let c = left in let f = frontier
      in
      frontier_valid old fresh k (Bigint.add depth 1Z) r
      && width r = Bigint.add 1Z (width f)
      && (if good c (Bigint.sub budget 1Z) || frontier_good f budget
          then frontier_good r (Bigint.sub budget 1Z) else true)} =
    fun old fresh k depth budget left frontier ->
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
      None in
    let last = choose old fresh k next_depth lower left absent in
    let result = [last] in
    ghost_ (Frontier_proof.singleton old fresh k next_depth lower last);
    result
  | right :: rest ->
    let candidate : {c : entry option | optional_valid old fresh right_k depth
      c} = right in
    let deletion = step_delete old fresh right_k depth candidate in
    let insertion = step_insert old fresh right_k depth candidate in
    ghost_ (branches right deletion insertion budget);
    let insertion : {c : entry option | optional_valid old fresh k next_depth
      c} = insertion in
    let next = choose old fresh k next_depth lower left insertion in
    let next_k = ghost_ (Bigint.add k 2Z) in
    let deletion : {c : entry option | optional_valid old fresh next_k
      next_depth c} = deletion in
    let rest : {f : entry option list |
      frontier_valid old fresh (Bigint.add next_k 1Z) depth f &&
        frontier_unfinished f} = rest in
    let tail = advance old fresh next_k depth budget deletion rest in
    let result = next :: tail in
    ghost_ (Frontier_proof.cons old fresh k next_depth lower next tail);
    result

let rec (finished @ total) : (old : int list) @ ghost -> (fresh : int list) @
  ghost ->
    (k : Bigint.t) @ ghost -> (depth : Bigint.t) @ ghost ->
    (frontier : {f : entry option list | frontier_valid old fresh k depth f})
      ->
    {s : script option | let f = frontier in match s with
      | None -> frontier_unfinished f
      | Some s -> source s === old && target s === fresh && cost s = depth} =
    fun old fresh k depth frontier ->
  ghost_ (frontier_valid_def old fresh k depth frontier);
  ghost_ (frontier_unfinished_def frontier);
  match frontier with
  | [] -> None
  | candidate :: rest ->
    ghost_ (optional_valid_def old fresh k depth candidate);
    ghost_ (unfinished_def candidate);
    match candidate with
    | Some ({old_tail = []; new_tail = []; _} as e) ->
      ghost_ (valid_def old fresh k depth e);
      ghost_ (source_def []);
      ghost_ (target_def []);
      ghost_ (cost_def []);
      let result = reverse_into e.rev [] in
      let result = Some result in result
    | _ ->
      let next_k = ghost_ (Bigint.add k 2Z) in
      let rest : {f : entry option list | frontier_valid old fresh next_k depth
        f} = rest in
      finished old fresh next_k depth rest

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
  ()

let rec (search @ total) : (old : int list) @ ghost -> (fresh : int list) @
  ghost ->
    (k : Bigint.t) @ ghost -> (depth : Bigint.t) @ ghost -> (budget : Bigint.t)
      @ ghost ->
    (frontier : {f : entry option list | frontier_valid old fresh k depth f
      && frontier_good f budget
      && 0Z <= depth && width f = Bigint.add depth 1Z
      && Bigint.add depth budget <= Bigint.add (size old) (size
        fresh)}) ->
    {s : script | source s === old && target s === fresh && cost s <=
      Bigint.add depth budget} =
    fun old fresh k depth budget frontier ->
  let f = frontier in
  ghost_ (frontier_budget old fresh k depth f budget);
  let current : {f : entry option list | frontier_valid old fresh k depth f} =
    f in
  let result = finished old fresh k depth current in
  match result with
  | Some s -> s
  | None ->
    let next_k = ghost_ (Bigint.sub k 1Z) in
    let next_depth = ghost_ (Bigint.add depth 1Z) in
    let lower = ghost_ (Bigint.sub budget 1Z) in
    ghost_ (optional_valid_def old fresh next_k next_depth None);
    let left : {c : entry option | optional_valid old fresh next_k next_depth
      c} = None in
    let current : {f : entry option list |
      frontier_valid old fresh (Bigint.add next_k 1Z) depth f &&
        frontier_unfinished f} = f in
    let next = advance old fresh next_k depth budget left current in
    let next : {f : entry option list | frontier_valid old fresh next_k
      next_depth f
      && frontier_good f lower
      && 0Z <= next_depth && width f = Bigint.add next_depth 1Z
      && Bigint.add next_depth lower <= Bigint.add (size
        old) (size fresh)} = next in
    search old fresh next_k next_depth lower next
[@@decreases budget]

let rec (bounded_length @ total) : (bound : int) -> (values : int list) ->
    {r : int option | match r with
      | None -> Bigint.of_int bound < size values
      | Some n -> 0 <= n && n <= bound && Bigint.of_int n = size values} = fun
        bound values ->
  ghost_ (size_def values);
  ghost_ (size_nonnegative values);
  match values with
  | [] -> if bound < 0 then None else (Some 0)
  | _ :: rest ->
    ghost_ (size_nonnegative rest);
    if bound <= 0 then None else
    let result = bounded_length (bound - 1) rest in
    match result with
    | None -> None
    | Some n -> (Some (n + 1))

type error = Input_too_large [@@inductive]

let (diff @ total) : (old : int list) -> (fresh : int list) ->
    {r : (script, error) result | match r with
      | Error Input_too_large -> 1000000Z < size old || 1000000Z < size fresh
      | Ok s -> size old <= 1000000Z && size fresh <= 1000000Z
        && source s === old && target s === fresh
        && apply old s === Some fresh && cost s = minimum_cost old fresh
        && 0Z <= cost s && script_size s <= Bigint.add (size old) (size fresh)}
          = fun old fresh ->
  let n = bounded_length 1000000 old in
  let m = bounded_length 1000000 fresh in
  match n, m with
  | Some n, Some m ->
    let initial = {x = 0; old_tail = old; new_tail = fresh; rev = []} in
    ghost_ (Entry_proof.initial_valid old fresh initial);
    let remaining = ghost_ (size old) in
    let candidate : {e : entry | valid old fresh 0Z 0Z e && remaining = size
      e.old_tail} = initial in
    let first = snake remaining (ghost_ old) (ghost_ fresh) (ghost_ 0Z)
      (ghost_ 0Z) candidate in
    let frontier = [Some first] in
    let budget = ghost_ (minimum_cost old fresh) in
    ghost_ (Frontier_proof.initial old fresh first);
    let frontier : {f : entry option list | frontier_valid old fresh 0Z 0Z f
      && frontier_good f budget
      && width f = 1Z
      && budget <= Bigint.add (size old) (size fresh)} = frontier in
    let script = search (ghost_ old) (ghost_ fresh) (ghost_ 0Z) (ghost_
      0Z) budget frontier in
    ghost_ (M.lower_bound script);
    ghost_ (apply_source script);
    ghost_ (script_bounds script);
    let result : (script, error) result = Ok script in result
  | _ ->
    let result : (script, error) result = Error Input_too_large in result

let (optimal_at @ total) : (old : int list) -> (fresh : int list) ->
    (computed : {s : script | cost s = minimum_cost old fresh}) -> (other :
      script)
      ->
    {u : unit |
      if apply old other === Some fresh then cost computed <= cost other
      else true} =
    fun old fresh computed other ->
  apply_characterization old other;
  M.lower_bound other;
  ()

let (invert_correct @ total) (script : script) :
    {u : unit | source (invert script) === target script
      && target (invert script) === source script
      && cost (invert script) = cost script} =
  Proof.invert_correct script

let (inverse_patch @ total) (script : script) :
    {u : unit | apply (target script) (invert script) === Some (source script)}
      =
  Proof.inverse_patch script
