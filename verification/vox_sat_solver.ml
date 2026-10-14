type vector =
  { mutable data : int array;
    mutable size : int
  }

let vector () = { data = [||]; size = 0 }

let append v x =
  if v.size = Array.length v.data
  then (
    let data = Array.make (max 4 (2 * v.size)) 0 in
    Array.blit v.data 0 data 0 v.size;
    v.data <- data);
  v.data.(v.size) <- x;
  v.size <- v.size + 1

let append_pair v x y =
  let size = v.size in
  if size + 2 > Array.length v.data then begin
    let data = Array.make (max (size + 2) (max 4 (2 * size))) 0 in
    Array.blit v.data 0 data 0 size;
    v.data <- data
  end;
  v.data.(size) <- x;
  v.data.(size + 1) <- y;
  v.size <- size + 2

type clause =
  { mutable literals : int array;
    learnt : bool;
    mutable activity : float;
    lbd : int;
    mutable locked : int;
    mutable search_start : int;
    mutable deleted : bool
  }

let empty_clause =
  { literals = [||];
    learnt = false;
    activity = 0.;
    lbd = 0;
    locked = 0;
    search_start = 2;
    deleted = false
  }

type statistics =
  { solves : int;
    decisions : int;
    propagations : int;
    conflicts : int;
    restarts : int;
    learnt_clauses : int;
    deleted_clauses : int
  }

type check_result =
  | Done
  | Continue
  | Incomplete

type external_reason =
  { consequent : int;
    preceding : int;
    explain : unit -> int array;
    mutable expanded : int array option
  }

type t =
  { mutable variables : int;
    mutable values : int array;
    mutable required : bool array;
    mutable levels : int array;
    mutable reasons : int array;
    mutable trail_positions : int array;
    mutable external_reasons : external_reason option array;
    mutable phases : int array;
    mutable activities : float array;
    mutable seen : int array;
    mutable level_seen : int array;
    mutable stamp : int;
    mutable watches : vector array;
    mutable binary : vector array;
    binary_clauses : vector;
    mutable clauses : clause array;
    mutable clause_count : int;
    learnt : vector;
    trail : vector;
    limits : vector;
    scopes : vector;
    mutable qhead : int;
    heap : vector;
    mutable positions : int array;
    mutable var_increment : float;
    mutable clause_increment : float;
    mutable consistent : bool;
    mutable busy : bool;
    mutable solves : int;
    mutable decisions : int;
    mutable propagations : int;
    mutable conflicts : int;
    mutable restarts : int;
    mutable learnt_clauses : int;
    mutable deleted_clauses : int;
    mutable next_reduction : int;
    mutable extension : extension option;
    mutable theory_head : int;
    mutable theory_conflict : int array option;
    pending_lemmas : int array Queue.t;
    mutable callback_active : bool;
    mutable callback_epoch : int;
    mutable theory_running : bool;
    mutable occurrence_activity : bool
  }

and context =
  { solver : t;
    epoch : int
  }

and extension =
  { start : unit -> unit;
    push_level : unit -> unit;
    pop_levels : int -> unit;
    assigned : context -> int -> unit;
    propagate : context -> unit;
    final_check : context -> check_result;
    finish : unit -> unit
  }

type result =
  | Sat of bool array
  | Unsat of int array
  | Unknown

let variable_count s = s.variables

let variable lit = lit lsr 1

let negate lit = lit lxor 1

let value s lit =
  let v = s.values.(variable lit) in
  if v < 0 then -1 else v lxor (lit land 1)

let check_idle s =
  if s.busy then invalid_arg "Vox_sat_solver: reentrant operation"

let heap_up s index =
  let heap = s.heap.data in
  let v = heap.(index) in
  let i = ref index in
  while !i > 0 && s.activities.(v) > s.activities.(heap.((!i - 1) lsr 1)) do
    let parent = (!i - 1) lsr 1 in
    heap.(!i) <- heap.(parent);
    s.positions.(heap.(!i)) <- !i;
    i := parent
  done;
  heap.(!i) <- v;
  s.positions.(v) <- !i

let heap_insert s v =
  if s.required.(v) && s.positions.(v) < 0
  then (
    append s.heap v;
    heap_up s (s.heap.size - 1))

let heap_remove s =
  let heap = s.heap.data in
  let result = heap.(0) in
  s.heap.size <- s.heap.size - 1;
  s.positions.(result) <- -1;
  if s.heap.size > 0
  then (
    let v = heap.(s.heap.size) in
    let i = ref 0 in
    let descending = ref true in
    while !descending do
      let left = (2 * !i) + 1 in
      if left >= s.heap.size
      then descending := false
      else
        let right = left + 1 in
        let child =
          if
            right < s.heap.size
            && s.activities.(heap.(right)) > s.activities.(heap.(left))
          then right
          else left
        in
        if s.activities.(v) >= s.activities.(heap.(child))
        then descending := false
        else (
          heap.(!i) <- heap.(child);
          s.positions.(heap.(!i)) <- !i;
          i := child)
    done;
    heap.(!i) <- v;
    s.positions.(v) <- !i);
  result

let grow array capacity default =
  if capacity <= Array.length array
  then array
  else
    let result = Array.make capacity default in
    Array.blit array 0 result 0 (Array.length array);
    result

let allocate_variable ?(completion = false) s =
  let v = s.variables in
  if v = Array.length s.values
  then (
    let capacity = max 16 (2 * v) in
    s.values <- grow s.values capacity (-1);
    s.required <- grow s.required capacity false;
    s.levels <- grow s.levels capacity 0;
    s.reasons <- grow s.reasons capacity (-1);
    s.trail_positions <- grow s.trail_positions capacity (-1);
    s.external_reasons <- grow s.external_reasons capacity None;
    s.phases <- grow s.phases capacity 0;
    s.activities <- grow s.activities capacity 0.;
    s.seen <- grow s.seen capacity 0;
    s.level_seen <- grow s.level_seen (capacity + 1) 0;
    s.positions <- grow s.positions capacity (-1);
    let extend old =
      Array.init (2 * capacity) (fun i ->
          if i < Array.length old then old.(i) else vector ())
    in
    s.watches <- extend s.watches;
    s.binary <- extend s.binary);
  s.variables <- v + 1;
  s.required.(v) <- not completion;
  heap_insert s v;
  v + 1

let new_var ?completion s =
  check_idle s;
  allocate_variable ?completion s

let create ?(variables = 0) ?extension () =
  if variables < 0 || variables > Sys.max_array_length / 2
  then invalid_arg "Vox_sat_solver.create: invalid variable count";
  let s =
    { variables = 0;
      values = [||];
      required = [||];
      levels = [||];
      reasons = [||];
      trail_positions = [||];
      external_reasons = [||];
      phases = [||];
      activities = [||];
      seen = [||];
      level_seen = [||];
      stamp = 0;
      watches = [||];
      binary = [||];
      binary_clauses = vector ();
      clauses = [||];
      clause_count = 0;
      learnt = vector ();
      trail = vector ();
      limits = vector ();
      scopes = vector ();
      qhead = 0;
      heap = vector ();
      positions = [||];
      var_increment = 1.;
      clause_increment = 1.;
      consistent = true;
      busy = false;
      solves = 0;
      decisions = 0;
      propagations = 0;
      conflicts = 0;
      restarts = 0;
      learnt_clauses = 0;
      deleted_clauses = 0;
      next_reduction = 2000;
      extension;
      theory_head = 0;
      theory_conflict = None;
      pending_lemmas = Queue.create ();
      callback_active = false;
      callback_epoch = 0;
      theory_running = false;
      occurrence_activity = false
    }
  in
  for _ = 1 to variables do
    ignore (allocate_variable s)
  done;
  s

let attach_extension s extension =
  check_idle s;
  match s.extension with
  | Some _ -> invalid_arg "Vox_sat_solver.attach_extension: already attached"
  | None -> s.extension <- Some extension

let check_literal s lit =
  if lit = 0 || lit > s.variables || lit < -s.variables
  then invalid_arg "Vox_sat_solver: unallocated or zero literal"

let encode lit = if lit > 0 then 2 * (lit - 1) else 2 * (-lit - 1) lor 1

let decode lit =
  let v = variable lit + 1 in
  if lit land 1 = 0 then v else -v

let enqueue s lit reason =
  let v = variable lit in
  s.values.(v) <- 1 lxor (lit land 1);
  s.levels.(v) <- s.limits.size;
  s.reasons.(v) <- reason;
  s.trail_positions.(v) <- s.trail.size;
  (if reason >= 0
   then
     let c = s.clauses.(reason) in
     c.locked <- c.locked + 1);
  append s.trail lit

let cancel_until s level =
  if s.limits.size > level
  then (
    let count = s.limits.size - level in
    let target = s.limits.data.(level) in
    for i = s.trail.size - 1 downto target do
      let v = variable s.trail.data.(i) in
      s.phases.(v) <- s.values.(v);
      s.values.(v) <- -1;
      let reason = s.reasons.(v) in
      if reason >= 0
      then s.clauses.(reason).locked <- s.clauses.(reason).locked - 1;
      s.reasons.(v) <- -1;
      s.external_reasons.(v) <- None;
      s.trail_positions.(v) <- -1;
      s.levels.(v) <- 0;
      heap_insert s v
    done;
    s.trail.size <- target;
    s.qhead <- min s.qhead target;
    s.limits.size <- level;
    s.theory_head <- min s.theory_head target;
    if s.theory_running
    then Option.iter (fun ext -> ext.pop_levels count) s.extension)

let push_level s =
  append s.limits s.trail.size;
  s.level_seen <- grow s.level_seen (s.limits.size + 1) 0;
  if s.theory_running
  then Option.iter (fun ext -> ext.push_level ()) s.extension

let seed_activity s weight literal =
  let v = variable literal in
  s.activities.(v) <- s.activities.(v) +. weight;
  if s.positions.(v) >= 0 then heap_up s s.positions.(v)

let store_binary_clause s first second =
  if s.occurrence_activity then begin
    seed_activity s 0.0000005 first;
    seed_activity s 0.0000005 second
  end;
  let require literal =
    let v = variable literal in
    if not s.required.(v) then begin
      s.required.(v) <- true;
      if s.values.(v) < 0 then heap_insert s v
    end in
  require first;
  require second;
  let reason = -3 - s.binary_clauses.size in
  append_pair s.binary_clauses first second;
  append_pair s.binary.(negate first) second reason;
  append_pair s.binary.(negate second) first reason

let clause_literals s reason =
  if reason >= 0 then s.clauses.(reason).literals
  else if reason <= -3 then begin
    let offset = -3 - reason in
    [|s.binary_clauses.data.(offset); s.binary_clauses.data.(offset + 1)|]
  end else invalid_arg "Vox_sat_solver: invalid stored clause"

let store_clause s ~learnt ~lbd literals =
  if not learnt && s.occurrence_activity then
    Array.iter
      (seed_activity s (0.000001 /. float_of_int (Array.length literals)))
      literals;
  Array.iter (fun lit ->
    let v = variable lit in
    if not s.required.(v) then begin
      s.required.(v) <- true;
      if s.values.(v) < 0 then heap_insert s v
    end) literals;
  if s.clause_count = Array.length s.clauses
  then s.clauses <- grow s.clauses (max 16 (2 * s.clause_count)) empty_clause;
  let id = s.clause_count in
  s.clause_count <- id + 1;
  s.clauses.(id)
    <- { literals;
         learnt;
         activity = 0.;
         lbd;
         locked = 0;
         search_start = 2;
         deleted = false
       };
  let n = Array.length literals in
  if n = 2
  then (
    append_pair s.binary.(negate literals.(0)) literals.(1) id;
    append_pair s.binary.(negate literals.(1)) literals.(0) id)
  else if n > 2
  then (
    append_pair s.watches.(negate literals.(0)) id literals.(1);
    append_pair s.watches.(negate literals.(1)) id literals.(0));
  if learnt
  then (
    append s.learnt id;
    s.learnt_clauses <- s.learnt_clauses + 1);
  id

let checked_context context =
  let s = context.solver in
  if (not s.callback_active) || context.epoch <> s.callback_epoch
  then invalid_arg "Vox_sat_solver: inactive theory context";
  s

let current_level context = (checked_context context).limits.size

let assigned_value context lit =
  let s = checked_context context in
  check_literal s lit;
  match value s (encode lit) with -1 -> None | v -> Some (v = 1)

let completion_value context lit =
  let s = checked_context context in
  check_literal s lit;
  match value s (encode lit) with
  | -1 when not s.required.(variable (encode lit)) -> Some (lit < 0)
  | -1 -> None
  | v -> Some (v = 1)

let new_theory_var ?completion context =
  allocate_variable ?completion (checked_context context)

let with_callback s callback =
  s.callback_epoch <- s.callback_epoch + 1;
  let context = { solver = s; epoch = s.callback_epoch } in
  s.callback_active <- true;
  match callback context with
  | result ->
    s.callback_active <- false;
    result
  | exception exn ->
    s.callback_active <- false;
    raise exn

let explain s preceding thunk =
  let active = s.callback_active in
  s.callback_active <- false;
  match
    let antecedents = thunk () in
    Array.map
      (fun lit ->
        check_literal s lit;
        let lit = encode lit in
        if value s lit <> 1 || s.trail_positions.(variable lit) >= preceding
        then invalid_arg "Vox_sat_solver: invalid theory antecedent";
        negate lit)
      antecedents
  with
  | result ->
    s.callback_active <- active;
    result
  | exception exn ->
    s.callback_active <- active;
    raise exn

let reason_literals s v =
  let reason = s.reasons.(v) in
  if reason >= 0 || reason <= -3
  then clause_literals s reason
  else
    match s.external_reasons.(v) with
    | Some { expanded = Some literals; _ } -> literals
    | Some reason ->
      let antecedents = explain s reason.preceding reason.explain in
      let literals = Array.append [| reason.consequent |] antecedents in
      reason.expanded <- Some literals;
      Queue.add (Array.copy literals) s.pending_lemmas;
      literals
    | None -> invalid_arg "Vox_sat_solver: missing implication reason"

let imply context lit ~reason =
  let s = checked_context context in
  check_literal s lit;
  let lit = encode lit in
  match value s lit with
  | 1 -> ()
  | 0 ->
    let antecedents = explain s s.trail.size reason in
    s.theory_conflict <- Some (Array.append [| lit |] antecedents)
  | _ ->
    if s.limits.size = 0
    then (
      ignore (explain s s.trail.size reason);
      let id = store_clause s ~learnt:false ~lbd:0 [| lit |] in
      enqueue s lit id)
    else (
      s.external_reasons.(variable lit)
        <- Some
             { consequent = lit;
               preceding = s.trail.size;
               explain = reason;
               expanded = None
             };
      enqueue s lit (-2))

let conflict context ~reason =
  let s = checked_context context in
  s.theory_conflict <- Some (explain s s.trail.size reason)

let add_lemma context literals =
  let s = checked_context context in
  Array.iter (check_literal s) literals;
  Queue.add (Array.map encode literals) s.pending_lemmas

let install_lemma s literals =
  Array.sort compare literals;
  let size = ref 0 in
  let previous = ref (-2) in
  let tautology = ref false in
  Array.iter
    (fun lit ->
      if lit = negate !previous then tautology := true;
      if lit <> !previous
      then (
        literals.(!size) <- lit;
        incr size);
      previous := lit)
    literals;
  if not !tautology
  then (
    let literals = Array.sub literals 0 !size in
    Array.sort
      (fun a b ->
        let a_false = value s a = 0 in
        let b_false = value s b = 0 in
        match a_false, b_false with
        | false, true -> -1
        | true, false -> 1
        | true, true -> compare s.levels.(variable b) s.levels.(variable a)
        | false, false -> 0)
      literals;
    let id = store_clause s ~learnt:false ~lbd:0 literals in
    if !size = 0 || value s literals.(0) = 0
    then begin
      s.theory_conflict <- Some literals;
      if !size = 0 || s.levels.(variable literals.(0)) = 0
      then s.consistent <- false
    end
    else if !size = 1 || value s literals.(1) = 0
    then
      let lit = literals.(0) in
      let level = if !size = 1 then 0 else s.levels.(variable literals.(1)) in
      if value s lit < 0 || s.levels.(variable lit) > level
      then (
        (try cancel_until s level with exn ->
          Queue.add (Array.copy literals) s.pending_lemmas;
          raise exn);
        enqueue s lit id))

exception Stopped

let propagate s should_stop =
  let conflict = ref (-1) in
  while !conflict = -1 && s.qhead < s.trail.size do
    (* Poll between trail entries: each watch list is compacted in place. *)
    if s.propagations land 1023 = 0 && should_stop () then raise Stopped;
    let p = s.trail.data.(s.qhead) in
    s.qhead <- s.qhead + 1;
    s.propagations <- s.propagations + 1;
    let binary = s.binary.(p) in
    let i = ref 0 in
    while !conflict = -1 && !i < binary.size do
      let lit = binary.data.(!i) in
      let reason = binary.data.(!i + 1) in
      let v = value s lit in
      if v = 0 then conflict := reason else if v < 0 then enqueue s lit reason;
      i := !i + 2
    done;
    if !conflict = -1
    then (
      let watched = s.watches.(p) in
      let read = ref 0 in
      let write = ref 0 in
      while !conflict = -1 && !read < watched.size do
        let id = watched.data.(!read) in
        let blocker = watched.data.(!read + 1) in
        read := !read + 2;
        let c = s.clauses.(id) in
        if not c.deleted
        then (
          if value s blocker = 1
          then (
            watched.data.(!write) <- id;
            watched.data.(!write + 1) <- blocker;
            write := !write + 2)
          else
            let literals = c.literals in
            let false_lit = negate p in
            if literals.(0) = false_lit
            then (
              literals.(0) <- literals.(1);
              literals.(1) <- false_lit);
            let first = literals.(0) in
            if value s first = 1
            then (
              watched.data.(!write) <- id;
              watched.data.(!write + 1) <- first;
              write := !write + 2)
            else
              let start = c.search_start in
              let replacement = ref start in
              while
                !replacement < Array.length literals
                && value s literals.(!replacement) = 0
              do
                incr replacement
              done;
              if !replacement = Array.length literals
              then (
                replacement := 2;
                while
                  !replacement < start && value s literals.(!replacement) = 0
                do
                  incr replacement
                done;
                if !replacement = start
                then replacement := Array.length literals);
              if !replacement < Array.length literals
              then (
                c.search_start
                  <- (if !replacement + 1 < Array.length literals
                      then !replacement + 1
                      else 2);
                let lit = literals.(!replacement) in
                literals.(1) <- lit;
                literals.(!replacement) <- false_lit;
                append_pair s.watches.(negate lit) id first)
              else (
                watched.data.(!write) <- id;
                watched.data.(!write + 1) <- first;
                write := !write + 2;
                match value s first with
                | 0 -> conflict := id
                | _ -> enqueue s first id))
      done;
      if !read < watched.size
      then (
        let remaining = watched.size - !read in
        Array.blit watched.data !read watched.data !write remaining;
        write := !write + remaining);
      watched.size <- !write)
  done;
  !conflict

let propagate_all s should_stop =
  let rec theory_loop ext =
    if should_stop () then raise Stopped;
    match s.theory_conflict with
    | Some literals ->
      s.theory_conflict <- None;
      Some (-1, literals)
    | None ->
      if not (Queue.is_empty s.pending_lemmas)
      then (
        install_lemma s (Queue.take s.pending_lemmas);
        theory_loop ext)
      else
        let conflict = propagate s should_stop in
        if conflict <> -1
        then Some (conflict, clause_literals s conflict)
        else if s.theory_head < s.trail.size
        then (
          let lit = s.trail.data.(s.theory_head) in
          s.theory_head <- s.theory_head + 1;
          with_callback s (fun context -> ext.assigned context (decode lit));
          theory_loop ext)
        else (
          with_callback s ext.propagate;
          if
            s.theory_conflict <> None
            || (not (Queue.is_empty s.pending_lemmas))
            || s.qhead < s.trail.size
          then theory_loop ext
          else None)
  in
  match s.extension with
  | Some ext -> theory_loop ext
  | None ->
    let conflict = propagate s should_stop in
    if conflict <> -1
    then Some (conflict, clause_literals s conflict)
    else None

let never_stop () = false

let add_encoded_clause s literals =
  (* Only root assignments may simplify clauses that outlive a solve. *)
  if s.consistent
  then (
    let swap i j =
      if literals.(i) > literals.(j) then begin
        let value = literals.(i) in
        literals.(i) <- literals.(j);
        literals.(j) <- value
      end in
    (match Array.length literals with
    | 0 | 1 -> ()
    | 2 -> swap 0 1
    | 3 -> swap 0 1; swap 1 2; swap 0 1
    | _ -> Array.sort Int.compare literals);
    let size = ref 0 in
    let satisfied = ref false in
    let previous = ref (-2) in
    Array.iter
      (fun lit ->
        if lit = negate !previous || value s lit = 1 then satisfied := true;
        if lit <> !previous && value s lit <> 0
        then (
          literals.(!size) <- lit;
          incr size);
        previous := lit)
      literals;
    if not !satisfied then
      if !size = 0 then s.consistent <- false
      else if !size = 2 then store_binary_clause s literals.(0) literals.(1)
      else
        let literals = if !size = Array.length literals then literals
        else Array.sub literals 0 !size in
        let id = store_clause s ~learnt:false ~lbd:0 literals in
        if !size = 1 then enqueue s literals.(0) id)

let add_clause_internal s ~scoped literals =
  check_idle s;
  Array.iter (check_literal s) literals;
  cancel_until s 0;
  let size = Array.length literals in
  let scoped = scoped && s.scopes.size > 0 in
  let encoded = Array.make (size + if scoped then 1 else 0) 0 in
  Array.iteri (fun i lit -> encoded.(i) <- encode lit) literals;
  if scoped then encoded.(size) <- negate s.scopes.data.(s.scopes.size - 1);
  add_encoded_clause s encoded

let add_clause s literals = add_clause_internal s ~scoped:true literals

let add_clause_global s literals = add_clause_internal s ~scoped:false literals

let push s =
  check_idle s;
  cancel_until s 0;
  append s.scopes (encode (allocate_variable s))

let pop s =
  check_idle s;
  if s.scopes.size = 0 then invalid_arg "Vox_sat_solver.pop: no scope";
  cancel_until s 0;
  s.scopes.size <- s.scopes.size - 1;
  add_encoded_clause s [| negate s.scopes.data.(s.scopes.size) |]

let fresh_stamp s =
  if s.stamp = max_int
  then (
    Array.fill s.seen 0 (Array.length s.seen) 0;
    Array.fill s.level_seen 0 (Array.length s.level_seen) 0;
    s.stamp <- 0);
  s.stamp <- s.stamp + 1;
  s.stamp

let bump_variable s v =
  s.activities.(v) <- s.activities.(v) +. s.var_increment;
  if s.activities.(v) > 1e100
  then (
    for i = 0 to s.variables - 1 do
      s.activities.(i) <- s.activities.(i) *. 1e-100
    done;
    s.var_increment <- s.var_increment *. 1e-100);
  if s.positions.(v) >= 0 then heap_up s s.positions.(v)

let bump_clause s id =
  let c = s.clauses.(id) in
  if c.learnt
  then (
    c.activity <- c.activity +. s.clause_increment;
    if c.activity > 1e20
    then (
      for i = 0 to s.learnt.size - 1 do
        let c = s.clauses.(s.learnt.data.(i)) in
        c.activity <- c.activity *. 1e-20
      done;
      s.clause_increment <- s.clause_increment *. 1e-20))

let analyze s conflict_id conflict =
  let stamp = fresh_stamp s in
  let learnt = vector () in
  append learnt 0;
  let path = ref 0 in
  let cursor = ref (s.trail.size - 1) in
  let pivot = ref (-1) in
  let reason = ref conflict_id in
  let current_literals = ref conflict in
  let searching = ref true in
  while !searching do
    if !reason >= 0 then bump_clause s !reason;
    let literals = !current_literals in
    for i = 0 to Array.length literals - 1 do
      let lit = literals.(i) in
      let v = variable lit in
      if v <> !pivot && s.seen.(v) <> stamp && s.levels.(v) > 0
      then (
        s.seen.(v) <- stamp;
        bump_variable s v;
        if s.levels.(v) = s.limits.size then incr path else append learnt lit)
    done;
    while s.seen.(variable s.trail.data.(!cursor)) <> stamp do
      decr cursor
    done;
    let lit = s.trail.data.(!cursor) in
    pivot := variable lit;
    s.seen.(!pivot) <- 0;
    decr cursor;
    decr path;
    if !path = 0
    then (
      learnt.data.(0) <- negate lit;
      searching := false)
    else (
      reason := s.reasons.(!pivot);
      current_literals := reason_literals s !pivot)
  done;
  let write = ref 1 in
  for i = 1 to learnt.size - 1 do
    let lit = learnt.data.(i) in
    let v = variable lit in
    let reason = s.reasons.(v) in
    let redundant = ref (reason <> -1) in
    (if reason <> -1
     then
       let antecedents = reason_literals s v in
       let j = ref 0 in
       while !redundant && !j < Array.length antecedents do
         let u = variable antecedents.(!j) in
         if u <> v && s.levels.(u) > 0 && s.seen.(u) <> stamp
         then redundant := false;
         incr j
       done);
    if not !redundant
    then (
      learnt.data.(!write) <- lit;
      incr write)
  done;
  let literals = Array.sub learnt.data 0 !write in
  let backtrack = ref 0 in
  let highest = ref 1 in
  let lbd = ref 0 in
  Array.iteri
    (fun i lit ->
      let level = s.levels.(variable lit) in
      if s.level_seen.(level) <> stamp
      then (
        s.level_seen.(level) <- stamp;
        incr lbd);
      if i > 0 && level > !backtrack
      then (
        backtrack := level;
        highest := i))
    literals;
  if Array.length literals > 1
  then (
    let lit = literals.(1) in
    literals.(1) <- literals.(!highest);
    literals.(!highest) <- lit);
  literals, !backtrack, !lbd

let assumption_core s assumptions index =
  let stamp = fresh_stamp s in
  let included = Array.make (Array.length assumptions) false in
  included.(index) <- true;
  s.seen.(variable assumptions.(index)) <- stamp;
  for i = s.trail.size - 1 downto 0 do
    let v = variable s.trail.data.(i) in
    if s.seen.(v) = stamp && s.levels.(v) > 0
    then
      let reason = s.reasons.(v) in
      if reason = -1
      then (
        let index = s.levels.(v) - 1 in
        if index < Array.length assumptions then included.(index) <- true)
      else
        Array.iter
          (fun lit ->
            let u = variable lit in
            if s.levels.(u) > 0 then s.seen.(u) <- stamp)
          (reason_literals s v)
  done;
  let core = vector () in
  for i = s.scopes.size to Array.length assumptions - 1 do
    if included.(i) then append core (decode assumptions.(i))
  done;
  Array.sub core.data 0 core.size

let reduce_clauses s =
  let candidates = vector () in
  for i = 0 to s.learnt.size - 1 do
    let id = s.learnt.data.(i) in
    let c = s.clauses.(id) in
    if
      (not c.deleted)
      && Array.length c.literals > 2
      && c.lbd > 2 && c.locked = 0
    then append candidates id
  done;
  let candidates = Array.sub candidates.data 0 candidates.size in
  Array.sort
    (fun a b -> Float.compare s.clauses.(a).activity s.clauses.(b).activity)
    candidates;
  for i = 0 to (Array.length candidates / 2) - 1 do
    let c = s.clauses.(candidates.(i)) in
    c.deleted <- true;
    c.literals <- [||];
    s.deleted_clauses <- s.deleted_clauses + 1
  done;
  let write = ref 0 in
  for i = 0 to s.learnt.size - 1 do
    let id = s.learnt.data.(i) in
    if not s.clauses.(id).deleted
    then (
      s.learnt.data.(!write) <- id;
      incr write)
  done;
  s.learnt.size <- !write;
  s.next_reduction <- s.conflicts + 2000

let luby index =
  let size = ref 1 in
  let exponent = ref 0 in
  while !size < index + 1 do
    size := (2 * !size) + 1;
    incr exponent
  done;
  let index = ref index in
  while !size - 1 <> !index do
    size := (!size - 1) lsr 1;
    decr exponent;
    index := !index mod !size
  done;
  1 lsl !exponent

let search s assumptions max_conflicts should_stop =
  let initial_conflicts = s.conflicts in
  let restart_index = ref 0 in
  let restart_at = ref (s.conflicts + 1000) in
  let lbd_window = 50 in
  let recent_lbd = Array.make lbd_window 0 in
  let recent_lbd_size = ref 0 in
  let recent_lbd_cursor = ref 0 in
  let recent_lbd_sum = ref 0 in
  let lbd_total = ref 0. in
  let lbd_count = ref 0 in
  let restart_due = ref false in
  let rec loop () =
    if should_stop () || s.conflicts - initial_conflicts >= max_conflicts
    then Unknown
    else if not s.consistent
    then Unsat [||]
    else
      match propagate_all s should_stop with
      | Some (conflict_id, conflict) ->
        s.conflicts <- s.conflicts + 1;
        let conflict_level =
          Array.fold_left
            (fun level lit -> max level s.levels.(variable lit))
            0 conflict
        in
        cancel_until s conflict_level;
        if s.limits.size = 0
        then (
          s.consistent <- false;
          Unsat [||])
        else
          let literals, backtrack, lbd = analyze s conflict_id conflict in
          if !recent_lbd_size = lbd_window
          then
            recent_lbd_sum := !recent_lbd_sum - recent_lbd.(!recent_lbd_cursor)
          else incr recent_lbd_size;
          recent_lbd.(!recent_lbd_cursor) <- lbd;
          recent_lbd_cursor := (!recent_lbd_cursor + 1) mod lbd_window;
          recent_lbd_sum := !recent_lbd_sum + lbd;
          lbd_total := !lbd_total +. float_of_int lbd;
          incr lbd_count;
          restart_due
            := !recent_lbd_size = lbd_window
               && float_of_int !recent_lbd_sum *. float_of_int !lbd_count
                  > 1.25 *. float_of_int lbd_window *. !lbd_total;
          cancel_until s backtrack;
          let id = store_clause s ~learnt:true ~lbd literals in
          bump_clause s id;
          enqueue s literals.(0) id;
          s.var_increment <- s.var_increment *. (1. /. 0.95);
          s.clause_increment <- s.clause_increment *. (1. /. 0.999);
          loop ()
      | None ->
        if (!restart_due || s.conflicts >= !restart_at) && s.limits.size > 0
        then (
          cancel_until s 0;
          s.restarts <- s.restarts + 1;
          recent_lbd_size := 0;
          recent_lbd_cursor := 0;
          recent_lbd_sum := 0;
          restart_due := false;
          incr restart_index;
          restart_at := s.conflicts + (1000 * luby !restart_index);
          loop ())
        else (
          if s.conflicts >= s.next_reduction then reduce_clauses s;
          let next = ref (-1) in
          let failed = ref (-1) in
          while
            !next < 0 && !failed < 0 && s.limits.size < Array.length assumptions
          do
            let index = s.limits.size in
            let lit = assumptions.(index) in
            match value s lit with
            | 1 -> push_level s
            | 0 -> failed := index
            | _ -> next := lit
          done;
          if !failed >= 0
          then Unsat (assumption_core s assumptions !failed)
          else (
            if !next < 0
            then
              while !next < 0 && s.heap.size > 0 do
                let v = heap_remove s in
                if s.values.(v) < 0
                then (
                  next := 2 * v lor (1 lxor s.phases.(v));
                  s.decisions <- s.decisions + 1)
              done;
            if !next < 0
            then
              let model () =
                Sat
                  (Array.init (s.variables + 1) (fun i ->
                       i > 0 && s.values.(i - 1) = 1))
              in
              match s.extension with
              | None -> model ()
              | Some ext -> (
                let variables = s.variables in
                let trail_size = s.trail.size in
                match with_callback s ext.final_check with
                | Incomplete -> Unknown
                | Continue -> loop ()
                | Done ->
                  if
                    variables <> s.variables || trail_size <> s.trail.size
                    || s.theory_conflict <> None
                    || not (Queue.is_empty s.pending_lemmas)
                  then loop ()
                  else model ())
            else (
              push_level s;
              enqueue s !next (-1);
              loop ())))
  in
  loop ()

let solve ?(assumptions = [||]) ?(max_conflicts = max_int)
    ?(should_stop = never_stop) s =
  check_idle s;
  if max_conflicts < 0
  then invalid_arg "Vox_sat_solver.solve: negative conflict budget";
  Array.iter (check_literal s) assumptions;
  let all_assumptions =
    Array.init
      (s.scopes.size + Array.length assumptions)
      (fun i ->
        if i < s.scopes.size
        then s.scopes.data.(i)
        else encode assumptions.(i - s.scopes.size))
  in
  s.level_seen
    <- grow s.level_seen (s.variables + Array.length all_assumptions + 1) 0;
  s.busy <- true;
  s.solves <- s.solves + 1;
  s.theory_head <- 0;
  s.theory_running <- Option.is_some s.extension;
  let outcome =
    try
      Option.iter (fun ext -> ext.start ()) s.extension;
      Ok (search s all_assumptions max_conflicts should_stop)
    with
    | Stopped -> Ok Unknown
    | exn -> Error exn
  in
  let cleanup_error = ref None in
  let cleanup f =
    try f ()
    with exn -> if !cleanup_error = None then cleanup_error := Some exn
  in
  cleanup (fun () -> cancel_until s 0);
  s.theory_running <- false;
  cleanup (fun () ->
      (* A newly installed conflicting unit has no watch to rediscover it
         after an interrupted solve. *)
      Option.iter
        (fun literals -> Queue.add (Array.copy literals) s.pending_lemmas)
        s.theory_conflict);
  cleanup (fun () -> Option.iter (fun ext -> ext.finish ()) s.extension);
  s.callback_active <- false;
  s.theory_conflict <- None;
  s.busy <- false;
  match outcome, !cleanup_error with
  | Error exn, _ | Ok _, Some exn -> raise exn
  | Ok result, None -> result

let statistics s =
  { solves = s.solves;
    decisions = s.decisions;
    propagations = s.propagations;
    conflicts = s.conflicts;
    restarts = s.restarts;
    learnt_clauses = s.learnt_clauses;
    deleted_clauses = s.deleted_clauses
  }

let reset_statistics s =
  check_idle s;
  s.solves <- 0;
  s.decisions <- 0;
  s.propagations <- 0;
  s.conflicts <- 0;
  s.restarts <- 0;
  s.learnt_clauses <- 0;
  s.deleted_clauses <- 0;
  s.next_reduction <- 2000

let clause_count t = t.clause_count + t.binary_clauses.size / 2

module Internal = struct
  let set_occurrence_activity s enabled =
    check_idle s;
    s.occurrence_activity <- enabled
end
