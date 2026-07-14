(* Dynamic relevancy driver for the SAT decision branch-filter (task #24). See
   relevancy.mli for the contract and logs/quf-propagation-log.md for the motivation +
   soundness argument.

   State is per-variable, grown on demand and indexed by SAT variable id:
   - [nodes.(v)] : [Leaf] (atom / selector / not-in-graph) or [Compound {kind; children}].
   - [gated.(v)] : [v] is a gated atom (the filter applies to it).
   - [relevant.(v)]: [v] is currently marked relevant.
   - [value.(v)] : current trail value (0 unassigned, 1 true, -1 false).
   - [parents.(v)]: compound variables that reference [v] as a child (so a child's value
     settling can re-trigger its parent disjunction's justifier choice).

   Marks and values are trailed with the decision level at which they were recorded, so
   [on_backtrack ~level] undoes exactly what lies above [level] (level-0 seeds persist).
   The relevancy propagation is a downward, monotonic-within-a-level marking over an
   acyclic term DAG, driven to a fixpoint by an explicit worklist — no recursion depth or
   wall-clock, so two identical runs mark identically (I6). *)

type kind =
  | KAnd
  | KOr
  | KIff
  | KIte

type node =
  | Leaf
  | Compound of
      { kind : kind
      ; children : (int * bool) array
      }

type t =
  { nodes : node Dynarray.t
  ; gated : bool Dynarray.t
  ; relevant : bool Dynarray.t
  ; value : int Dynarray.t
  ; parents : int list Dynarray.t
  ; relev_trail : int Dynarray.t (* vars marked relevant, in mark order *)
  ; relev_level : int Dynarray.t (* the level each was marked at (parallel) *)
  ; value_trail : int Dynarray.t (* vars assigned, in trail order *)
  ; value_level : int Dynarray.t (* the level each was assigned at (parallel) *)
  ; work : int Dynarray.t (* reused propagation worklist *)
  ; activity : int -> float
    (* read-only VSIDS activity of a SAT var (task #24 activity-based candidate selection):
     when a satisfied disjunction has no justifying child yet, the branch candidate it
     keeps relevant is the highest-activity unassigned child, aligning the forced decision
     with the solver's own order instead of an arbitrary lowest-var pick. Defaults to the
     constant 0.0 (=> pure lowest-var tie-break, the pre-experiment behaviour) when no
     accessor is supplied. *)
  }

let enabled_from_env () =
  match Sys.getenv_opt "OXSMT_RELEVANCY" with
  | Some ("1" | "true" | "yes" | "on") -> true
  | Some _ | None -> false
;;

let create ?(activity = fun _ -> 0.0) () =
  { nodes = Dynarray.create ()
  ; gated = Dynarray.create ()
  ; relevant = Dynarray.create ()
  ; value = Dynarray.create ()
  ; parents = Dynarray.create ()
  ; relev_trail = Dynarray.create ()
  ; relev_level = Dynarray.create ()
  ; value_trail = Dynarray.create ()
  ; value_level = Dynarray.create ()
  ; work = Dynarray.create ()
  ; activity
  }
;;

let ensure_var t var =
  while Dynarray.length t.nodes <= var do
    Dynarray.add_last t.nodes Leaf;
    Dynarray.add_last t.gated false;
    Dynarray.add_last t.relevant false;
    Dynarray.add_last t.value 0;
    Dynarray.add_last t.parents []
  done
;;

let register_atom t v =
  ensure_var t v;
  Dynarray.set t.gated v true
;;

let register_node t ~var ~kind ~children =
  ensure_var t var;
  List.iter (fun (cv, _) -> ensure_var t cv) children;
  Dynarray.set t.nodes var (Compound { kind; children = Array.of_list children });
  List.iter
    (fun (cv, _) -> Dynarray.set t.parents cv (var :: Dynarray.get t.parents cv))
    children
;;

(* Child-expression truth: [(cv, positive)] denotes [value cv] when [positive], its
   negation otherwise. [justifies] asks whether that expression currently holds the value
   [want_true] (a true disjunct justifies an Or; a false conjunct justifies an And). *)
let justifies t (cv, positive) ~want_true =
  let x = Dynarray.get t.value cv in
  x <> 0 && Bool.equal (Bool.equal (x = 1) positive) want_true
;;

(* Mark [v] relevant at [level] and enqueue it for downward propagation. Monotonic: a
   second mark at any level is a no-op, so the worklist terminates over the acyclic DAG. *)
let rec mark t v level =
  if not (Dynarray.get t.relevant v)
  then (
    Dynarray.set t.relevant v true;
    Dynarray.add_last t.relev_trail v;
    Dynarray.add_last t.relev_level level;
    Dynarray.add_last t.work v)

and mark_all t children level = Array.iter (fun (cv, _) -> mark t cv level) children

(* Choose the justifying child of a satisfied disjunction (Or true, [want_true]) or a
   falsified conjunction (And false, [want_true = false]): the connective's truth is
   pinned down by ONE child (a true disjunct / a false conjunct), so only that child need
   be relevant — the siblings are the ones we want the branch filter to skip.

   Because the Tseitin encoding can force the connective variable true before any child
   settles (e.g. from a root unit under a frame selector), the choice is z3's watched
   scheme, marking at most one child at a time:
   - if a relevant child already justifies, the connective is accounted for — done;
   - else if some child already justifies, mark the lowest-var such child (now relevant);
   - else (no child justifies yet) keep exactly ONE unassigned child branchable as a
     candidate: if one is already relevant, wait for it; otherwise mark the
     HIGHEST-ACTIVITY unassigned child (lowest-var tie-break) so the forced decision
     follows the solver's own VSIDS order. When the candidate settles, this re-fires (via
     the parent index) and either finds it justifying or advances to the next candidate.
     With no unassigned child left (every child settled the wrong way) nothing is marked —
     the Tseitin clause is then falsified and the SAT core takes the conflict, so the
     search never stalls with an unsatisfiable clause whose literals are all filtered out. *)
and pick_justifier t children level ~want_true =
  let satisfied =
    Array.exists
      (fun ((cv, _) as ch) -> Dynarray.get t.relevant cv && justifies t ch ~want_true)
      children
  in
  if not satisfied
  then (
    let best_just = ref (-1) in
    Array.iter
      (fun ((cv, _) as ch) ->
         if justifies t ch ~want_true && (!best_just = -1 || cv < !best_just)
         then best_just := cv)
      children;
    if !best_just >= 0
    then mark t !best_just level
    else (
      let has_pending =
        Array.exists
          (fun (cv, _) -> Dynarray.get t.relevant cv && Dynarray.get t.value cv = 0)
          children
      in
      if not has_pending
      then (
        (* keep the HIGHEST-ACTIVITY unassigned child branchable (tie-break: lowest var),
           so the decision the filter forces here is the one the solver's own VSIDS order
           would prefer rather than an arbitrary lowest-var pick fighting it. *)
        let best_cand = ref (-1) in
        let best_act = ref neg_infinity in
        Array.iter
          (fun (cv, _) ->
             if Dynarray.get t.value cv = 0
             then (
               let a = t.activity cv in
               if !best_cand = -1 || a > !best_act || (a = !best_act && cv < !best_cand)
               then (
                 best_cand := cv;
                 best_act := a)))
          children;
        if !best_cand >= 0 then mark t !best_cand level)))

and propagate t p level =
  match Dynarray.get t.nodes p with
  | Leaf -> ()
  | Compound { kind; children } ->
    if Dynarray.get t.relevant p && Dynarray.get t.value p <> 0
    then (
      let this_true = Dynarray.get t.value p = 1 in
      match kind with
      | KIff -> mark_all t children level (* both sides needed to evaluate the iff *)
      | KAnd ->
        if this_true
        then mark_all t children level
        else pick_justifier t children level ~want_true:false
      | KOr ->
        if this_true
        then pick_justifier t children level ~want_true:true
        else mark_all t children level
      | KIte ->
        let cond_var, cond_pos = children.(0) in
        mark t cond_var level;
        let x = Dynarray.get t.value cond_var in
        if x <> 0
        then (
          let taken =
            if Bool.equal (x = 1) cond_pos then children.(1) else children.(2)
          in
          mark t (fst taken) level))

and run t level =
  while Dynarray.length t.work > 0 do
    let n = Dynarray.length t.work in
    let p = Dynarray.get t.work (n - 1) in
    Dynarray.truncate t.work (n - 1);
    propagate t p level
  done
;;

let seed_root t v =
  ensure_var t v;
  mark t v 0;
  run t 0
;;

let on_assign t ~var ~value ~level =
  ensure_var t var;
  Dynarray.set t.value var (if value then 1 else -1);
  Dynarray.add_last t.value_trail var;
  Dynarray.add_last t.value_level level;
  (* Self: a relevant compound whose value just settled propagates down. *)
  Dynarray.add_last t.work var;
  (* Parents: a relevant Or/And/Ite whose child [var] just settled may now pick [var] as
     its justifier / taken branch. *)
  List.iter (fun p -> Dynarray.add_last t.work p) (Dynarray.get t.parents var);
  run t level
;;

let on_backtrack t ~level =
  let unwind trail levels reset =
    let n = ref (Dynarray.length trail) in
    while !n > 0 && Dynarray.get levels (!n - 1) > level do
      reset (Dynarray.get trail (!n - 1));
      decr n
    done;
    Dynarray.truncate trail !n;
    Dynarray.truncate levels !n
  in
  unwind t.relev_trail t.relev_level (fun v -> Dynarray.set t.relevant v false);
  unwind t.value_trail t.value_level (fun v -> Dynarray.set t.value v 0)
;;

let should_branch t v =
  v >= Dynarray.length t.gated
  || (not (Dynarray.get t.gated v))
  || Dynarray.get t.relevant v
;;
