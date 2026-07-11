(* Propositional CDCL SAT core, MiniSat design (Eén–Sörensson 2003). See sat.mli for the
   contract. Stdlib-only (dependency firewall I3): the only non-Stdlib data structure used
   is [Dynarray] (OCaml 5.2+ standard library).

   Novelty-free by intent (DESIGN.md §5). The structure mirrors MiniSat: a clause arena
   with two watched literals, a trail with per-level boundaries, VSIDS activity in a
   binary heap, phase saving, Luby restarts, and activity-based [reduceDB]. Determinism
   (I6): no wall-clock, no randomness; every schedule is count-based and every tie is
   broken by variable index via the heap. *)

type var = int
type lit = int

(* Literal encoding: [2*v] positive, [2*v+1] negative (MiniSat). Even = positive. This
   lets watch lists be indexed directly by literal. *)
let pos v = 2 * v
let neg v = (2 * v) + 1
let neg_lit l = l lxor 1
let var_of_lit l = l lsr 1
let sign_of_lit l = l land 1 = 0

type result =
  | Sat
  | Unsat

(* A clause in the arena. [lits.(0)] and [lits.(1)] are the two watched literals;
   propagation swaps array slots to move a watch. [id] is stable and unique
   (proof-readiness, §7). [learnt] clauses are subject to [reduce_db]; original clauses
   are never deleted. [deleted] is set by [reduce_db] and swept lazily out of watch lists
   during propagation. *)
type clause =
  { id : int
  ; lits : lit array
  ; mutable activity : float
  ; learnt : bool
  ; mutable deleted : bool
  }

(* A watch: the clause plus a cached "blocker" literal (the other watched literal). If the
   blocker is already true the clause is satisfied and needs no inspection — the standard
   MiniSat fast path. *)
type watch =
  { cl : clause
  ; blocker : lit
  }

type trace =
  { on_learned : id:int -> clause:lit array -> antecedents:int list -> btlevel:int -> unit
  }

type t =
  { mutable nvars : int
  ; mutable ok : bool (* false once an empty clause is derived: permanently unsat *)
  ; (* Per-variable state, indexed by var. *)
    assigns : int Dynarray.t (* 0 unknown, 1 true, -1 false *)
  ; level : int Dynarray.t (* decision level at which the var was assigned *)
  ; reason : clause option Dynarray.t (* implying clause, or None for a decision *)
  ; polarity : bool Dynarray.t (* saved phase: true => decide negative first *)
  ; seen : bool Dynarray.t (* scratch flag for conflict analysis *)
  ; (* Per-variable VSIDS activity and its max-heap (top = highest activity). *)
    var_act : float Dynarray.t
  ; heap : int Dynarray.t (* heap of vars *)
  ; heap_pos : int Dynarray.t (* var -> index in [heap], or -1 if absent *)
  ; (* Watch lists indexed by literal (length [2 * nvars]). *)
    watches : watch Dynarray.t Dynarray.t
  ; (* The assignment trail and its per-decision-level boundaries. *)
    trail : lit Dynarray.t
  ; trail_lim : int Dynarray.t
  ; mutable qhead : int (* propagation cursor into [trail] *)
  ; (* Clause storage. *)
    clauses : clause Dynarray.t
  ; learnts : clause Dynarray.t
  ; mutable next_id : int
  ; (* Activity increments and decay factors. *)
    mutable var_inc : float
  ; mutable cla_inc : float
  ; (* Learned-clause budget (grows across restarts). *)
    mutable max_learnts : float
  ; (* The model snapshot of the most recent Sat, and the last failed core. *)
    saved_model : int Dynarray.t
  ; mutable failed : lit list
  ; (* Cumulative stats (I6: exact, deterministic). *)
    mutable conflicts : int
  ; mutable decisions : int
  ; mutable propagations : int
  ; restart_base : int
  ; mutable trace : trace option
  }

let var_decay = 0.95
let cla_decay = 0.999

let create () =
  { nvars = 0
  ; ok = true
  ; assigns = Dynarray.create ()
  ; level = Dynarray.create ()
  ; reason = Dynarray.create ()
  ; polarity = Dynarray.create ()
  ; seen = Dynarray.create ()
  ; var_act = Dynarray.create ()
  ; heap = Dynarray.create ()
  ; heap_pos = Dynarray.create ()
  ; watches = Dynarray.create ()
  ; trail = Dynarray.create ()
  ; trail_lim = Dynarray.create ()
  ; qhead = 0
  ; clauses = Dynarray.create ()
  ; learnts = Dynarray.create ()
  ; next_id = 0
  ; var_inc = 1.0
  ; cla_inc = 1.0
  ; max_learnts = 0.0
  ; saved_model = Dynarray.create ()
  ; failed = []
  ; conflicts = 0
  ; decisions = 0
  ; propagations = 0
  ; restart_base = 100
  ; trace = None
  }
;;

let set_trace t tr = t.trace <- tr
let num_vars t = t.nvars
let decision_level t = Dynarray.length t.trail_lim

(* Signed value of a literal under the current assignment: 1 true, -1 false, 0 unknown. *)
let lit_val t l =
  let v = Dynarray.get t.assigns (var_of_lit l) in
  if v = 0 then 0 else if sign_of_lit l then v else -v
;;

(* ------------------------------------------------------------------ *)
(* VSIDS variable-activity max-heap. Ordered by activity; ties by nothing explicit, so the
   heap's shape (a function of insertion/removal order) is the deterministic tiebreak. *)

let heap_lt t a b = Dynarray.get t.var_act a > Dynarray.get t.var_act b

let heap_up t i =
  let x = Dynarray.get t.heap i in
  let i = ref i in
  let continue = ref true in
  while !i > 0 && !continue do
    let parent = (!i - 1) / 2 in
    let px = Dynarray.get t.heap parent in
    if heap_lt t x px
    then (
      Dynarray.set t.heap !i px;
      Dynarray.set t.heap_pos px !i;
      i := parent)
    else continue := false
  done;
  Dynarray.set t.heap !i x;
  Dynarray.set t.heap_pos x !i
;;

let heap_down t i =
  let x = Dynarray.get t.heap i in
  let n = Dynarray.length t.heap in
  let i = ref i in
  let continue = ref true in
  while !continue do
    let l = (2 * !i) + 1 in
    if l >= n
    then continue := false
    else (
      let r = l + 1 in
      let child =
        if r < n && heap_lt t (Dynarray.get t.heap r) (Dynarray.get t.heap l)
        then r
        else l
      in
      let cx = Dynarray.get t.heap child in
      if heap_lt t cx x
      then (
        Dynarray.set t.heap !i cx;
        Dynarray.set t.heap_pos cx !i;
        i := child)
      else continue := false)
  done;
  Dynarray.set t.heap !i x;
  Dynarray.set t.heap_pos x !i
;;

let heap_insert t v =
  if Dynarray.get t.heap_pos v < 0
  then (
    let i = Dynarray.length t.heap in
    Dynarray.add_last t.heap v;
    Dynarray.set t.heap_pos v i;
    heap_up t i)
;;

let heap_remove_max t =
  if Dynarray.length t.heap = 0
  then None
  else (
    let top = Dynarray.get t.heap 0 in
    let last_i = Dynarray.length t.heap - 1 in
    let last = Dynarray.get t.heap last_i in
    Dynarray.set t.heap 0 last;
    Dynarray.set t.heap_pos last 0;
    Dynarray.set t.heap_pos top (-1);
    Dynarray.remove_last t.heap;
    if Dynarray.length t.heap > 0 then heap_down t 0;
    Some top)
;;

(* ------------------------------------------------------------------ *)
(* Variable / clause activity bumping with periodic rescale. *)

let var_bump t v =
  let a = Dynarray.get t.var_act v +. t.var_inc in
  Dynarray.set t.var_act v a;
  if a > 1e100
  then (
    for w = 0 to t.nvars - 1 do
      Dynarray.set t.var_act w (Dynarray.get t.var_act w *. 1e-100)
    done;
    t.var_inc <- t.var_inc *. 1e-100);
  let hp = Dynarray.get t.heap_pos v in
  if hp >= 0 then heap_up t hp
;;

let var_decay_bump t = t.var_inc <- t.var_inc /. var_decay

let cla_bump t c =
  c.activity <- c.activity +. t.cla_inc;
  if c.activity > 1e20
  then (
    Dynarray.iter (fun c -> c.activity <- c.activity *. 1e-20) t.learnts;
    t.cla_inc <- t.cla_inc *. 1e-20)
;;

let cla_decay_bump t = t.cla_inc <- t.cla_inc /. cla_decay

(* ------------------------------------------------------------------ *)
(* Variable allocation. Grows every per-var Dynarray and the two watch lists for the new
   var's literals, and makes the var decision-eligible. *)

let ensure_var t v =
  while t.nvars <= v do
    Dynarray.add_last t.assigns 0;
    Dynarray.add_last t.level 0;
    Dynarray.add_last t.reason None;
    Dynarray.add_last t.polarity true;
    Dynarray.add_last t.seen false;
    Dynarray.add_last t.var_act 0.0;
    Dynarray.add_last t.heap_pos (-1);
    Dynarray.add_last t.watches (Dynarray.create ());
    Dynarray.add_last t.watches (Dynarray.create ());
    let nv = t.nvars in
    t.nvars <- t.nvars + 1;
    heap_insert t nv
  done
;;

let new_var t =
  let v = t.nvars in
  ensure_var t v;
  v
;;

let fresh_id t =
  let id = t.next_id in
  t.next_id <- t.next_id + 1;
  id
;;

let mk_clause t lits learnt =
  let c =
    { id = fresh_id t; lits = Array.copy lits; activity = 0.0; learnt; deleted = false }
  in
  if learnt then Dynarray.add_last t.learnts c else Dynarray.add_last t.clauses c;
  c
;;

let attach t c =
  let l0 = c.lits.(0)
  and l1 = c.lits.(1) in
  Dynarray.add_last (Dynarray.get t.watches (neg_lit l0)) { cl = c; blocker = l1 };
  Dynarray.add_last (Dynarray.get t.watches (neg_lit l1)) { cl = c; blocker = l0 }
;;

(* ------------------------------------------------------------------ *)
(* Trail. *)

let new_decision_level t = Dynarray.add_last t.trail_lim (Dynarray.length t.trail)

let unchecked_enqueue t lit reason =
  let v = var_of_lit lit in
  Dynarray.set t.assigns v (if sign_of_lit lit then 1 else -1);
  Dynarray.set t.level v (decision_level t);
  Dynarray.set t.reason v reason;
  Dynarray.add_last t.trail lit
;;

(* Undo assignments back to [level] (0-based decision level to keep). Saves the phase of
   every unassigned var (phase saving) and returns it to the heap. *)
let cancel_until t level =
  if decision_level t > level
  then (
    let target = Dynarray.get t.trail_lim level in
    for i = Dynarray.length t.trail - 1 downto target do
      let l = Dynarray.get t.trail i in
      let v = var_of_lit l in
      Dynarray.set t.polarity v (Dynarray.get t.assigns v = -1);
      Dynarray.set t.assigns v 0;
      Dynarray.set t.reason v None;
      heap_insert t v
    done;
    Dynarray.truncate t.trail target;
    Dynarray.truncate t.trail_lim level;
    t.qhead <- target)
;;

(* ------------------------------------------------------------------ *)
(* Two-watched-literal propagation. Returns the conflicting clause, if any. *)

let propagate t =
  let confl = ref None in
  while !confl = None && t.qhead < Dynarray.length t.trail do
    let p = Dynarray.get t.trail t.qhead in
    t.qhead <- t.qhead + 1;
    t.propagations <- t.propagations + 1;
    (* Clauses in [watches.(p)] watch [neg_lit p], which is now false. *)
    let ws = Dynarray.get t.watches p in
    let n = Dynarray.length ws in
    let i = ref 0
    and j = ref 0 in
    let false_lit = neg_lit p in
    while !i < n do
      let w = Dynarray.get ws !i in
      if lit_val t w.blocker = 1
      then (
        (* Clause already satisfied by its blocker; keep the watch untouched. *)
        Dynarray.set ws !j w;
        incr i;
        incr j)
      else (
        let c = w.cl in
        if c.deleted
        then incr i (* sweep deleted clause out of the watch list *)
        else (
          (* Ensure the false literal is at slot 1, its partner at slot 0. *)
          if c.lits.(0) = false_lit
          then (
            c.lits.(0) <- c.lits.(1);
            c.lits.(1) <- false_lit);
          let first = c.lits.(0) in
          let w' = { cl = c; blocker = first } in
          if first <> w.blocker && lit_val t first = 1
          then (
            (* Newly satisfied by the partner watch. *)
            Dynarray.set ws !j w';
            incr i;
            incr j)
          else (
            (* Look for a non-false literal to watch instead of [false_lit]. *)
            let len = Array.length c.lits in
            let k = ref 2 in
            let found = ref false in
            while (not !found) && !k < len do
              if lit_val t c.lits.(!k) <> -1 then found := true else incr k
            done;
            if !found
            then (
              let lk = c.lits.(!k) in
              c.lits.(1) <- lk;
              c.lits.(!k) <- false_lit;
              Dynarray.add_last (Dynarray.get t.watches (neg_lit lk)) w';
              incr i (* drop from this watch list; now watched elsewhere *))
            else (
              (* No new watch: the clause is unit or conflicting. Keep the watch. *)
              Dynarray.set ws !j w';
              incr i;
              incr j;
              if lit_val t first = -1
              then (
                confl := Some c;
                (* copy the tail of the watch list unchanged *)
                while !i < n do
                  Dynarray.set ws !j (Dynarray.get ws !i);
                  incr i;
                  incr j
                done)
              else unchecked_enqueue t first (Some c)))))
    done;
    Dynarray.truncate ws !j
  done;
  !confl
;;

(* ------------------------------------------------------------------ *)
(* 1UIP conflict analysis with local (self-subsumption) minimization.

   Returns the learned clause (asserting literal at index 0), the backjump level, and —
   only when a trace is set — the antecedent clause ids of the resolution derivation. *)

let analyze t confl =
  let out = Dynarray.create () in
  Dynarray.add_last out 0 (* placeholder for the asserting literal *);
  let track = t.trace <> None in
  let ants = ref (if track then [ confl.id ] else []) in
  (* Vars marked [seen] during analysis, to clear at the end. *)
  let marked = Dynarray.create () in
  let mark v =
    if not (Dynarray.get t.seen v)
    then (
      Dynarray.set t.seen v true;
      Dynarray.add_last marked v)
  in
  let path_c = ref 0 in
  let p = ref (-1) in
  let index = ref (Dynarray.length t.trail - 1) in
  let c = ref confl in
  let dl = decision_level t in
  let continue = ref true in
  while !continue do
    if !c.learnt then cla_bump t !c;
    let lits = !c.lits in
    let start = if !p = -1 then 0 else 1 in
    for jj = start to Array.length lits - 1 do
      let q = lits.(jj) in
      let vq = var_of_lit q in
      if (not (Dynarray.get t.seen vq)) && Dynarray.get t.level vq > 0
      then (
        var_bump t vq;
        mark vq;
        if Dynarray.get t.level vq >= dl then incr path_c else Dynarray.add_last out q)
    done;
    (* Next literal to resolve on: the most recent seen literal on the trail. *)
    while not (Dynarray.get t.seen (var_of_lit (Dynarray.get t.trail !index))) do
      decr index
    done;
    let pl = Dynarray.get t.trail !index in
    decr index;
    p := pl;
    let vp = var_of_lit pl in
    Dynarray.set t.seen vp false;
    decr path_c;
    if !path_c <= 0
    then continue := false
    else (
      (c
       := match Dynarray.get t.reason vp with
          | Some cc -> cc
          | None -> assert false);
      if track then ants := !c.id :: !ants)
  done;
  Dynarray.set out 0 (neg_lit !p);
  (* Local minimization: drop a literal whose reason's other literals are all already in
     the learned clause (marked) or fixed at level 0. *)
  let len = Dynarray.length out in
  let jw = ref 1 in
  for i = 1 to len - 1 do
    let l = Dynarray.get out i in
    let v = var_of_lit l in
    let redundant =
      match Dynarray.get t.reason v with
      | None -> false (* a decision literal is never redundant *)
      | Some rc ->
        let rlits = rc.lits in
        let ok = ref true in
        let k = ref 1 in
        while !ok && !k < Array.length rlits do
          let vk = var_of_lit rlits.(!k) in
          if (not (Dynarray.get t.seen vk)) && Dynarray.get t.level vk > 0
          then ok := false;
          incr k
        done;
        !ok
    in
    if not redundant
    then (
      Dynarray.set out !jw l;
      incr jw)
  done;
  Dynarray.truncate out !jw;
  (* Backjump level: the second-highest decision level in the clause (0 for a unit). Move
     that literal to slot 1 so the learned clause watches it. *)
  let learnt = Dynarray.to_array out in
  let bt =
    if Array.length learnt = 1
    then 0
    else (
      let maxi = ref 1 in
      for i = 2 to Array.length learnt - 1 do
        if
          Dynarray.get t.level (var_of_lit learnt.(i))
          > Dynarray.get t.level (var_of_lit learnt.(!maxi))
        then maxi := i
      done;
      let tmp = learnt.(1) in
      learnt.(1) <- learnt.(!maxi);
      learnt.(!maxi) <- tmp;
      Dynarray.get t.level (var_of_lit learnt.(1)))
  in
  Dynarray.iter (fun v -> Dynarray.set t.seen v false) marked;
  learnt, bt, List.rev !ants
;;

(* Which assumption literals forced [p] to be true (i.e. the failed-assumption core, §7).
   [p] is the negation of a false assumption. *)
let analyze_final t p =
  let out = Dynarray.create () in
  Dynarray.add_last out p;
  if decision_level t > 0
  then (
    let marked = Dynarray.create () in
    let mark v =
      if not (Dynarray.get t.seen v)
      then (
        Dynarray.set t.seen v true;
        Dynarray.add_last marked v)
    in
    mark (var_of_lit p);
    let start = Dynarray.get t.trail_lim 0 in
    for i = Dynarray.length t.trail - 1 downto start do
      let l = Dynarray.get t.trail i in
      let v = var_of_lit l in
      if Dynarray.get t.seen v
      then (
        match Dynarray.get t.reason v with
        | None -> Dynarray.add_last out (neg_lit l)
        | Some c ->
          let lits = c.lits in
          for j = 1 to Array.length lits - 1 do
            let vj = var_of_lit lits.(j) in
            if Dynarray.get t.level vj > 0 then mark vj
          done)
    done;
    Dynarray.iter (fun v -> Dynarray.set t.seen v false) marked);
  List.map neg_lit (Array.to_list (Dynarray.to_array out))
;;

(* ------------------------------------------------------------------ *)
(* Learned-clause deletion. A clause is locked if it is the reason for its currently-true
   asserting literal. We drop roughly the least-active half of the unlocked, non-binary
   learned clauses. Deleted clauses are swept out of watch lists lazily during
   propagation. *)

let locked t c =
  let l0 = c.lits.(0) in
  lit_val t l0 = 1
  &&
  match Dynarray.get t.reason (var_of_lit l0) with
  | Some rc -> rc == c
  | None -> false
;;

let reduce_db t =
  let arr = Dynarray.to_array t.learnts in
  Array.sort (fun a b -> compare a.activity b.activity) arr;
  let n = Array.length arr in
  for i = 0 to n - 1 do
    let c = arr.(i) in
    if (not (locked t c)) && Array.length c.lits > 2 && i < n / 2 then c.deleted <- true
  done;
  Dynarray.clear t.learnts;
  Array.iter (fun c -> if not c.deleted then Dynarray.add_last t.learnts c) arr
;;

(* ------------------------------------------------------------------ *)
(* Permanent clause addition with level-0 simplification. Only legal at decision level 0
   (guaranteed by [solve], which cancels to 0 before returning). *)

let add_clause t lits =
  List.iter (fun l -> ensure_var t (var_of_lit l)) lits;
  if t.ok
  then (
    let ls = List.sort_uniq compare lits in
    let tautology = List.exists (fun l -> List.mem (neg_lit l) ls) ls in
    if not tautology
    then (
      let already_true = List.exists (fun l -> lit_val t l = 1) ls in
      if not already_true
      then (
        let ls = List.filter (fun l -> lit_val t l <> -1) ls in
        match ls with
        | [] -> t.ok <- false
        | [ l ] -> if lit_val t l = 0 then unchecked_enqueue t l None
        | _ ->
          let c = mk_clause t (Array.of_list ls) false in
          attach t c)))
;;

(* ------------------------------------------------------------------ *)
(* Branching and search. *)

let rec pick_branch t =
  match heap_remove_max t with
  | None -> None
  | Some v ->
    if Dynarray.get t.assigns v = 0
    then Some (if Dynarray.get t.polarity v then neg v else pos v)
    else pick_branch t
;;

let save_model t =
  Dynarray.clear t.saved_model;
  for v = 0 to t.nvars - 1 do
    Dynarray.add_last t.saved_model (Dynarray.get t.assigns v)
  done
;;

let record_learnt t learnt bt ants =
  if Array.length learnt = 1
  then (
    unchecked_enqueue t learnt.(0) None;
    match t.trace with
    | Some tr ->
      tr.on_learned ~id:(fresh_id t) ~clause:learnt ~antecedents:ants ~btlevel:bt
    | None -> ())
  else (
    let c = mk_clause t learnt true in
    attach t c;
    cla_bump t c;
    unchecked_enqueue t learnt.(0) (Some c);
    match t.trace with
    | Some tr -> tr.on_learned ~id:c.id ~clause:learnt ~antecedents:ants ~btlevel:bt
    | None -> ())
;;

type search_result =
  | R_sat
  | R_unsat
  | R_restart

(* One search episode, bounded by [conflict_limit] conflicts (0 = unbounded). *)
let search t assumps conflict_limit =
  let result = ref None in
  let conflicts_here = ref 0 in
  while !result = None do
    match propagate t with
    | Some confl ->
      t.conflicts <- t.conflicts + 1;
      incr conflicts_here;
      if decision_level t = 0
      then (
        t.ok <- false;
        result := Some R_unsat)
      else (
        let learnt, bt, ants = analyze t confl in
        cancel_until t bt;
        record_learnt t learnt bt ants;
        var_decay_bump t;
        cla_decay_bump t;
        if
          float_of_int (Dynarray.length t.learnts - Dynarray.length t.trail)
          >= t.max_learnts
        then reduce_db t)
    | None ->
      if conflict_limit > 0 && !conflicts_here >= conflict_limit
      then (
        cancel_until t 0;
        result := Some R_restart)
      else (
        (* Advance through assumptions, then branch. *)
        let next = ref (-1) in
        let brk = ref false in
        while (not !brk) && decision_level t < Array.length assumps do
          let pa = assumps.(decision_level t) in
          match lit_val t pa with
          | 1 -> new_decision_level t (* dummy level: assumption already true *)
          | -1 ->
            t.failed <- analyze_final t (neg_lit pa);
            result := Some R_unsat;
            brk := true
          | _ ->
            next := pa;
            brk := true
        done;
        if !result = None
        then
          if !next = -1
          then (
            match pick_branch t with
            | None ->
              save_model t;
              result := Some R_sat
            | Some l ->
              t.decisions <- t.decisions + 1;
              new_decision_level t;
              unchecked_enqueue t l None)
          else (
            t.decisions <- t.decisions + 1;
            new_decision_level t;
            unchecked_enqueue t !next None))
  done;
  match !result with
  | Some r -> r
  | None -> assert false
;;

(* Luby restart sequence (unit multiples), MiniSat's finite-subsequence form. *)
let luby restart_no =
  let size = ref 1
  and seq = ref 0 in
  while !size < restart_no + 1 do
    incr seq;
    size := (2 * !size) + 1
  done;
  let x = ref restart_no in
  while !size - 1 <> !x do
    size := (!size - 1) / 2;
    decr seq;
    x := !x mod !size
  done;
  1 lsl !seq
;;

let solve ?(assumptions = []) t =
  t.failed <- [];
  List.iter (fun l -> ensure_var t (var_of_lit l)) assumptions;
  if not t.ok
  then Unsat
  else (
    let assumps = Array.of_list assumptions in
    cancel_until t 0;
    t.max_learnts <- Float.max 100.0 (float_of_int (Dynarray.length t.clauses) /. 3.0);
    let rec go restart_no =
      let lim = luby restart_no * t.restart_base in
      match search t assumps lim with
      | R_restart ->
        t.max_learnts <- t.max_learnts *. 1.1;
        go (restart_no + 1)
      | R_sat -> Sat
      | R_unsat -> Unsat
    in
    let r = go 0 in
    cancel_until t 0;
    r)
;;

(* ------------------------------------------------------------------ *)
(* Model and stats accessors. *)

let value t v = v < Dynarray.length t.saved_model && Dynarray.get t.saved_model v = 1
let model t = Array.init (Dynarray.length t.saved_model) (fun v -> value t v)
let failed_assumptions t = t.failed

module Stats = struct
  type t =
    { conflicts : int
    ; decisions : int
    ; propagations : int
    }
end

let stats t =
  { Stats.conflicts = t.conflicts
  ; decisions = t.decisions
  ; propagations = t.propagations
  }
;;
