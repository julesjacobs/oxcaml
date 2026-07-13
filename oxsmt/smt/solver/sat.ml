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
  ; mutable lbd : int
    (* LBD ("glue"): distinct decision levels among the literals, computed when the
         clause is learned and improved (lowered) whenever conflict analysis re-derives it
         as a reason. Drives LBD-based reduceDB (S3). 0 for original / transient clauses
         (never in [learnts], never scored). *)
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

(* Why a variable is assigned. [Decision] is a branch choice or a level-0 unit (reason
   [None] in the pre-seam core). [Implied_by c] is Boolean unit propagation on clause [c].
   [Theory_prop] marks a literal enqueued by a plugged theory (ADR-0005 §3 T_consistent):
   its reason clause is NOT stored — it is reconstructed lazily via [theory.explain] only
   if conflict analysis resolves on it (CONTRACT-EX). With no theory plugged, only
   [Decision]/[Implied_by] occur, isomorphic to the original [clause option]. *)
type reason =
  | Decision
  | Implied_by of clause
  | Theory_prop

(* Certificate provenance / trace types (ADR-0013 §4.0). These are the frozen
   cert-emission seam (sat.mli, Tranche C). The emission bodies land later as [sat.ml]
   internals; the record here is only DESTRUCTURED (never constructed) by the core —
   clients build it. *)
type origin =
  | Query
  | Theory_lemma

type theory_clause_role =
  | Reason
  | Conflict

type unsat_conclusion =
  | Root_empty of { input_id : int }
  | Level0_conflict of { conflict_id : int }
  | Failed_assumption of { antecedents : int list }

type trace =
  { on_input : id:int -> clause:lit array -> origin:origin -> unit
  ; on_unit : id:int -> lit:lit -> unit
  ; on_learned : id:int -> clause:lit array -> antecedents:int list -> btlevel:int -> unit
  ; on_theory_clause : id:int -> clause:lit array -> role:theory_clause_role -> unit
  ; on_unsat : unsat_conclusion -> unit
  }

(* ADR-0005 §3 CDCL(T) theory-callback seam. Modeled on {!trace}: a settable record,
   [None] by default (pure propositional core; one branch of overhead when unset). See
   sat.mli for the contract. *)
type theory_result =
  | T_consistent of lit list
  | T_conflict of lit list
  | T_lemma of lit list list

type theory =
  { on_assign : lit -> unit
  ; on_backtrack : level:int -> unit
  ; check : final:bool -> theory_result
  ; explain : lit -> lit list
  }

(* Raised when a plugged theory violates a seam soundness contract the core cannot
   otherwise uphold — a non-falsified conflict clause, or an [explain] premise that is not
   asserted strictly before the literal it explains (CONTRACT-EX). Unconditional (not an
   [assert], which -noassert would drop): learning from a corrupt explanation is a
   soundness break, so we fail loudly. The engine's CONTRACT-POISON catch degrades the
   query to [unknown]. *)
exception Theory_contract_violation of string

type t =
  { mutable nvars : int
  ; mutable ok : bool (* false once an empty clause is derived: permanently unsat *)
  ; (* Per-variable state, indexed by var. *)
    assigns : int Dynarray.t (* 0 unknown, 1 true, -1 false *)
  ; level : int Dynarray.t (* decision level at which the var was assigned *)
  ; trail_pos : int Dynarray.t
    (* var -> its index in [trail] while assigned, else -1. Read only for the theory
         seam's strict CONTRACT-EX precedence check; write-only otherwise. *)
  ; reason :
      reason Dynarray.t (* why the var was assigned (Decision/Implied_by/Theory_prop) *)
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
  ; (* LBD-based reduceDB schedule (S3, Glucose-style): fire reduceDB when [conflicts]
       reaches [next_reduce], then step it by a fixed increment — decoupled from restarts
       (which are now frequent under the adaptive policy). *)
    mutable next_reduce : int
  ; (* Glucose-style adaptive restart / CaDiCaL-style rephasing state (S3 + #155). Fast
       and slow exponential moving averages of learned-clause LBD: a restart fires when
       recent LBD (fast) runs worse than the long-run average (slow). [trail_ema] is the
       moving average of the trail length at conflict, used to BLOCK restarts/rephasing
       while the trail is large (progress toward a model), which is what keeps rephasing
       from disrupting the SAT instances it would otherwise regress. All reset per
       [solve]. *)
    mutable lbd_ema_fast : float
  ; mutable lbd_ema_slow : float
  ; mutable trail_ema : float
  ; mutable conflicts_since_restart : int
  ; mutable conflicts_at_solve_start : int
    (* [t.conflicts] snapshot at the current [solve]'s entry. The restart/reduceDB
         warm-up and the blocking gate read [t.conflicts - conflicts_at_solve_start] — a
         PER-SOLVE conflict count — so an incremental re-solve neither leaks the
         cumulative count into the blocking gate (codex M2) nor fires reduceDB immediately
         against it (codex M3). *)
  ; (* Rephasing (#155): [decisions_since_rephase] drives a conflict-INDEPENDENT interval
       (so it fires on the firehose, where conflicts≈0), [rephase_events] indexes the
       [{saved, flipped, default, best}] cycle, [rephase_interval] grows to back off.
       [best_phase] holds the per-var phase of the longest trail prefix seen; the phase
       cycle's [Best_trail] mode replays it. *)
    mutable decisions_since_rephase : int
  ; mutable rephase_events : int
  ; mutable rephase_interval : int
  ; mutable best_trail_len : int
  ; best_phase : bool Dynarray.t
  ; (* The model snapshot of the most recent Sat, and the last failed core. *)
    saved_model : int Dynarray.t
  ; mutable failed : lit list
  ; (* Cumulative stats (I6: exact, deterministic). *)
    mutable conflicts : int
  ; mutable decisions : int
  ; mutable propagations : int
  ; restart_base : int
  ; mutable trace : trace option
  ; mutable terminal : unsat_conclusion option
    (* ADR-0013 §4.0: the terminal conclusion of a PERMANENT unsat ([t.ok] false) —
         [Root_empty] for an empty [Query]/[Theory_lemma] input (E1/E4), [Level0_conflict]
         for a level-0 conflict (E2). Set (traced only) at the [t.ok] true→false
         transition and PERSISTED across solves, so every repeated [solve] that returns
         [Unsat] via the [not t.ok] entry re-emits the same checkable conclusion (no
         silent traced Unsat — codex CRIT-3). E3 (failed assumption) leaves [t.ok] true
         and is re-derived fresh each solve, so it never populates this. [None] when
         [t.ok] holds or untraced. *)
  ; mutable theory : theory option
  ; mutable budget_tick : (unit -> unit) option
    (* board #60: called at each conflict / decision to tick a deterministic effort counter
     the driver owns; may raise to unwind [solve] at a budget cap. [None] in the pure core
     (bit-identical). *)
  }

let var_decay = 0.95
let cla_decay = 0.999

(* Modern-search tuning (S3 + #155). Novelty-free: the values mirror Glucose / CaDiCaL. *)
let lbd_ema_alpha_fast = 1.0 /. 32.0 (* recent-LBD window *)
let lbd_ema_alpha_slow = 1.0 /. 4096.0 (* long-run LBD average *)
let trail_ema_alpha = 1.0 /. 4096.0 (* trail-length average, for blocking *)
let restart_margin = 1.25 (* fast/slow LBD ratio that forces a restart *)
let block_margin = 1.4 (* trail vs its average that BLOCKS a restart/rephase *)
let restart_min_conflicts = 50 (* EMA warm-up before adaptive restarts / blocking *)
let reduce_first = 2000 (* first reduceDB at this many conflicts *)
let reduce_inc = 300 (* then every this-many more *)
let rephase_base_interval = 1000 (* decisions between the first rephase impulses *)

(* Glucose adaptive restarts default OFF (team-lead ruling B, board #172 fix round). The
   mechanism is fully built and compiled — EMAs, trigger, blocking — but inert behind this
   flag: on the measured corpus it fired for no benefit and cost QF_UF/QF_UFLIA (official
   950: +17 with it on vs +20 with it off; see logs/sat-search-report.md). Luby stays the
   restart policy. Kept as a tuning follow-up: a future family that wants glucose restarts
   flips this to [true] (and re-measures). The LBD EMAs still update (they are the
   trigger's input and cost one float op per conflict); [blocking] still gates the rephase
   impulse. *)
let adaptive_restart_enabled = false

(* One exponential-moving-average step: [x <- x + alpha*(sample - x)]. *)
let ema_step x ~alpha ~sample = x +. (alpha *. (sample -. x))

let create () =
  { nvars = 0
  ; ok = true
  ; assigns = Dynarray.create ()
  ; level = Dynarray.create ()
  ; trail_pos = Dynarray.create ()
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
  ; next_reduce = reduce_first
  ; lbd_ema_fast = 0.0
  ; lbd_ema_slow = 0.0
  ; trail_ema = 0.0
  ; conflicts_since_restart = 0
  ; conflicts_at_solve_start = 0
  ; decisions_since_rephase = 0
  ; rephase_events = 0
  ; rephase_interval = rephase_base_interval
  ; best_trail_len = 0
  ; best_phase = Dynarray.create ()
  ; saved_model = Dynarray.create ()
  ; failed = []
  ; conflicts = 0
  ; decisions = 0
  ; propagations = 0
  ; restart_base = 100
  ; trace = None
  ; terminal = None
  ; theory = None
  ; budget_tick = None
  }
;;

let set_trace t tr = t.trace <- tr
let set_budget_tick t f = t.budget_tick <- f

(* Emit the persisted terminal conclusion of a permanent unsat ([t.terminal]) if a trace
   is installed. Called at every [solve] exit that returns [Unsat] off [not t.ok] (E1
   entry, E2 level-0 conflict, E4 empty theory lemma), so a REPEATED solve on an
   already-unsat core re-emits its checkable conclusion — never a silent traced [Unsat]
   (codex CRIT-3). *)
let emit_terminal t =
  match t.trace, t.terminal with
  | Some tr, Some c -> tr.on_unsat c
  | _ -> ()
;;

(* Tick the driver's effort counter at a counted work event (conflict / decision). Opaque
   to the core; may raise (e.g. [Budget.Exceeded]) to unwind [solve] at a cap — the driver
   catches it at [check_sat]. [None] (pure core) is a no-op branch, bit-identical. *)
let budget_tick t =
  match t.budget_tick with
  | None -> ()
  | Some f -> f ()
;;

(* Pristine-attach (seam lifecycle contract): a theory may be attached/detached only when
   the solver is pristine — [ok], no clauses, empty trail. This makes the lifecycle safe
   as a MECHANISM: attaching after clauses/units exist would leave the theory unaware of
   trail literals it never heard (a wrong-[Sat] risk on theory-unsat instances); detaching
   mid-lifecycle would strand theory-propagated literals whose lazy reasons can no longer
   be reconstructed; and [ok] guards the subtle case where a theory returned an
   unconditional [T_conflict []] — which sets [ok := false] with NOTHING stored, so the
   solver *looks* clause/trail-pristine yet a reattach-then-solve would return wrong-unsat
   off the leftover flag. A poisoned solver is not pristine; rebuild it. The driver
   installs the theory first, before asserting. *)
let set_theory t th =
  if
    (not t.ok)
    || Dynarray.length t.clauses <> 0
    || Dynarray.length t.learnts <> 0
    || Dynarray.length t.trail <> 0
  then
    invalid_arg
      "Sat.set_theory: a theory may only be (de)attached on a pristine solver (ok, no \
       clauses, empty trail)";
  t.theory <- th
;;

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

(* LBD ("glue") of a clause under the current assignment: the number of distinct decision
   levels among its literals (S3). Never on the conflict-free firehose path — only inside
   conflict analysis / reduceDB. *)
let clause_lbd t lits =
  Search_heuristics.lbd_of_levels
    (Array.map (fun l -> Dynarray.get t.level (var_of_lit l)) lits)
;;

(* ------------------------------------------------------------------ *)
(* Variable allocation. Grows every per-var Dynarray and the two watch lists for the new
   var's literals, and makes the var decision-eligible. *)

let ensure_var t v =
  while t.nvars <= v do
    Dynarray.add_last t.assigns 0;
    Dynarray.add_last t.level 0;
    Dynarray.add_last t.trail_pos (-1);
    Dynarray.add_last t.reason Decision;
    Dynarray.add_last t.polarity true;
    Dynarray.add_last t.best_phase true (* best-trail phase memory: FALSE-first default *);
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

let mk_clause_with_id t id lits learnt =
  let c =
    { id; lits = Array.copy lits; activity = 0.0; lbd = 0; learnt; deleted = false }
  in
  if learnt then Dynarray.add_last t.learnts c else Dynarray.add_last t.clauses c;
  c
;;

let mk_clause t lits learnt = mk_clause_with_id t (fresh_id t) lits learnt

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
  Dynarray.set t.trail_pos v (Dynarray.length t.trail);
  Dynarray.set t.reason v reason;
  Dynarray.add_last t.trail lit;
  (* Trail-extension notify (ADR-0005 §3 on_assign): every literal placed on the trail —
     decision, propagation, assumption, learned unit — streams to the theory, which
     filters for its own atoms. Fires in trail order. *)
  match t.theory with
  | None -> ()
  | Some th -> th.on_assign lit
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
      Dynarray.set t.trail_pos v (-1);
      Dynarray.set t.reason v Decision;
      heap_insert t v
    done;
    Dynarray.truncate t.trail target;
    Dynarray.truncate t.trail_lim level;
    t.qhead <- target;
    (* Backjump notify (ADR-0005 §3 on_backtrack): the trail is now unwound to decision
       [level]; the adapter pops the theory state asserted above it. Fires only on a real
       unwind, after the Boolean trail is truncated. *)
    match t.theory with
    | None -> ()
    | Some th -> th.on_backtrack ~level)
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
              else unchecked_enqueue t first (Implied_by c)))))
    done;
    Dynarray.truncate ws !j
  done;
  !confl
;;

(* ------------------------------------------------------------------ *)
(* Theory seam (ADR-0005 §3). A theory conflict/reason is turned into an ordinary clause
   so 1UIP analysis treats it exactly like a propositional one — the seam is
   soundness-preserving by construction (learn ¬premises; propagate with a lazy reason).
   These reason/conflict clauses are TRANSIENT: minted with an id (for trace antecedents)
   but never attached to a watch list or stored in the arena — they exist only to be read
   by [analyze]. *)

let transient_clause t lits =
  { id = fresh_id t; lits; activity = 0.0; lbd = 0; learnt = false; deleted = false }
;;

(* Cert emission (ADR-0013 §4.0): surface a materialized theory transient's id ↔ clause so
   any later citation of it (an [analyze]/[analyze_final] antecedent, or an
   [unsat_conclusion]) resolves to a content-bearing event. [Reason] is the propagation
   clause [p ∨ ¬p₁ ∨ … ∨ ¬pₖ] (implied literal at slot 0); [Conflict] is a falsified
   premise clause. Pure side channel, guarded by the trace: [transient_clause] mints the
   id regardless of trace, so firing this changes nothing when untraced (bit-identical). *)
let note_theory_clause t role c =
  (match t.trace with
   | Some tr -> tr.on_theory_clause ~id:c.id ~clause:c.lits ~role
   | None -> ());
  c
;;

(* The lits of a lazy theory reason clause for a propagated literal [lit] with (validated)
   premises [p₁..pₖ]: [| lit; ¬p₁; …; ¬pₖ |]. Shared by {!theory_reason_clause} and the E3
   [analyze_final] materialization. *)
let reason_lits lit premises =
  let lits = Array.make (List.length premises + 1) lit in
  List.iteri (fun i p -> lits.(i + 1) <- neg_lit p) premises;
  lits
;;

(* [theory.explain lit], validated against CONTRACT-EX: every premise must be an asserted
   (true) literal that appears STRICTLY earlier on the trail than [lit] — the reason valid
   at [lit]'s propagation time. A violation would make a malformed 1UIP back-edge (and can
   close an unsound cycle) or a wrong failed-assumption core, so we raise (not assert, so
   it survives -noassert) rather than trust it. Shared by both consumers of a theory
   reason: the 1UIP path ({!theory_reason_clause}) and the assumption-core path
   ({!analyze_final}). *)
let theory_explain_checked t lit =
  let th =
    match t.theory with
    | Some th -> th
    | None -> assert false
  in
  let premises = th.explain lit in
  let lit_pos = Dynarray.get t.trail_pos (var_of_lit lit) in
  List.iter
    (fun p ->
       if
         not
           (lit_val t p = 1
            && Dynarray.get t.trail_pos (var_of_lit p) >= 0
            && Dynarray.get t.trail_pos (var_of_lit p) < lit_pos)
       then
         raise
           (Theory_contract_violation
              "explain: premise not asserted strictly before the explained literal \
               (CONTRACT-EX)"))
    premises;
  premises
;;

(* The lazy reason clause of a theory-propagated literal [lit] that is currently TRUE (the
   case where 1UIP analysis resolves it): [lit ∨ ¬p₁ ∨ … ∨ ¬pₖ] where [p₁..pₖ] are the
   (validated) premises. Every premise is currently true, so every [¬pᵢ] is false and the
   clause forces [lit] — a genuine implication, valid at [lit]'s propagation time. *)
let theory_reason_clause t lit =
  let premises = theory_explain_checked t lit in
  note_theory_clause t Reason (transient_clause t (reason_lits lit premises))
;;

(* A theory conflict, given the asserted premise set whose conjunction is T-inconsistent:
   the falsified clause [¬p₁ ∨ … ∨ ¬pₙ] (each premise true ⇒ each literal false). An empty
   premise set is an unconditional theory contradiction — an empty (always-false) clause.
   The falsification is verified and raised on (not asserted): an ill-formed conflict fed
   to [analyze] would be a soundness break. *)
let theory_conflict_clause t premises =
  let lits = Array.of_list (List.map neg_lit premises) in
  Array.iter
    (fun l ->
       if lit_val t l <> -1
       then
         raise (Theory_contract_violation "conflict premise set is not all asserted-true"))
    lits;
  note_theory_clause t Conflict (transient_clause t lits)
;;

(* The theory implied [lit] but its negation is already asserted (a propagation into a
   falsified literal): the clause [lit ∨ ¬p₁ ∨ … ∨ ¬pₖ] is then all-false, an immediate
   conflict. Unlike {!theory_reason_clause} there is no precedence relation to check —
   [lit] was never theory-propagated here — only that the resulting clause is falsified. *)
let theory_prop_conflict_clause t lit =
  let th =
    match t.theory with
    | Some th -> th
    | None -> assert false
  in
  let premises = th.explain lit in
  let lits = reason_lits lit premises in
  Array.iter
    (fun l ->
       if lit_val t l <> -1
       then
         raise
           (Theory_contract_violation
              "theory propagated a literal whose negation is asserted, but its \
               explanation is not falsified"))
    lits;
  note_theory_clause t Conflict (transient_clause t lits)
;;

(* ------------------------------------------------------------------ *)
(* 1UIP conflict analysis with local (self-subsumption) minimization.

   Returns the learned clause (asserting literal at index 0), the backjump level, and —
   only when a trace is set — the antecedent clause ids of the resolution derivation, in
   the frozen [on_learned] ordered-RUP order [rₙ..r₁; conflict] (ADR-0013 §1.4(a)): the
   reason clauses in reverse-resolution order, conflict last. The accumulator prepends
   [confl.id] first then each reason [rᵢ.id] as it is resolved (r₁ first), so [!ants] is
   already exactly that order — no reversal. *)

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
    if !c.learnt
    then (
      cla_bump t !c;
      (* LBD improvement (Glucose): a learned clause re-derived as a reason may now tie
         together fewer levels; lowering its LBD protects a recently-useful clause from
         reduceDB. Only lower, never raise. *)
      let l = clause_lbd t !c.lits in
      if l < !c.lbd then !c.lbd <- l);
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
          | Implied_by cc -> cc
          | Theory_prop -> theory_reason_clause t pl (* materialize the lazy reason *)
          | Decision -> assert false);
      if track then ants := !c.id :: !ants)
  done;
  Dynarray.set out 0 (neg_lit !p);
  (* Local minimization: drop a literal whose reason's other literals are all already in
     the learned clause (marked) or fixed at level 0.

     BYPASSED when a trace is active [ADR-0013 §1.4(b)/§4.0 delta 1; frozen sat.mli:156]:
     the emitted-AND-stored learned clause must be the UNMINIMIZED 1UIP clause.
     Minimization drops a literal justified by a level>0 reason that is NOT among [ants]
     (the resolution chain); a hint-restricted ordered-RUP replay of the minimized clause
     would need that absent reason and stall (codex CRIT-1). The full [ants] chain
     (finalized above, before this loop) already matches the unminimized clause exactly.
     Bypassing slows the traced solve (a weaker solver — larger learnts) but keeps every
     downstream antecedent chain over unminimized clauses; trace is OFF by default, so the
     uncertified corpus is untouched. When untraced, minimize as before (bit-identical). *)
  if not track
  then (
    let len = Dynarray.length out in
    let jw = ref 1 in
    for i = 1 to len - 1 do
      let l = Dynarray.get out i in
      let v = var_of_lit l in
      let redundant =
        match Dynarray.get t.reason v with
        | Decision -> false (* a decision literal is never redundant *)
        | Theory_prop ->
          false (* keep theory-propagated literals (sound: never over-drop) *)
        | Implied_by rc ->
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
    Dynarray.truncate out !jw);
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
  learnt, bt, !ants
;;

(* Which assumption literals forced [p] to be true (i.e. the failed-assumption core, §7),
   and — only when a trace is set (ADR-0013 §4.0 E3) — the reason-clause ids of the
   forcing derivation, in ordered-RUP order [rₙ..r₁] (the [Failed_assumption]
   antecedents). [p] is the negation of a false assumption.

   The E3 emission is the assumption-core parallel of {!analyze}'s :626 materialization:
   the trail walk (high→low = most-recent-first) prepends a reason id per crossed
   PROPAGATED literal, so [!ants] ends up oldest-reason-first = the RUP-consumption order.
   Two reason kinds are crossed and BOTH must be materialized [H1, the load-bearing fold]:
   [Implied_by cc] cites [cc.id] (already resolvable as an Input/learned clause);
   [Theory_prop] has NO stored clause, so — like {!analyze}, unlike the pre-cert
   [analyze_final] — the lazy reason [l ∨ ¬p₁ ∨ … ∨ ¬pₖ] is materialized here (same
   premises used for marking), surfaced as a [Reason] theory leaf via
   {!note_theory_clause}, and its id cited. Omitting the [Theory_prop] leg would drop
   exactly the theory- propagation ancestors that dominate the EUF/LIA production path, so
   a genuine theory [unsat] session would be uncertifiable. Assumption/selector literals
   ([Decision]) are the core leaves — added to [out], no antecedent id (they are stripped
   to [] at emission by the session, §1.0). When untraced, [ants] stays [] and no clause
   is materialized — bit-identical to the pre-cert core walk. *)
let analyze_final t p =
  let out = Dynarray.create () in
  Dynarray.add_last out p;
  let track = t.trace <> None in
  let ants = ref [] in
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
        | Decision -> Dynarray.add_last out (neg_lit l)
        | Implied_by c ->
          let lits = c.lits in
          for j = 1 to Array.length lits - 1 do
            let vj = var_of_lit lits.(j) in
            if Dynarray.get t.level vj > 0 then mark vj
          done;
          if track then ants := c.id :: !ants
        | Theory_prop ->
          (* a theory-propagated literal's premises are its reason; mark them (mirrors the
             [Implied_by] clause body, whose slot 0 is [l] itself and is skipped). Same
             strict CONTRACT-EX validation as the 1UIP path — a precedence-violating
             reason here would silently produce a wrong failed-assumption core, so it must
             raise. *)
          let premises = theory_explain_checked t l in
          List.iter
            (fun q -> if Dynarray.get t.level (var_of_lit q) > 0 then mark (var_of_lit q))
            premises;
          if track
          then (
            (* materialize the lazy reason (H1) and cite it *)
            let c =
              note_theory_clause t Reason (transient_clause t (reason_lits l premises))
            in
            ants := c.id :: !ants))
    done;
    Dynarray.iter (fun v -> Dynarray.set t.seen v false) marked);
  List.map neg_lit (Array.to_list (Dynarray.to_array out)), !ants
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
  | Implied_by rc -> rc == c
  | Decision | Theory_prop -> false
;;

let reduce_db t =
  let arr = Dynarray.to_array t.learnts in
  (* LBD-based reduceDB (S3): protect glue (LBD <= threshold), locked, and binary clauses;
     among the rest delete the worst half by LBD (desc), ties by activity (asc). The pure
     selection lives in {!Search_heuristics.reduce_deletions}; here we only supply the
     per-clause stats and apply the verdict. *)
  let stats =
    Array.map
      (fun c ->
         { Search_heuristics.lbd = c.lbd
         ; activity = c.activity
         ; protected_ = locked t c || Array.length c.lits <= 2
         })
      arr
  in
  let del = Search_heuristics.reduce_deletions stats in
  Array.iteri (fun i c -> if del.(i) then c.deleted <- true) arr;
  Dynarray.clear t.learnts;
  Array.iter (fun c -> if not c.deleted then Dynarray.add_last t.learnts c) arr
;;

(* ------------------------------------------------------------------ *)
(* Permanent clause addition with level-0 simplification. Only legal at decision level 0
   (guaranteed by [solve], which cancels to 0 before returning). *)

let add_clause ?(origin = Query) t lits =
  List.iter (fun l -> ensure_var t (var_of_lit l)) lits;
  if t.ok
  then (
    (* Cert emission (ADR-0013 §4.0): reserve a stable id and surface the RAW input clause
       with its [origin] BEFORE level-0 simplification — so a clause that filters to []
       (E1/E4) is still id-resolvable. The retained arena clause below REUSES [input_id]
       (mk_clause_with_id), so any downstream citation of it (a level-0 conflict's
       [conflict_id], an [analyze]/[analyze_final] antecedent) resolves to this [on_input]
       event. Guarded by the trace: untraced runs allocate no id and fire no hook, so the
       id sequence and behaviour are bit-identical to the pre-cert core. *)
    let input_id =
      match t.trace with
      | None -> -1
      | Some tr ->
        let id = fresh_id t in
        tr.on_input ~id ~clause:(Array.of_list lits) ~origin;
        id
    in
    let ls = List.sort_uniq compare lits in
    let tautology = List.exists (fun l -> List.mem (neg_lit l) ls) ls in
    if not tautology
    then (
      let already_true = List.exists (fun l -> lit_val t l = 1) ls in
      if not already_true
      then (
        let ls = List.filter (fun l -> lit_val t l <> -1) ls in
        match ls with
        | [] ->
          t.ok <- false;
          (* E1 (a [Query] input) / E4 (a [Theory_lemma], via {!add_theory_lemmas})
             filtered to []: persist the terminal [Root_empty] citing this input's id, so
             the solve exit (and any repeated solve) re-emits it. E1 vs E4 is read off
             [input_id]'s recorded [origin] by the emitter. (A subsequent level-0 conflict
             cannot follow — [t.ok] is now false — so the propagate-effort empty-lemma
             path, which routes through E2, overwrites this to [Level0_conflict] at the E2
             site as ADR §4.0 specifies.) *)
          if t.trace <> None then t.terminal <- Some (Root_empty { input_id })
        | [ l ] ->
          if lit_val t l = 0
          then (
            unchecked_enqueue t l Decision;
            (* a standing level-0 unit (ADR-0013 §1.3): declared to the checker, which
               also re-derives the unit closure from the [Input] clauses by BCP *)
            match t.trace with
            | Some tr -> tr.on_unit ~id:(fresh_id t) ~lit:l
            | None -> ())
        | _ ->
          let id = if input_id >= 0 then input_id else fresh_id t in
          let c = mk_clause_with_id t id (Array.of_list ls) false in
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

let record_learnt t learnt bt ants lbd =
  if Array.length learnt = 1
  then (
    unchecked_enqueue t learnt.(0) Decision;
    match t.trace with
    | Some tr ->
      tr.on_learned ~id:(fresh_id t) ~clause:learnt ~antecedents:ants ~btlevel:bt
    | None -> ())
  else (
    let c = mk_clause t learnt true in
    c.lbd <- lbd;
    attach t c;
    cla_bump t c;
    unchecked_enqueue t learnt.(0) (Implied_by c);
    match t.trace with
    | Some tr -> tr.on_learned ~id:c.id ~clause:learnt ~antecedents:ants ~btlevel:bt
    | None -> ())
;;

(* Enqueue theory-implied literals (ADR-0005 §3 T_consistent) at the current decision
   level, each with a lazy [Theory_prop] reason. Returns [`Progress true] if any new
   literal was enqueued (re-propagate), [`Progress false] if all were already satisfied
   (fixpoint), or [`Confl c] if an implied literal is already false — its lazy reason
   clause is then falsified, an immediate theory conflict (a well-behaved theory reports
   this via T_conflict; handled here for robustness). *)
let enqueue_theory_lits t lits =
  let rec go progressed = function
    | [] -> `Progress progressed
    | l :: rest ->
      (match lit_val t l with
       | 1 -> go progressed rest (* already implied; skip *)
       | 0 ->
         unchecked_enqueue t l Theory_prop;
         (* Cert emission (ADR-0013 §4.0, codex CRIT-2): a theory propagation at LEVEL 0
            is NOT derivable from the [Input] clauses, so the checker's level-0 BCP
            closure (§1.3) cannot recover it — an E2 [Level0_conflict] (or E3 core)
            resting on it would cite a clause the closure can't falsify. Eagerly
            materialize the reason [l ∨ ¬p₁ ∨ … ∨ ¬pₖ] as a [Reason] theory leaf (via
            {!theory_reason_clause}'s [on_theory_clause] side effect) so the checker can
            derive [l] in its level-0 closure. Level>0 props stay lazy —
            [analyze]/[analyze_final] materialize them on demand. Guarded by the trace
            ([theory_reason_clause] validates CONTRACT-EX and allocates a transient; both
            are pure side channels here). *)
         if t.trace <> None && decision_level t = 0
         then ignore (theory_reason_clause t l : clause);
         go true rest
       | _ -> `Confl (theory_prop_conflict_clause t l) (* forced true but already false *))
  in
  go false lits
;;

(* Add mid-solve theory lemmas (ADR-0005 §3 T_lemma: CONTRACT-SPLIT disjunctions, already
   internalized to existing vars by the adapter). Each becomes a permanent clause. Adding
   a clause during search is only well-defined at level 0 (level-0 simplification assumes
   it), so we first unwind all decisions — a split is a case-split that restarts the
   Boolean search over the refined clause set. A lemma may simplify to the empty clause at
   level 0 (all literals already false), which sets [t.ok <- false]; callers MUST recheck
   [t.ok] and conclude unsat (see uses below).

   Termination note: the [split → re-search] loop has no intrinsic bound (LIA branch-and-
   bound can diverge, CONTRACT-SPLIT-TERM). No split budget exists in this core today; a
   deterministic budget that routes to [unknown] on exhaustion is the driver's obligation
   at M4. A well-behaved theory makes monotone progress (the permanent lemma prevents
   re-reaching the same total assignment). *)
let add_theory_lemmas t clauses =
  cancel_until t 0;
  (* [Theory_lemma] provenance (ADR-0013 §4.0 RR5): a Split/B&B/N-O lemma goes through the
     SAME [add_clause] as a query input, so [on_input] must tag it so the emitter routes
     it to a [Valid_lemma] leaf — never a trusted [Input]. A lemma that filters to [] here
     is the E4 exit (a [Theory_lemma]-origin [Root_empty]). *)
  List.iter (fun ls -> add_clause ~origin:Theory_lemma t ls) clauses
;;

(* Boolean BCP interleaved with cheap Propagate-effort theory checks to a combined
   fixpoint (the Final-effort check is a distinct step, run once at a full model in
   [search]). Returns [Some conflict] (Boolean or theory) or [None] (consistent fixpoint).
   With no theory plugged this is exactly {!propagate}. *)
let propagate_theory t =
  let confl = ref None in
  let again = ref true in
  while !again do
    again := false;
    match propagate t with
    | Some _ as c -> confl := c
    | None ->
      (match t.theory with
       | None -> ()
       | Some th ->
         (match th.check ~final:false with
          | T_consistent [] -> ()
          | T_consistent lits ->
            (match enqueue_theory_lits t lits with
             | `Confl c -> confl := Some c
             | `Progress p -> again := p)
          | T_conflict premises -> confl := Some (theory_conflict_clause t premises)
          | T_lemma clauses ->
            (* D3: Split is a Final-effort result; a Propagate-effort lemma is a contract
               deviation but still sound to add, so we accept it and re-propagate. *)
            add_theory_lemmas t clauses;
            (* a lemma that simplified to the empty clause at level 0 makes the instance
               unsat; surface it as an (empty, always-false) conflict so [handle_confl]
               concludes unsat rather than letting search run on to a spurious model *)
            if t.ok
            then again := true
            else confl := Some (note_theory_clause t Conflict (transient_clause t [||]))))
  done;
  !confl
;;

(* Snapshot the phase of the current trail as the "best" (longest-prefix) phase memory
   whenever the trail is longer than any seen before this solve (#155 Best_trail mode).
   Each assigned var records the polarity that reproduces its current value; unassigned
   vars keep their prior best_phase. Called at a conflict, where the trail is at a local
   maximum. *)
let update_best_trail t =
  let tl = Dynarray.length t.trail in
  if tl > t.best_trail_len
  then (
    t.best_trail_len <- tl;
    for i = 0 to tl - 1 do
      let l = Dynarray.get t.trail i in
      (* value true ⇒ decide positive ⇒ polarity false; value false ⇒ polarity true. The
         literal on the trail is the true one, so [not (sign_of_lit l)] is the polarity. *)
      Dynarray.set t.best_phase (var_of_lit l) (not (sign_of_lit l))
    done)
;;

(* A restart/rephase is BLOCKED while the trail is much larger than its recent average:
   many assignments = progress toward a model, so disrupting it would regress SAT
   instances (the Glucose blocking-restart idea, reused for rephasing — this is what keeps
   the TRUE-flip rephase from costing the QF_UFLIA files it would otherwise churn).
   Requires EMA warm-up ([restart_min_conflicts] conflicts THIS solve — codex M2: a
   per-solve count, not the cumulative [t.conflicts], so a warm [trail_ema] is never
   compared against a cold reset on an incremental re-solve); with conflicts≈0 (the
   firehose) blocking never fires, so the rephase impulse is free to search there. *)
let blocking t =
  t.conflicts - t.conflicts_at_solve_start >= restart_min_conflicts
  && float_of_int (Dynarray.length t.trail) > block_margin *. t.trail_ema
;;

(* Glucose-style adaptive restart: recent learned-clause LBD (fast EMA) running worse than
   the long-run average (slow EMA) means the search is in an unproductive region —
   restart. Gated by EMA warm-up and by {!blocking}. *)
let adaptive_restart t =
  adaptive_restart_enabled
  && t.conflicts_since_restart >= restart_min_conflicts
  && t.lbd_ema_fast > restart_margin *. t.lbd_ema_slow
  && not (blocking t)
;;

(* Apply one rephase impulse from the [{saved, flipped, default, best}] cycle (#155):
   reset the phase-saving array wholesale, then advance the cycle counter. The caller
   restarts so the next descent uses the new phases from the top. *)
let apply_rephase t =
  let mode = Search_heuristics.rephase_mode t.rephase_events in
  t.rephase_events <- t.rephase_events + 1;
  match mode with
  | Search_heuristics.Flipped_true ->
    for v = 0 to t.nvars - 1 do
      Dynarray.set t.polarity v false (* decide TRUE-first *)
    done
  | Search_heuristics.Original_default ->
    for v = 0 to t.nvars - 1 do
      Dynarray.set t.polarity v true (* decide FALSE-first (the create-time default) *)
    done
  | Search_heuristics.Best_trail ->
    for v = 0 to t.nvars - 1 do
      Dynarray.set t.polarity v (Dynarray.get t.best_phase v)
    done
  | Search_heuristics.Saved -> () (* keep phase saving untouched *)
;;

type search_result =
  | R_sat
  | R_unsat
  | R_restart

(* One search episode. Restarts are driven by the Glucose-style adaptive trigger
   ({!adaptive_restart}) plus a conflict-count [conflict_limit] fallback cap (the Luby
   sequence, kept alongside per S3), and by the conflict-independent rephase interval. *)
let search t assumps conflict_limit =
  let result = ref None in
  let conflicts_here = ref 0 in
  (* Handle a conflict clause — Boolean (from BCP) or theory (T_conflict / a falsified
     theory reason). A theory conflict can be falsified below the current decision level;
     realign first by unwinding to the highest level present in the clause, so 1UIP
     analysis sees a literal at the current level (its precondition). For a Boolean BCP
     conflict the highest level is always the current one, so the realignment is a no-op —
     and it is only computed when a theory is plugged, keeping the pure core untouched. *)
  let handle_confl confl =
    t.conflicts <- t.conflicts + 1;
    incr conflicts_here;
    budget_tick t (* effort (#60): one SAT conflict *);
    (* First conflict of THIS solve? Then SEED the EMAs to the first sample rather than
       EMA-stepping up from a cold 0.0 (codex M2: a cold EMA makes [blocking]'s
       [L > 1.4*ema] trivially true for thousands of conflicts, leaving adaptive restart
       inert). Seeding makes the averages meaningful from conflict 1. *)
    let first_conflict = t.conflicts - t.conflicts_at_solve_start = 1 in
    (* Best-trail memory + trail-length EMA at the conflict point (the trail is at a local
       maximum), before any realignment/backjump unwinds it (S3/#155). *)
    update_best_trail t;
    let trail_len = float_of_int (Dynarray.length t.trail) in
    t.trail_ema
    <- (if first_conflict
        then trail_len
        else ema_step t.trail_ema ~alpha:trail_ema_alpha ~sample:trail_len);
    if t.theory <> None
    then (
      let maxl = ref 0 in
      Array.iter
        (fun l ->
           let lv = Dynarray.get t.level (var_of_lit l) in
           if lv > !maxl then maxl := lv)
        confl.lits;
      if !maxl < decision_level t then cancel_until t !maxl);
    if decision_level t = 0
    then (
      t.ok <- false;
      (* E2 (ADR-0013 §4.0): a level-0 conflict. The terminal step is level-0 RUP of
         [confl] against the checker's re-derived unit closure. [confl.id] resolves via
         [on_input] / [on_learned] (a Boolean clause) or [on_theory_clause] (a theory
         transient — incl. an unconditional [T_conflict []] empty clause, Rev 6).
         Persisted so a repeated solve on the now-unsat core re-emits it (codex CRIT-3). *)
      if t.trace <> None
      then t.terminal <- Some (Level0_conflict { conflict_id = confl.id });
      emit_terminal t;
      result := Some R_unsat)
    else (
      let learnt, bt, ants = analyze t confl in
      let lbd = clause_lbd t learnt in
      let lbd_f = float_of_int lbd in
      (* Feed the LBD EMAs (S3 adaptive restart), seeding on the first conflict of the
         solve (codex M2) and counting this conflict toward the restart warm-up. *)
      t.lbd_ema_fast
      <- (if first_conflict
          then lbd_f
          else ema_step t.lbd_ema_fast ~alpha:lbd_ema_alpha_fast ~sample:lbd_f);
      t.lbd_ema_slow
      <- (if first_conflict
          then lbd_f
          else ema_step t.lbd_ema_slow ~alpha:lbd_ema_alpha_slow ~sample:lbd_f);
      t.conflicts_since_restart <- t.conflicts_since_restart + 1;
      cancel_until t bt;
      record_learnt t learnt bt ants lbd;
      var_decay_bump t;
      cla_decay_bump t;
      (* LBD-based reduceDB on the conflict-count schedule (decoupled from restarts). *)
      if t.conflicts >= t.next_reduce
      then (
        reduce_db t;
        t.next_reduce <- t.next_reduce + reduce_inc))
  in
  while !result = None do
    match propagate_theory t with
    | Some confl -> handle_confl confl
    | None ->
      let interval_hit = t.decisions_since_rephase >= t.rephase_interval in
      let do_rephase = interval_hit && not (blocking t) in
      (* A blocked rephase defers to the next interval without advancing the cycle. *)
      if interval_hit && not do_rephase then t.decisions_since_rephase <- 0;
      if do_rephase
      then (
        (* Conflict-independent rephase impulse (#155): reset phases per the cycle and
           restart so the next descent uses them from the top. Fires on the firehose
           (conflicts≈0) where conflict-triggered restarts never do; [blocking] shields
           SAT instances making real progress toward a model (self-correction). ORDER
           MATTERS (codex M1): [cancel_until t 0] must run BEFORE [apply_rephase], because
           backtracking phase-SAVES every trailed var back to its current value — which
           would clobber the wholesale polarity reset for exactly the ~1000 assigned
           firehose vars the [Flipped_true] epoch targets. Cancel first, then flip. *)
        cancel_until t 0;
        apply_rephase t;
        t.rephase_interval <- Search_heuristics.grow_interval t.rephase_interval;
        t.decisions_since_rephase <- 0;
        t.conflicts_since_restart <- 0;
        result := Some R_restart)
      else if
        (conflict_limit > 0 && !conflicts_here >= conflict_limit) || adaptive_restart t
      then (
        cancel_until t 0;
        t.conflicts_since_restart <- 0;
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
            let core, ants = analyze_final t (neg_lit pa) in
            t.failed <- core;
            (* E3 (ADR-0013 §4.0), the universal session exit: [ants] is the
               assumption-forcing reason chain in ordered-RUP order (Implied_by clause
               ids + materialized Theory_prop reason ids). After the session strips the
               assumed selectors it derives []. *)
            (match t.trace with
             | Some tr -> tr.on_unsat (Failed_assumption { antecedents = ants })
             | None -> ());
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
            | Some l ->
              t.decisions <- t.decisions + 1;
              t.decisions_since_rephase <- t.decisions_since_rephase + 1
              (* rephase interval clock (#155): count genuine branch decisions only —
                 assumption-forced decisions have a fixed phase, so they never need
                 rephasing and would only dilute the interval *);
              new_decision_level t;
              unchecked_enqueue t l Decision;
              (* effort (#60): tick AFTER the decision is on the trail. [pick_branch] has
                 already popped [l]'s var from the VSIDS heap; ticking before [enqueue]
                 would let [Budget.Exceeded] escape with the var neither on the trail nor
                 in the heap, and [Sat.solve]'s entry [cancel_until 0] only restores
                 trailed vars — the var would be lost, and a later solve on the same core
                 could return a model that omits (hence may falsify a clause over) it
                 [codex AP1, wrong-SAT]. Ticking here means any raise leaves [l] trailed
                 and fully recoverable by [cancel_until 0]. *)
              budget_tick t
            | None ->
              (* Full Boolean assignment consistent under Propagate-effort. A plugged
                 theory gets a complete (Final) check: it may accept the model (Sat),
                 refute it (T_conflict), or refine the search (T_lemma split / propagate,
                 e.g. B&B or model-based N-O). *)
              (match t.theory with
               | None ->
                 save_model t;
                 result := Some R_sat
               | Some th ->
                 (match th.check ~final:true with
                  | T_consistent lits ->
                    (match enqueue_theory_lits t lits with
                     | `Confl c -> handle_confl c
                     | `Progress true -> () (* re-check at the new fixpoint *)
                     | `Progress false ->
                       save_model t;
                       result := Some R_sat)
                  | T_conflict premises ->
                    handle_confl (theory_conflict_clause t premises)
                  | T_lemma clauses ->
                    add_theory_lemmas t clauses;
                    (* an empty-at-level-0 lemma makes the instance unsat (blocker: search
                       must not run on to a spurious model) *)
                    if not t.ok
                    then (
                      (* E4 (ADR-0013 §4.0 H3): a Final-effort [Theory_lemma] filtered to
                         [] at level 0, setting [result] directly (no [confl], distinct
                         from E2). [t.terminal] was set to [Root_empty] (that lemma's
                         [on_input] id, [Theory_lemma] origin) by [add_clause]; emit it. *)
                      emit_terminal t;
                      result := Some R_unsat))))
          else (
            t.decisions <- t.decisions + 1;
            new_decision_level t;
            unchecked_enqueue t !next Decision;
            (* effort (#60): tick AFTER enqueue, same rule as the [pick_branch] site above
               (codex AP1). This branch decides an assumption literal, whose var was NOT
               popped from the heap, so a pre-enqueue raise here would in fact be
               recoverable — but ticking post-enqueue keeps a single, obviously-safe
               placement for every decision and is robust to future refactors. *)
            budget_tick t))
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
  then (
    (* Permanent-unsat entry: re-emit the persisted terminal conclusion (codex CRIT-3, no
       silent traced Unsat). [t.terminal] is [Root_empty] when a [Query] input filtered to
       [] before any solve (E1), or [Level0_conflict]/[Root_empty] carried over from a
       prior solve that drove the core permanently unsat (E2/E4). E3 leaves [t.ok] true,
       so it never reaches here. *)
    emit_terminal t;
    Unsat)
  else (
    let assumps = Array.of_list assumptions in
    cancel_until t 0;
    (* Reset the modern-search episode state (S3 + #155) so this [solve] is a
       deterministic function of the clause set + assumptions — two identical solves stay
       bit-identical (I6). [best_phase] contents persist as warm memory (also
       deterministic); only its length watermark resets so the next solve can re-snapshot. *)
    t.lbd_ema_fast <- 0.0;
    t.lbd_ema_slow <- 0.0;
    t.trail_ema <- 0.0;
    t.conflicts_since_restart <- 0;
    t.conflicts_at_solve_start <- t.conflicts;
    t.decisions_since_rephase <- 0;
    t.rephase_events <- 0;
    t.rephase_interval <- rephase_base_interval;
    (* reduceDB schedule RELATIVE to this solve's starting conflict count (codex M3): an
       absolute [reduce_first] would fire reduceDB immediately (then every conflict) on an
       incremental re-solve whose cumulative [t.conflicts] already exceeds it. *)
    t.next_reduce <- t.conflicts + reduce_first;
    t.best_trail_len <- 0;
    let rec go restart_no =
      (* Luby conflict-count cap kept alongside the adaptive trigger (S3): a loose upper
         bound that forces a restart if the LBD trigger never fires. *)
      let lim = luby restart_no * t.restart_base in
      match search t assumps lim with
      | R_restart -> go (restart_no + 1)
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
