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

(* FLAT CLAUSE ARENA (task #48, W3). A clause is a [cref]: an index into parallel arrays.
   All clause literals are concatenated into one shared [a_lits]; a clause owns the span
   [a_off.(cr) .. a_off.(cr) + a_len.(cr)). Slots 0 and 1 of that span are the two watched
   literals (propagation swaps within the span, exactly like the old [c.lits.(0)/(1)]
   swaps). [a_id.(cr)] is the stable unique id (proof-readiness, §7). [a_lbd]/[a_act] carry
   the LBD "glue" and VSIDS activity. [a_flags] bit 0 = learnt, bit 1 = deleted; deleted is
   set by [reduce_db]/vivification and swept lazily out of watch lists during propagation.

   Why an arena. It replaces per-clause heap records (each with its own [lits] array),
   cutting the allocation/GC-marking churn that dominated the hard gap files; and it lets a
   clause be named by an IMMEDIATE int, so a watch entry and an [Implied_by] reason store an
   unboxed cref into an unboxed [int] array — NO [caml_modify] write barrier fires on the
   propagation firehose (the 6.7% self-time the thr-sat-watch lever-1 analysis attributed
   3:1 to watch-entry stores over reason stores). [reduce_db] is the crux: it rebuilds a
   fresh arena from the kept clauses and remaps every live cref (all watch lists + every
   [Implied_by] reason) — see there. *)
type cref = int

(* Reason encoding (was [type reason = Decision | Implied_by of clause | Theory_prop]).
   Stored UNBOXED in an [int Dynarray.t] so [reason.(v) <- ...] fires no write barrier. A
   nonnegative value IS the [Implied_by] cref; the two negative sentinels are the constant
   constructors. [Decision] is a branch choice or a level-0 unit; [Theory_prop] marks a
   literal a plugged theory enqueued (ADR-0005 §3 T_consistent), whose reason clause is
   not stored but reconstructed lazily via [theory.explain] iff conflict analysis resolves
   on it (CONTRACT-EX). With no theory plugged only [Decision]/[Implied_by] occur. *)
let r_decision = -1
let r_theory = -2

(* An off-arena TRANSIENT theory reason/conflict clause (ADR-0005 §3). Minted with an id
   (for trace antecedents) but never attached to a watch list, never stored in the arena,
   never reduced — it exists only to be read by [analyze]/[analyze_final]. Kept as a
   lightweight record (id + lits) so it costs the arena nothing, preserving the pre-arena
   transient behaviour. *)
type tclause =
  { tid : int
  ; tlits : lit array
  }

(* A CLAUSE HANDLE for conflict analysis: either an arena clause (by cref) or an off-arena
   transient. Used ONLY by [analyze]/[analyze_final]/[handle_confl] — never stored in a
   watch or a reason (those hold bare crefs), so this box is per-conflict, off the
   propagation firehose. *)
type chandle =
  | H_arena of cref
  | H_transient of tclause

(* A per-literal WATCH LIST, unboxed. The old [watch = { cl : clause; mutable blocker }]
   record lived in a [watch Dynarray.t] — a POINTER array, so compacting it during
   propagation ([Dynarray.set ws j w]) fired [caml_modify] on every kept entry (the
   dominant barrier cost). Here each list is two PARALLEL unboxed [int Dynarray.t] kept in
   lockstep: entry [i] is the clause [Dynarray.get wc i] watched with cached blocker
   literal [Dynarray.get wb i]. Both are plain [int] arrays, so a compaction store is
   barrier-free. The blocker is the MiniSat fast-path cache: if it is already true the
   clause is satisfied and needs no inspection. INVARIANT: [wc] and [wb] always have equal
   length (every add/keep/relocate/truncate below touches them together). *)
type watchlist =
  { wc : int Dynarray.t (* watched clause crefs *)
  ; wb : int Dynarray.t (* cached blocker literal per entry *)
  }

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
  ; reason : int Dynarray.t
    (* why the var was assigned, UNBOXED (Implied_by cref>=0 / [r_decision] /
         [r_theory]); an [int] array, so [reason.(v) <- ...] fires no write barrier *)
  ; polarity : bool Dynarray.t (* saved phase: true => decide negative first *)
  ; seen : bool Dynarray.t (* scratch flag for conflict analysis *)
  ; (* Per-variable VSIDS activity and its max-heap (top = highest activity). *)
    var_act : float Dynarray.t
  ; heap : int Dynarray.t (* heap of vars *)
  ; heap_pos : int Dynarray.t (* var -> index in [heap], or -1 if absent *)
  ; (* Watch lists indexed by literal (length [2 * nvars]). *)
    watches : watchlist Dynarray.t
  ; (* The assignment trail and its per-decision-level boundaries. *)
    trail : lit Dynarray.t
  ; trail_lim : int Dynarray.t
  ; mutable qhead : int (* propagation cursor into [trail] *)
  ; (* Flat clause arena (parallel arrays indexed by cref). [a_lits] holds every clause's
       literals concatenated; a clause [cr] owns [a_lits.(a_off.(cr) .. +a_len.(cr))],
       slots 0/1 watched. [a_flags] bit0 learnt, bit1 deleted. [clauses]/[learnts] are the
       crefs of the original / learned clauses (originals never deleted; learnts subject
       to [reduce_db]). *)
    a_lits : int Dynarray.t
  ; a_off : int Dynarray.t
  ; a_len : int Dynarray.t
  ; a_id : int Dynarray.t
  ; a_lbd : int Dynarray.t
  ; a_act : float Dynarray.t
  ; a_flags : int Dynarray.t
  ; clauses : cref Dynarray.t
  ; learnts : cref Dynarray.t
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
    (* board #60: called at each conflict / decision to tick a deterministic effort
         counter the driver owns; may raise to unwind [solve] at a budget cap. [None] in
         the pure core (bit-identical). *)
  ; mutable branch_filter : (int -> bool) option
    (* Relevancy branch-filter (sat.mli set_branch_filter). [None] => [pick_branch] is
         bit-identical to the pre-hook core. When [Some f], [pick_branch] will not DECIDE
         an unassigned var [v] with [f v = false]; it skips and re-inserts it (so it stays
         a candidate for when it becomes relevant), and returns [None] when only
         filtered-out vars remain (a complete assignment over the branchable vars => hand
         off to the Final check). The filter adds nothing to the trail, so it cannot
         create a conflict; the driver owns the soundness obligation that an irrelevant
         atom is safe to leave unbranched (backstop: the session Final-check /
         model-check, fail-closed). *)
  ; (* CNF preprocessing / inprocessing (DESIGN.md A10; Jacobs 2021 bounded clause
       elimination). [satpre] is the OXSMT_SATPRE env gate, read once at [create]: OFF =>
       [preprocess] is a no-op and every field below stays empty/false, so the core is
       bit-identical. *)
    satpre : bool
  ; eliminable : bool Dynarray.t
    (* per-var: [true] iff a client marked [v] eliminable (set_eliminable). DEFAULT
         false (frozen) — only pure-aux Tseitin vars are ever marked. *)
  ; eliminated : bool Dynarray.t
    (* per-var: [true] once [preprocess] eliminated [v] (all its clauses removed). Such
         a var is skipped by [pick_branch] (it is in no clause) and its model value is
         reconstructed in [save_model]. *)
  ; elim_stack : (lit array * lit) Dynarray.t
    (* the reconstruction stack (the note's clause-deletion form): (deleted clause,
         pivot literal), in elimination order. [save_model] pops it in REVERSE, flipping
         the pivot var to satisfy any deleted clause the reduced model left unsatisfied. *)
  ; restore_map : (var, lit array list) Hashtbl.t
    (* per eliminated pivot var: the clauses deleted for it, so [add_clause] can restore
         them (re-add + un-eliminate) if a later clause names the var — the note's
         "restore clauses deleted on l when a clause containing ¬l arrives" (kept sound
         under incrementality). Empty unless something was eliminated. *)
  ; equiv : (var, lit) Hashtbl.t
    (* Equivalent-literal substitution (ELS) reconstruction: [x -> L] means the positive
         literal of [x] is equivalent to literal [L] (a representative that is NOT itself
         ELS-eliminated), so [x] was substituted away. [save_model] sets [x]'s value to
         [L]'s AFTER the flip-stack pass — a definitional reconstruction distinct from the
         note's flip-to-satisfy. Empty unless ELS eliminated something. *)
  ; mutable inproc_next : int
    (* Phase-2 inprocessing schedule: fire a restart-boundary round once [t.conflicts]
         reaches this. Reset per [solve] to a first-round offset; stepped by
         [inproc_interval] with geometric back-off after each fire. Only consulted when
         [satpre] is on, so the pure-core schedule is inert. *)
  ; mutable inproc_interval : int
  ; (* A10 elimination statistics (cumulative over the solver's life), for the A/B
       per-family report. Emitted to stderr at each [solve] exit when OXSMT_SATPRE_STATS
       is truthy; never read by search, no frozen surface. *)
    mutable stat_elim_vars : int
  ; mutable stat_deleted_clauses : int
  ; mutable stat_resolvents : int
  ; mutable stat_vivified : int
  ; mutable stat_els : int
  ; mutable stat_flp : int
  ; chrono : bool
    (* Chronological backtracking (task #41 Stage 1; Nadel–Ryvchin SAT'18, Möhle–Biere
         SAT'19). Read once from [OXSMT_CHRONO] at [create]; [false] (default) ⇒ every CB
         branch below is dead and the core is BYTE-IDENTICAL (verdicts, models, and the
         conflicts/decisions/propagations trio) to the pre-CB engine. When [true], the
         trail is no longer level-monotone: each literal's [level] slot is its TRUE level
         (max level among a propagation's reason, a fresh level for a decision),
         [cancel_until] removes the SCATTERED [level > target] literals rather than a
         suffix, [analyze] works at the conflict clause's max level, and [handle_confl]
         backtracks chronologically to [conflict_level - 1] when the backjump gap is
         within [chrono_threshold]. *)
  ; chrono_threshold : int
    (* Nadel–Ryvchin threshold T ([OXSMT_CHRONO_T], default 100): backjump
         non-chronologically (to [bt]) when [conflict_level - bt > chrono_threshold], else
         chronologically (to [conflict_level - 1]). Inert unless [chrono]. *)
  ; chrono_reason : (int, lit list) Hashtbl.t
    (* F1: preserved reasons of surviving theory-PROPAGATED literals across a chrono
         [cancel_until] rebuild. var -> its snapshotted premise list, taken at the
         pre-rebuild instant (when the adapter's reason cache is still intact) and served
         by {!theory_premises} afterwards, because the rebuild ([on_backtrack ~level:0] +
         replay) destroys the adapter's frame-scoped cache for the survivor. Entries are
         dropped when the var is removed by a later [cancel_until]; empty (and untouched)
         unless [chrono]. Per-[t] (vars are per-[t]); [reset] each [solve]. *)
  ; base_l0_cert_mode : bool
    (* The ONE base-l0 CERTIFICATE-EMITTER mode bit (base #53). Default [false] ⇒ every
     emitter behaviour is byte-identical to the pre-#53 build (raw-Sat [cert_emit_test]
     on_unit + E2 expectations hold). The session sets it [true] under base-l0
     (OXSMT_BASE_L0), where it drives BOTH cert-mode behaviours together so the OFF path
     stays trunk-identical BY CONSTRUCTION (codex #53 bounce):
     1. [add_clause] SUPPRESSES the redundant [on_unit] level-0-unit DECLARATION — the
        checker re-derives every level-0 unit from the raw [Input] clause + BCP
        (verified-not-trusted, checker.ml (b)), and under base-l0 a base-frame input unit
        that a level-0 THEORY conflict retracts in the checker's (legitimately
        contradictory) closure would otherwise spuriously fail (b).
     2. a level-0 THEORY conflict concludes via the empty-core E3 [Failed_assumption]
        route rather than E2 [Level0_conflict] (see [handle_confl]) — E3's [refutes_under]
        over the whole DB certifies the contradictory closure; (b)'s false failure is
        avoided the same way the pre-base-l0 build's base ASSUMPTION made these E3. Set
        once at [create]; never read by search — no effect on verdicts/models/counters. *)
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

let inproc_first =
  5000 (* first inprocessing round after this many conflicts this solve *)
;;

let inproc_interval_base =
  5000 (* base gap between rounds; doubles each fire (back-off) *)
;;

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

(* CNF preprocessing / inprocessing gate (DESIGN.md A10). OFF by default; ON only when
   OXSMT_SATPRE names a truthy value. Read once at [create] into [t.satpre]. With it OFF
   the whole feature is inert (bit-identical). *)
let satpre_enabled () =
  match Sys.getenv_opt "OXSMT_SATPRE" with
  | Some ("1" | "true" | "yes" | "on") -> true
  | Some _ | None -> false
;;

(* Chronological backtracking (task #41) is dark, env-gated at [create]: unset ⇒ [false] ⇒
   byte-identical. Same on-value vocabulary as [OXSMT_RELEVANCY]. *)
let chrono_from_env () =
  match Sys.getenv_opt "OXSMT_CHRONO" with
  | Some ("1" | "true" | "yes" | "on") -> true
  | Some _ | None -> false
;;

(* First-inprocessing-round conflict offset (Phase 2). A measurement/test knob
   (OXSMT_SATPRE_INPROC_FIRST): default [inproc_first], overridable so an A/B can tune
   round frequency and a test can force an early round. Read per [solve]; only consulted
   when the gate is on. *)
let inproc_first_offset () =
  match Sys.getenv_opt "OXSMT_SATPRE_INPROC_FIRST" with
  | Some s ->
    (try max 1 (int_of_string s) with
     | _ -> inproc_first)
  | None -> inproc_first
;;

(* Nadel–Ryvchin threshold T ([OXSMT_CHRONO_T]); non-negative int, default 100 (the paper
   default). A malformed or negative value falls back to the default. *)
let chrono_threshold_from_env () =
  match Sys.getenv_opt "OXSMT_CHRONO_T" with
  | Some s ->
    (match int_of_string_opt s with
     | Some n when n >= 0 -> n
     | Some _ | None -> 100)
  | None -> 100
;;

let create ?(base_l0_cert_mode = false) () =
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
  ; a_lits = Dynarray.create ()
  ; a_off = Dynarray.create ()
  ; a_len = Dynarray.create ()
  ; a_id = Dynarray.create ()
  ; a_lbd = Dynarray.create ()
  ; a_act = Dynarray.create ()
  ; a_flags = Dynarray.create ()
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
  ; branch_filter = None
  ; satpre = satpre_enabled ()
  ; eliminable = Dynarray.create ()
  ; eliminated = Dynarray.create ()
  ; elim_stack = Dynarray.create ()
  ; restore_map = Hashtbl.create 16
  ; equiv = Hashtbl.create 16
  ; inproc_next = max_int
  ; inproc_interval = 0
  ; stat_elim_vars = 0
  ; stat_deleted_clauses = 0
  ; stat_resolvents = 0
  ; stat_vivified = 0
  ; stat_els = 0
  ; stat_flp = 0
  ; chrono = chrono_from_env ()
  ; chrono_threshold = chrono_threshold_from_env ()
  ; chrono_reason = Hashtbl.create 16
  ; base_l0_cert_mode
  }
;;

let set_trace t tr = t.trace <- tr
let set_budget_tick t f = t.budget_tick <- f
let set_branch_filter t f = t.branch_filter <- f

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
(* Flat clause arena accessors. A [cref] indexes the parallel [a_*] arrays; a clause's
   literals live at [a_lits.(a_off.(cr) .. +a_len.(cr))]. All are O(1) [Dynarray] gets.
   The literal getter/setter add the per-clause base offset; slots 0/1 are the watched
   pair, so [cl_set_lit] is exactly the old [c.lits.(i) <- _] watched-literal swap (in
   place, within the clause's span — never relocates). *)
let cl_len t (cr : cref) = Dynarray.get t.a_len cr
let cl_lit t (cr : cref) i = Dynarray.get t.a_lits (Dynarray.get t.a_off cr + i)
let cl_set_lit t (cr : cref) i l = Dynarray.set t.a_lits (Dynarray.get t.a_off cr + i) l
let cl_id t (cr : cref) = Dynarray.get t.a_id cr
let cl_lbd t (cr : cref) = Dynarray.get t.a_lbd cr
let cl_set_lbd t (cr : cref) v = Dynarray.set t.a_lbd cr v
let cl_act t (cr : cref) = Dynarray.get t.a_act cr
let cl_set_act t (cr : cref) v = Dynarray.set t.a_act cr v
let cl_learnt t (cr : cref) = Dynarray.get t.a_flags cr land 1 <> 0
let cl_deleted t (cr : cref) = Dynarray.get t.a_flags cr land 2 <> 0

let cl_set_deleted t (cr : cref) =
  Dynarray.set t.a_flags cr (Dynarray.get t.a_flags cr lor 2)
;;

(* Materialize a clause's literals as a fresh array (allocates). Off the firehose: used
   only where an array value is genuinely needed (never in [propagate]). *)
let cl_lits t (cr : cref) = Array.init (cl_len t cr) (fun i -> cl_lit t cr i)

(* Clause-handle accessors, unifying an arena cref and an off-arena transient for
   [analyze]/[analyze_final]. Per-conflict, off the firehose. A transient is never learnt. *)
let ch_id t = function
  | H_arena cr -> cl_id t cr
  | H_transient tc -> tc.tid
;;

let ch_len t = function
  | H_arena cr -> cl_len t cr
  | H_transient tc -> Array.length tc.tlits
;;

let ch_lit t ch i =
  match ch with
  | H_arena cr -> cl_lit t cr i
  | H_transient tc -> tc.tlits.(i)
;;

(* Append a clause to the arena and return its cref. Copies [lits] into the shared
   [a_lits] (so the caller's array is never aliased — matching the old [Array.copy]);
   records the metadata; and files the cref in [clauses] (original) or [learnts]
   (learned). *)
let arena_add t id lits learnt =
  let cr : cref = Dynarray.length t.a_off in
  let off = Dynarray.length t.a_lits in
  Array.iter (fun l -> Dynarray.add_last t.a_lits l) lits;
  Dynarray.add_last t.a_off off;
  Dynarray.add_last t.a_len (Array.length lits);
  Dynarray.add_last t.a_id id;
  Dynarray.add_last t.a_lbd 0;
  Dynarray.add_last t.a_act 0.0;
  Dynarray.add_last t.a_flags (if learnt then 1 else 0);
  if learnt then Dynarray.add_last t.learnts cr else Dynarray.add_last t.clauses cr;
  cr
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

let cla_bump t (cr : cref) =
  cl_set_act t cr (cl_act t cr +. t.cla_inc);
  if cl_act t cr > 1e20
  then (
    Dynarray.iter (fun cr -> cl_set_act t cr (cl_act t cr *. 1e-20)) t.learnts;
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

(* LBD of an ARENA clause [cr], reading its literals' levels in place (no
   materialization). Same value as [clause_lbd t (cl_lits t cr)]; used on the per-conflict
   [analyze] path. *)
let clause_lbd_cref t (cr : cref) =
  clause_lbd t (Array.init (cl_len t cr) (fun i -> cl_lit t cr i))
;;

(* ------------------------------------------------------------------ *)
(* Variable allocation. Grows every per-var Dynarray and the two watch lists for the new
   var's literals, and makes the var decision-eligible. *)

let ensure_var t v =
  while t.nvars <= v do
    Dynarray.add_last t.assigns 0;
    Dynarray.add_last t.level 0;
    Dynarray.add_last t.trail_pos (-1);
    Dynarray.add_last t.reason r_decision;
    Dynarray.add_last t.polarity true;
    Dynarray.add_last t.best_phase true (* best-trail phase memory: FALSE-first default *);
    Dynarray.add_last t.seen false;
    Dynarray.add_last t.var_act 0.0;
    Dynarray.add_last t.heap_pos (-1);
    Dynarray.add_last t.watches { wc = Dynarray.create (); wb = Dynarray.create () };
    Dynarray.add_last t.watches { wc = Dynarray.create (); wb = Dynarray.create () };
    Dynarray.add_last t.eliminable false (* default frozen (A10) *);
    Dynarray.add_last t.eliminated false;
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

(* Mark [v] eligible for variable elimination (DESIGN.md A10). Idempotent; grows per-var
   state on demand so a var named before allocation is handled. DEFAULT is frozen — this
   is the sole opt-in. *)
let set_eliminable t v =
  ensure_var t v;
  Dynarray.set t.eliminable v true
;;

let fresh_id t =
  let id = t.next_id in
  t.next_id <- t.next_id + 1;
  id
;;

let mk_clause_with_id t id lits learnt : cref = arena_add t id lits learnt
let mk_clause t lits learnt = mk_clause_with_id t (fresh_id t) lits learnt

(* Add a watch entry (cref + cached blocker) to literal [l]'s list, keeping the parallel
   arrays in lockstep. *)
let watch_add t l ~cref ~blocker =
  let ws = Dynarray.get t.watches l in
  Dynarray.add_last ws.wc cref;
  Dynarray.add_last ws.wb blocker
;;

let attach t (cr : cref) =
  let l0 = cl_lit t cr 0
  and l1 = cl_lit t cr 1 in
  watch_add t (neg_lit l0) ~cref:cr ~blocker:l1;
  watch_add t (neg_lit l1) ~cref:cr ~blocker:l0
;;

(* ------------------------------------------------------------------ *)
(* Trail. *)

let new_decision_level t = Dynarray.add_last t.trail_lim (Dynarray.length t.trail)

(* The decision level to stamp on a literal being enqueued (task #41 §10.1). Without CB
   ([not t.chrono]) it is always the current [decision_level] — byte-identical to the
   pre-CB core, which set exactly that. Under CB a literal carries its TRUE level: a
   decision (or a theory propagation, tagged conservatively at the current level) gets the
   current [decision_level]; a Boolean propagation [Implied_by c] gets the max level among
   the reason clause's OTHER literals ([c.lits.(1..)], all currently false), which is the
   level at which the clause became unit — this may be BELOW the current decision level,
   and is precisely what makes the out-of-order trail correct. Derived internally (no
   signature change): all 8 [unchecked_enqueue] call sites are untouched, and
   [record_learnt]'s [Implied_by] path automatically stamps the asserting literal its
   correct backjump level [bt] even when the trail sits chronologically at
   [conflict_level - 1]. *)
let enqueue_level t reason =
  if not t.chrono
  then decision_level t
  else if reason < 0
  then (* [r_decision] / [r_theory] *) decision_level t
  else (
    (* [Implied_by cr]: max level among the reason clause's OTHER (currently false) lits. *)
    let cr : cref = reason in
    let m = ref 0 in
    for i = 1 to cl_len t cr - 1 do
      let lv = Dynarray.get t.level (var_of_lit (cl_lit t cr i)) in
      if lv > !m then m := lv
    done;
    !m)
;;

let unchecked_enqueue t lit reason =
  let v = var_of_lit lit in
  Dynarray.set t.assigns v (if sign_of_lit lit then 1 else -1);
  Dynarray.set t.level v (enqueue_level t reason);
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

(* The premise set of a theory-propagated [lit]. Normally the theory's own [explain]
   (which reads its intact per-frame reason cache). Under CB, a surviving
   theory-propagated literal's adapter cache was destroyed by a [cancel_until] rebuild
   ([on_backtrack ~level:0]); its reason was snapshotted into [t.chrono_reason] just
   before that rebuild, so we serve it from there (F1). A freshly propagated literal has
   no snapshot and falls through to [explain] against the fresh cache. OFF
   ([not t.chrono]): [chrono_reason] is always empty, so this is exactly [th.explain lit]
   — byte-identical. *)
let theory_premises t th lit =
  match Hashtbl.find_opt t.chrono_reason (var_of_lit lit) with
  | Some prem -> prem
  | None -> th.explain lit
;;

(* Undo assignments back to [level] (0-based decision level to keep). Saves the phase of
   every unassigned var (phase saving) and returns it to the heap.

   Without CB (the [not t.chrono] arm) this truncates the trail SUFFIX [\[target, end)]
   and is byte-identical to the pre-CB core. Under CB the [level > level] literals are
   SCATTERED (the trail is not level-monotone), so we scan the whole trail, unassign
   exactly those, compact the survivors in place preserving their relative order (and
   their [trail_pos], which [explain]'s CONTRACT-EX reads), truncate [trail_lim] purely as
   the decision COUNTER (its position entries are meaningless under CB), fire the theory
   backtrack, and reset [qhead] to 0 (§10.2 — the soundness-critical crux; see below). *)
let cancel_until t level =
  if decision_level t > level
  then
    if not t.chrono
    then (
      let target = Dynarray.get t.trail_lim level in
      for i = Dynarray.length t.trail - 1 downto target do
        let l = Dynarray.get t.trail i in
        let v = var_of_lit l in
        Dynarray.set t.polarity v (Dynarray.get t.assigns v = -1);
        Dynarray.set t.assigns v 0;
        Dynarray.set t.trail_pos v (-1);
        Dynarray.set t.reason v r_decision;
        heap_insert t v
      done;
      Dynarray.truncate t.trail target;
      Dynarray.truncate t.trail_lim level;
      t.qhead <- target;
      (* Backjump notify (ADR-0005 §3 on_backtrack): the trail is now unwound to decision
         [level]; the adapter pops the theory state asserted above it. Fires only on a
         real unwind, after the Boolean trail is truncated. *)
      match t.theory with
      | None -> ()
      | Some th -> th.on_backtrack ~level)
    else (
      (* Scattered removal + in-place compaction of survivors (level <= [level]). *)
      let n = Dynarray.length t.trail in
      let w = ref 0 in
      for i = 0 to n - 1 do
        let l = Dynarray.get t.trail i in
        let v = var_of_lit l in
        if Dynarray.get t.level v > level
        then (
          Dynarray.set t.polarity v (Dynarray.get t.assigns v = -1);
          Dynarray.set t.assigns v 0;
          Dynarray.set t.trail_pos v (-1);
          Dynarray.set t.reason v r_decision;
          Hashtbl.remove t.chrono_reason v;
          heap_insert t v)
        else (
          Dynarray.set t.trail !w l;
          Dynarray.set t.trail_pos v !w;
          incr w)
      done;
      Dynarray.truncate t.trail !w;
      Dynarray.truncate t.trail_lim level;
      (* F1 — explanation provenance for surviving theory-PROPAGATED literals. The
         theory-seam rebuild below pops the theory to base ([on_backtrack ~level:0]) and
         re-drives it ONLY with [on_assign] (assertions); it never re-runs a theory check,
         so a surviving [Theory_prop] literal is re-asserted as a FACT with NO reason
         re-cached — the adapter's frame-scoped [explain_cache] (euf/lia/arr) for its
         now-popped frame is gone, and a later [analyze] resolving through it would hit
         "no cached reason (frame was popped)" and fail closed to unknown. We CANNOT
         simply drop such survivors: a survivor's [Implied_by] reason clause may cite one
         as a false premise, so removing it mid-trail corrupts that reason (a
         [Decision]-pivot in [analyze]). Instead we SNAPSHOT each surviving [Theory_prop]
         literal's reason NOW — while the theory's cache is still intact — into
         [t.chrono_reason], and serve it from there afterwards (see [theory_premises]).
         The snapshot is precedence-valid because it is taken at the pre-rebuild instant
         (mirrors the adapters' own propagation-time snapshot), and
         [theory_explain_checked] re-validates CONTRACT-EX against the compacted trail at
         use time (fail-closed if a premise did not survive — the rare non-monotone case).
         INVARIANT: every surviving [Theory_prop] literal remains explainable via its
         snapshot. *)
      (match t.theory with
       | None -> ()
       | Some th ->
         for i = 0 to Dynarray.length t.trail - 1 do
           let l = Dynarray.get t.trail i in
           let v = var_of_lit l in
           if Dynarray.get t.reason v = r_theory
           then
             (* [theory_premises] serves an EXISTING snapshot if this survivor was already
                snapshotted at an earlier backtrack (its adapter cache is long gone); only
                a freshly-propagated survivor falls through to the intact [th.explain]. *)
             Hashtbl.replace t.chrono_reason v (theory_premises t th l)
         done);
      (* THE §10.2 CRUX. A scattered removal can turn a clause whose ONLY satisfying
         literal was a removed watched literal — with a surviving FALSE partner watch —
         into an UNDETECTED unit (or, once the caller flips a literal, a conflict).
         Standard suffix truncation never reaches this, so the pre-CB core relied on
         watches staying valid across a suffix cancel and never re-propagated. Under CB we
         cannot: we reset [qhead] to 0 so the next [propagate] re-derives EVERY
         implication over the compacted assignment from scratch. This is unconditionally
         sound — it is plain BCP over the current partial model — and [propagate] itself
         re-establishes each clause's watch (moving the removed literal's watch to a
         non-false literal, forcing the unit, or reporting the conflict via its
         [first]-value check), so no separate watch-repair pass is needed and no unit is
         enqueued mid-backtrack to collide with the caller's [record_learnt]. COST:
         re-scanning the surviving trail's watch lists per chrono backtrack — the Stage-1
         correctness-first choice; a highest-level-watched invariant (Nadel–Ryvchin option
         (b)) is the perf follow-up. *)
      t.qhead <- 0;
      (* THEORY-SEAM REBUILD under CB (task #41 §3.6/§10.5, audit item 6). The scattered
         removal is NOT a clean top-frame suffix, so the frame-count [on_backtrack ~level]
         the suffix arm uses would discard the WRONG theory assertions (a wrong-verdict
         hazard): a theory literal whose TRUE level is <= [level] may have been filed into
         a higher frame under the out-of-order trail, and vice versa. Instead we REBUILD:
         [on_backtrack ~level:0] pops the theory to its base (the pre-solve registrations
         survive — they sit at the base frame), then [on_assign] for each surviving trail
         literal in order re-asserts it (the adapter filters non-atoms and re-registers
         split atoms, so the theory ends holding exactly the survivors). Survivors that
         were theory-PROPAGATED are re-asserted as facts and lose their in-adapter reason
         cache; the snapshot taken above ([t.chrono_reason]) preserves their reasons (F1).
         Sound and simple — it mirrors the [qhead <- 0] Boolean rebuild — and uses only
         the frozen seam callbacks, so cdclt and sat.mli are untouched. CONTRACT-EX stays
         valid: survivors are re-asserted in their compacted trail-position order
         (preserved by the compaction), and [trail_pos] was updated above. COST:
         O(surviving trail) theory re-assertions per chrono backtrack — the Stage-1
         correctness-first choice paired with the [qhead <- 0] cost; incremental
         (earliest-removed) undo is the follow-up. *)
      match t.theory with
      | None -> ()
      | Some th ->
        th.on_backtrack ~level:0;
        Dynarray.iter (fun l -> th.on_assign l) t.trail)
;;

(* ------------------------------------------------------------------ *)
(* Two-watched-literal propagation. Returns the conflicting clause, if any. *)

let propagate t =
  let confl = ref None in
  while !confl = None && t.qhead < Dynarray.length t.trail do
    let p = Dynarray.get t.trail t.qhead in
    t.qhead <- t.qhead + 1;
    t.propagations <- t.propagations + 1;
    (* Clauses in [watches.(p)] watch [neg_lit p], which is now false. The list is two
       parallel unboxed [int] arrays ([wc] crefs, [wb] cached blockers); we sweep entry
       [i] and compact kept entries down to [j]. Every store below is into an [int] array
       — no [caml_modify] write barrier (the arena's point). *)
    let ws = Dynarray.get t.watches p in
    let wc = ws.wc
    and wb = ws.wb in
    let n = Dynarray.length wc in
    let i = ref 0
    and j = ref 0 in
    let false_lit = neg_lit p in
    while !i < n do
      let cr = Dynarray.get wc !i in
      let blk = Dynarray.get wb !i in
      if lit_val t blk = 1
      then (
        (* Clause already satisfied by its blocker; keep the watch untouched. *)
        Dynarray.set wc !j cr;
        Dynarray.set wb !j blk;
        incr i;
        incr j)
      else if cl_deleted t cr
      then incr i (* sweep deleted clause out of the watch list *)
      else (
        (* Ensure the false literal is at slot 1, its partner at slot 0. *)
        if cl_lit t cr 0 = false_lit
        then (
          cl_set_lit t cr 0 (cl_lit t cr 1);
          cl_set_lit t cr 1 false_lit);
        let first = cl_lit t cr 0 in
        (* The refreshed blocker is the new partner [first] (the pre-arena code mutated
           [w.blocker <- first] here; we simply store [first] as the kept/relocated
           entry's blocker below). [old_blocker] preserves the exact partner-satisfied
           condition. *)
        let old_blocker = blk in
        if first <> old_blocker && lit_val t first = 1
        then (
          (* Newly satisfied by the partner watch. *)
          Dynarray.set wc !j cr;
          Dynarray.set wb !j first;
          incr i;
          incr j)
        else (
          (* Look for a non-false literal to watch instead of [false_lit]. *)
          let len = cl_len t cr in
          let k = ref 2 in
          let found = ref false in
          while (not !found) && !k < len do
            if lit_val t (cl_lit t cr !k) <> -1 then found := true else incr k
          done;
          if !found
          then (
            let lk = cl_lit t cr !k in
            cl_set_lit t cr 1 lk;
            cl_set_lit t cr !k false_lit;
            watch_add t (neg_lit lk) ~cref:cr ~blocker:first;
            incr i (* drop from this watch list; now watched elsewhere *))
          else (
            (* No new watch: the clause is unit or conflicting. Keep the watch. *)
            Dynarray.set wc !j cr;
            Dynarray.set wb !j first;
            incr i;
            incr j;
            if lit_val t first = -1
            then (
              confl := Some (H_arena cr);
              (* copy the tail of the watch list unchanged *)
              while !i < n do
                Dynarray.set wc !j (Dynarray.get wc !i);
                Dynarray.set wb !j (Dynarray.get wb !i);
                incr i;
                incr j
              done)
            else unchecked_enqueue t first cr)))
    done;
    Dynarray.truncate wc !j;
    Dynarray.truncate wb !j
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

let transient_clause t lits : tclause = { tid = fresh_id t; tlits = lits }

(* Cert emission (ADR-0013 §4.0): surface a materialized theory transient's id ↔ clause so
   any later citation of it (an [analyze]/[analyze_final] antecedent, or an
   [unsat_conclusion]) resolves to a content-bearing event. [Reason] is the propagation
   clause [p ∨ ¬p₁ ∨ … ∨ ¬pₖ] (implied literal at slot 0); [Conflict] is a falsified
   premise clause. Pure side channel, guarded by the trace: [transient_clause] mints the
   id regardless of trace, so firing this changes nothing when untraced (bit-identical). *)
let note_theory_clause t role (c : tclause) =
  (match t.trace with
   | Some tr -> tr.on_theory_clause ~id:c.tid ~clause:c.tlits ~role
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
  let premises = theory_premises t th lit in
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
  let premises = theory_premises t th lit in
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
  let ants = ref (if track then [ ch_id t confl ] else []) in
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
  (* The level 1UIP resolves down to. Without CB it is the current [decision_level]; under
     CB (task #41 §10.3, Möhle–Biere corrected 1UIP) the conflict clause can be falsified
     BELOW the current level, so it is the MAX literal level in the conflict clause. Both
     coincide in the monotone case (a Boolean/theory conflict always involves a
     current-level literal), so the [not t.chrono] path is byte-identical. Every literal
     reached during resolution is at level <= [conflict_level], so counting
     [>= conflict_level] is the same as [= conflict_level]. *)
  let conflict_level =
    if not t.chrono
    then decision_level t
    else (
      let m = ref 0 in
      for i = 0 to ch_len t confl - 1 do
        let lv = Dynarray.get t.level (var_of_lit (ch_lit t confl i)) in
        if lv > !m then m := lv
      done;
      !m)
  in
  let continue = ref true in
  while !continue do
    (match !c with
     | H_arena cr when cl_learnt t cr ->
       cla_bump t cr;
       (* LBD improvement (Glucose): a learned clause re-derived as a reason may now tie
          together fewer levels; lowering its LBD protects a recently-useful clause from
          reduceDB. Only lower, never raise. *)
       let l = clause_lbd_cref t cr in
       if l < cl_lbd t cr then cl_set_lbd t cr l
     | H_arena _ | H_transient _ -> ());
    let start = if !p = -1 then 0 else 1 in
    for jj = start to ch_len t !c - 1 do
      let q = ch_lit t !c jj in
      let vq = var_of_lit q in
      if (not (Dynarray.get t.seen vq)) && Dynarray.get t.level vq > 0
      then (
        var_bump t vq;
        mark vq;
        if Dynarray.get t.level vq >= conflict_level
        then incr path_c
        else Dynarray.add_last out q)
    done;
    (* Next literal to resolve on: the most recent seen conflict-level literal on the
       trail. Without CB the seen conflict-level literals sit contiguously at the trail
       top, so the plain "most recent seen" walk lands on one — byte-identical. Under CB a
       lower-level seen literal (already routed to [out]) can sit ABOVE conflict-level
       literals, so the walk must skip any seen literal not at [conflict_level]. *)
    if t.chrono
    then
      while
        let v = var_of_lit (Dynarray.get t.trail !index) in
        not (Dynarray.get t.seen v && Dynarray.get t.level v = conflict_level)
      do
        decr index
      done
    else
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
       := let r = Dynarray.get t.reason vp in
          if r >= 0
          then H_arena r
          else if r = r_theory
          then H_transient (theory_reason_clause t pl) (* materialize the lazy reason *)
          else assert false);
      if track then ants := ch_id t !c :: !ants)
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
      let r = Dynarray.get t.reason v in
      let redundant =
        (* [r_decision]: a decision literal is never redundant. [r_theory]: keep
           theory-propagated literals (sound: never over-drop). Otherwise [r >= 0] is the
           [Implied_by] reason clause. *)
        if r < 0
        then false
        else (
          let cr : cref = r in
          let ok = ref true in
          let k = ref 1 in
          while !ok && !k < cl_len t cr do
            let vk = var_of_lit (cl_lit t cr !k) in
            if (not (Dynarray.get t.seen vk)) && Dynarray.get t.level vk > 0
            then ok := false;
            incr k
          done;
          !ok)
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
    (* Walk start + level-0 skip (F2, corrected by F-core). Without CB the level-0
       literals are the contiguous prefix [\[0, trail_lim.(0))], so starting the walk at
       [trail_lim.(0)] skips them. Under CB [trail_lim] POSITION entries are stale (a
       level-0 literal can land AFTER a level>0 one on the out-of-order trail), so we scan
       the WHOLE trail but SKIP level-0 literals in the body — the exact same immunity the
       non-CB start gives, restored by a per-literal test. This skip is load-bearing, NOT
       a no-op: a level-0 UNIT is enqueued with reason [Decision] ([add_clause] / a
       learned unit in [record_learnt]), so without the guard the [Decision] arm would
       append it to [out] and [failed_assumptions] would return a SUPERSET of the
       assumptions — a violation of the frozen sat.mli subset contract (verdict
       unaffected; the initial [p] is the true failed assumption). Level-0 facts are
       unconditional (derivable from the clause set alone), never part of an assumption
       core, so skipping them is correct; the [Implied_by]/[Theory_prop] arms already mark
       only [level > 0] premises. *)
    let start = if t.chrono then 0 else Dynarray.get t.trail_lim 0 in
    for i = Dynarray.length t.trail - 1 downto start do
      let l = Dynarray.get t.trail i in
      let v = var_of_lit l in
      if Dynarray.get t.seen v && ((not t.chrono) || Dynarray.get t.level v > 0)
      then (
        let r = Dynarray.get t.reason v in
        if r = r_decision
        then Dynarray.add_last out (neg_lit l)
        else if r >= 0
        then (
          (* [Implied_by cr] *)
          let cr : cref = r in
          for j = 1 to cl_len t cr - 1 do
            let vj = var_of_lit (cl_lit t cr j) in
            if Dynarray.get t.level vj > 0 then mark vj
          done;
          if track then ants := cl_id t cr :: !ants)
        else (
          (* [r_theory]: a theory-propagated literal's premises are its reason; mark them
             (mirrors the [Implied_by] clause body, whose slot 0 is [l] itself and is
             skipped). Same strict CONTRACT-EX validation as the 1UIP path — a
             precedence-violating reason here would silently produce a wrong
             failed-assumption core, so it must raise. *)
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
            ants := c.tid :: !ants)))
    done;
    Dynarray.iter (fun v -> Dynarray.set t.seen v false) marked);
  List.map neg_lit (Array.to_list (Dynarray.to_array out)), !ants
;;

(* ------------------------------------------------------------------ *)
(* Learned-clause deletion. A clause is locked if it is the reason for its currently-true
   asserting literal. We drop roughly the least-active half of the unlocked, non-binary
   learned clauses. Deleted clauses are swept out of watch lists lazily during
   propagation. *)

let locked t (cr : cref) =
  let l0 = cl_lit t cr 0 in
  lit_val t l0 = 1
  &&
  (* the reason of [l0]'s var is [Implied_by cr] iff that reason int equals [cr] (was the
     physical-equality [rc == c]). *)
  let r = Dynarray.get t.reason (var_of_lit l0) in
  r >= 0 && r = cr
;;

(* Learned-clause deletion + ARENA RELOCATION (the crux, spec CRUX 1). Selects the learnts
   to drop (unchanged: protect glue / locked / binary; delete the worst half by LBD desc,
   activity asc — {!Search_heuristics.reduce_deletions}), then REBUILDS a fresh arena from
   the kept clauses and remaps every live cref.

   Kept order (deterministic ⇒ counted-identity: crefs are never observed in
   verdict/counters/cert, only ids are, and ids are preserved in [a_id]): the originals
   [t.clauses] in order, then the surviving learnts [t.learnts] in order. New crefs are
   0,1,… in that order. The remap ([old_cref -> new_cref], -1 = dropped) then rewrites
   BOTH live-cref holder classes in one pass:
   1. the [reason] array — every [Implied_by cr]; a locked clause (the reason of a live
      level>0 asserting literal) is PROTECTED from drop, so its remap is >=0 (fail-closed:
      raise if a live one was dropped). A dropped cref can only be a STALE reason never
      read — a level-0 unit's, or (defensively) an unassigned var's — which is reset to
      [r_decision] (level-0/unassigned reasons are never dereferenced: [analyze]/
      [analyze_final] skip level 0, [locked] ranges over learnts, [cancel_until] resets on
      unwind). Such stale reasons arise only under SATPRE inprocessing, which re-creates
      originals; the OFF path never drops a reason.
   2. every watch list — remap the cref, DROP entries whose clause was dropped (matches
      the old lazy deleted-sweep; live entries keep their relative order, so propagation
      order is unchanged), blockers unchanged. *)
let reduce_db t =
  let learnt_arr = Dynarray.to_array t.learnts in
  let stats =
    Array.map
      (fun cr ->
         { Search_heuristics.lbd = cl_lbd t cr
         ; activity = cl_act t cr
         ; protected_ = locked t cr || cl_len t cr <= 2
         })
      learnt_arr
  in
  let del = Search_heuristics.reduce_deletions stats in
  Array.iteri (fun i cr -> if del.(i) then cl_set_deleted t cr) learnt_arr;
  (* Kept clauses in deterministic order: originals, then surviving learnts. *)
  let kept_orig = Dynarray.to_array t.clauses in
  let kept_learnt =
    Array.of_seq (Seq.filter (fun cr -> not (cl_deleted t cr)) (Array.to_seq learnt_arr))
  in
  let n_orig = Array.length kept_orig in
  let total = n_orig + Array.length kept_learnt in
  let kept = Array.make (max 1 total) 0 in
  Array.blit kept_orig 0 kept 0 n_orig;
  Array.blit kept_learnt 0 kept n_orig (Array.length kept_learnt);
  (* Snapshot each kept clause's data BEFORE clearing the arena, and build the remap. *)
  let old_size = Dynarray.length t.a_off in
  let remap = Array.make (max 1 old_size) (-1) in
  let snap_lits = Array.init total (fun i -> cl_lits t kept.(i)) in
  let snap_id = Array.init total (fun i -> cl_id t kept.(i)) in
  let snap_lbd = Array.init total (fun i -> cl_lbd t kept.(i)) in
  let snap_act = Array.init total (fun i -> cl_act t kept.(i)) in
  let snap_learnt = Array.init total (fun i -> cl_learnt t kept.(i)) in
  for i = 0 to total - 1 do
    remap.(kept.(i)) <- i
  done;
  Dynarray.clear t.a_lits;
  Dynarray.clear t.a_off;
  Dynarray.clear t.a_len;
  Dynarray.clear t.a_id;
  Dynarray.clear t.a_lbd;
  Dynarray.clear t.a_act;
  Dynarray.clear t.a_flags;
  Dynarray.clear t.clauses;
  Dynarray.clear t.learnts;
  for i = 0 to total - 1 do
    let off = Dynarray.length t.a_lits in
    Array.iter (fun l -> Dynarray.add_last t.a_lits l) snap_lits.(i);
    Dynarray.add_last t.a_off off;
    Dynarray.add_last t.a_len (Array.length snap_lits.(i));
    Dynarray.add_last t.a_id snap_id.(i);
    Dynarray.add_last t.a_lbd snap_lbd.(i);
    Dynarray.add_last t.a_act snap_act.(i);
    Dynarray.add_last t.a_flags (if snap_learnt.(i) then 1 else 0);
    if snap_learnt.(i)
    then Dynarray.add_last t.learnts i
    else Dynarray.add_last t.clauses i
  done;
  (* 1. reason array. *)
  for v = 0 to t.nvars - 1 do
    let r = Dynarray.get t.reason v in
    if r >= 0
    then (
      let nr = remap.(r) in
      if nr >= 0
      then Dynarray.set t.reason v nr
      else (
        if Dynarray.get t.assigns v <> 0 && Dynarray.get t.level v > 0
        then
          failwith
            "Sat.reduce_db: a live (level>0) Implied_by reason clause was dropped — \
             locked invariant violated";
        Dynarray.set t.reason v r_decision))
  done;
  (* 2. watch lists. *)
  Dynarray.iter
    (fun ws ->
       let wc = ws.wc
       and wb = ws.wb in
       let n = Dynarray.length wc in
       let j = ref 0 in
       for i = 0 to n - 1 do
         let nr = remap.(Dynarray.get wc i) in
         if nr >= 0
         then (
           Dynarray.set wc !j nr;
           Dynarray.set wb !j (Dynarray.get wb i);
           incr j)
       done;
       Dynarray.truncate wc !j;
       Dynarray.truncate wb !j)
    t.watches
;;

(* ------------------------------------------------------------------ *)
(* Permanent clause addition with level-0 simplification. Only legal at decision level 0
   (guaranteed by [solve], which cancels to 0 before returning). *)

(* Restore-on-mention (the note's "a new clause containing ¬l restores clauses deleted on
   l", generalized to any mention of the pivot var — conservatively sound, restores a
   superset). Un-eliminates [v]: re-decision-eligible, and every clause deleted for it is
   re-added through {!add_clause} (so it is level-0-simplified afresh). Cascades — a
   restored clause may name another eliminated var — and terminates because each call
   removes [v] from [restore_map] first. The stale [elim_stack] entries for [v] are
   harmless: the clause is back in the DB and satisfied by any model, so the
   flip-to-satisfy walk never fires on it. In oxsmt this path is unreachable (aux vars are
   per-formula fresh, never named by a later clause, and theory lemmas hold only frozen
   theory atoms) — it is here for the general incremental contract. Forward reference to
   [add_clause] via a ref, set below. *)
let add_clause_ref : (?origin:origin -> t -> lit list -> unit) ref =
  ref (fun ?origin:_ _ _ -> assert false)
;;

let rec restore_eliminated t v =
  match Hashtbl.find_opt t.restore_map v with
  | None ->
    (* An eliminated var with no [restore_map] entry is an ELS-substituted var
       ([t.equiv]): its equivalence-establishing clauses were rewritten away, so
       reactivating it soundly needs the incremental-ELS reactivation machinery
       (Fazekas–Biere–Scholl SAT'19) that is not built. This cannot happen in oxsmt — ELS
       eliminates only per-formula-fresh aux vars, which no later clause names — so make
       the contract violation LOUD rather than a silent wrong result. *)
    if Hashtbl.mem t.equiv v
    then
      invalid_arg
        "Sat.add_clause: a clause names a variable eliminated by equivalent-literal \
         substitution (A10); ELS reactivation is unsupported"
  | Some clauses ->
    Hashtbl.remove t.restore_map v;
    Dynarray.set t.eliminated v false;
    heap_insert t v;
    List.iter
      (fun cl ->
         Array.iter
           (fun l ->
              let w = var_of_lit l in
              if Dynarray.get t.eliminated w then restore_eliminated t w)
           cl;
         !add_clause_ref t (Array.to_list cl))
      clauses
;;

let add_clause ?(origin = Query) t lits =
  List.iter (fun l -> ensure_var t (var_of_lit l)) lits;
  (* A10 restore hook: if any literal names an already-eliminated var, restore it first so
     the elimination stays sound (BVE) or fail loud (ELS-substituted; see
     {!restore_eliminated}). Guarded on non-empty [restore_map]/[equiv] so an untouched
     core (OXSMT_SATPRE off) pays nothing. *)
  if Hashtbl.length t.restore_map > 0 || Hashtbl.length t.equiv > 0
  then
    List.iter
      (fun l ->
         let v = var_of_lit l in
         if Dynarray.get t.eliminated v then restore_eliminated t v)
      lits;
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
            unchecked_enqueue t l r_decision;
            (* a standing level-0 unit (ADR-0013 §1.3): declared to the checker, which
               also re-derives the unit closure from the [Input] clauses by BCP. The
               declaration is thus redundant; [base_l0_cert_mode] (default [false] emits
               it, trunk-identical) SUPPRESSES it under base-l0 — see the field doc + the
               E3 route in [handle_confl] (#53). *)
            if not t.base_l0_cert_mode
            then (
              match t.trace with
              | Some tr -> tr.on_unit ~id:(fresh_id t) ~lit:l
              | None -> ()))
        | _ ->
          let id = if input_id >= 0 then input_id else fresh_id t in
          let c = mk_clause_with_id t id (Array.of_list ls) false in
          attach t c)))
;;

(* Wire the forward reference used by {!restore_eliminated} (A10). *)
let () = add_clause_ref := add_clause

(* ------------------------------------------------------------------ *)
(* Branching and search. *)

(* Pick the highest-activity unassigned variable to branch on, as a signed literal under
   phase saving. With no branch-filter installed this is the classic loop: pop the VSIDS
   max, skip already-assigned vars, decide the first unassigned one — and
   [t.branch_filter = None] takes exactly that path (bit-identical, sat.mli
   set_branch_filter).

   With a filter installed, an unassigned var the filter rejects (currently irrelevant) is
   NOT decided: it is stashed and re-inserted into the heap after the pick, so it stays a
   candidate for when a later relevancy mark makes [filter] accept it (no backtrack
   needed). [None] is returned when the heap holds only assigned or filtered-out vars — a
   complete assignment over the branchable vars — which the caller hands to the Final
   check exactly as it does on an exhausted heap. The stash is re-inserted on every exit
   path, so the filter only reorders WHICH candidate is picked; it never drops a var from
   the order. *)
let pick_branch t =
  match t.branch_filter with
  | None ->
    let rec go () =
      match heap_remove_max t with
      | None -> None
      | Some v ->
        (* An eliminated var (A10) is in no clause; drop it from the order permanently
           (never re-inserted) so it is never decided — its model value is reconstructed
           in [save_model]. [eliminated] is all-false unless preprocessing ran, so this is
           bit-identical when OXSMT_SATPRE is off. *)
        if Dynarray.get t.assigns v = 0 && not (Dynarray.get t.eliminated v)
        then Some (if Dynarray.get t.polarity v then neg v else pos v)
        else go ()
    in
    go ()
  | Some filter ->
    let stashed = ref [] in
    (* The var just popped from the heap and not yet either stashed or returned. Held so
       the [finally] below re-inserts it on ANY exit — crucially if [filter] RAISES while
       deciding it: [heap_remove_max] already set its [heap_pos] to -1, so without this it
       would be lost from the heap and, being untrailed, NOT restored by [cancel_until 0]
       — a later filter-free solve on the same core could then return a model omitting it
       and falsifying a clause over it (a wrong-SAT reachable from the public API; codex
       S1 finding). [-1] means "none in flight". *)
    let in_flight = ref (-1) in
    let reinsert () =
      (* Re-insert the in-flight var (if the filter raised mid-decision) and every stashed
         (unassigned, filtered-out) var, so every var popped in this call stays in the
         activity order. [heap_insert] no-ops a var already present; [heap_remove_max] set
         each popped var's [heap_pos] to -1, so this restores them. Runs on the normal
         exit too (where [in_flight = -1], a no-op) — behaviourally identical to the
         pre-fix re-insertion when [filter] does not raise. *)
      if !in_flight >= 0
      then (
        heap_insert t !in_flight;
        in_flight := -1);
      List.iter (fun v -> heap_insert t v) !stashed
    in
    let rec go () =
      match heap_remove_max t with
      | None -> None
      | Some v ->
        if Dynarray.get t.assigns v <> 0 || Dynarray.get t.eliminated v
        then go () (* already assigned or eliminated (A10): drop, as the no-filter loop *)
        else (
          (* [v] is popped: own it via [in_flight] across the (untrusted,
             possibly-raising) [filter] call, so the [finally] re-inserts it if [filter]
             raises here. *)
          in_flight := v;
          if not (filter v)
          then (
            in_flight := -1;
            (* unassigned but currently irrelevant: keep it as a future candidate *)
            stashed := v :: !stashed;
            go ())
          else (
            (* decided: it becomes the branch literal, enqueued (hence assigned, off-heap)
               by the caller — so it is NOT re-inserted. *)
            in_flight := -1;
            Some (if Dynarray.get t.polarity v then neg v else pos v)))
    in
    Fun.protect ~finally:reinsert go
;;

(* Signed value of a literal under [saved_model] (1 true, -1 false, 0 unknown), for the
   A10 reconstruction — [lit_val] reads [t.assigns], but at save time we walk the SNAPSHOT
   because reconstruction mutates it. *)
let saved_lit_val t l =
  let s = Dynarray.get t.saved_model (var_of_lit l) in
  if s = 0 then 0 else if sign_of_lit l then s else -s
;;

(* Take the model snapshot; then, if variable elimination ran (A10), reconstruct the
   eliminated variables. Per the note's clause-deletion form (Lemma 1): an eliminated var
   is unassigned in [assigns], so it enters the snapshot with a default of FALSE; then we
   pop the deletion stack in REVERSE and, for each (deleted clause, pivot literal) whose
   clause the reduced model leaves unsatisfied, force the pivot literal true. Reverse
   order is essential — the last elimination is undone first — and correctness rests on
   the added resolvents (permanent, hence satisfied) guaranteeing no two same-var pivots
   demand opposite values. With no elimination [elim_stack] is empty, so this is exactly
   the old snapshot (bit-identical when OXSMT_SATPRE is off). *)
let save_model t =
  Dynarray.clear t.saved_model;
  for v = 0 to t.nvars - 1 do
    let a = Dynarray.get t.assigns v in
    (* an eliminated var is never on the trail (never decided/propagated): default it
       FALSE in the snapshot, to be fixed up by the flip-to-satisfy walk below *)
    let a = if a = 0 && Dynarray.get t.eliminated v then -1 else a in
    Dynarray.add_last t.saved_model a
  done;
  for i = Dynarray.length t.elim_stack - 1 downto 0 do
    let cl, piv = Dynarray.get t.elim_stack i in
    let satisfied = Array.exists (fun l -> saved_lit_val t l = 1) cl in
    if not satisfied
    then Dynarray.set t.saved_model (var_of_lit piv) (if sign_of_lit piv then 1 else -1)
  done;
  (* ELS reconstruction (A10): definitional [x := value(rep)]. Runs AFTER the flip stack
     so every representative's value is final (if a rep was later BVE-eliminated its flip
     entry above already fixed it). Empty map unless ELS eliminated something.

     Chain resolution: [t.equiv] PERSISTS across solves, so a representative chosen in one
     solve (recording [x := +r]) can itself be ELS-eliminated in a later solve (recording
     [r := +s]) — a chain [x → r → s]. A single-hop read under this unordered
     [Hashtbl.iter] would, if it visited [x] before [r], read [r]'s pre-reconstruction
     default rather than its resolved value, yielding an order-dependent wrong model. So
     resolve each var to its ULTIMATE representative literal before reading. TERMINATES:
     an ELS target is a var that is not-yet-eliminated when the entry is recorded, and
     elimination is monotone (an eliminated var's clauses are gone from later rounds'
     implication graph, so it is never a later round's representative), so the walk visits
     strictly-distinct keys and ends at a var with no [equiv] entry (a true
     representative, whose snapshot value is final). *)
  let rec resolve l =
    match Hashtbl.find_opt t.equiv (var_of_lit l) with
    | None -> l
    | Some m -> resolve (if sign_of_lit l then m else neg_lit m)
  in
  if Hashtbl.length t.equiv > 0
  then
    Hashtbl.iter
      (fun x l ->
         Dynarray.set t.saved_model x (if saved_lit_val t (resolve l) = 1 then 1 else -1))
      t.equiv
;;

let record_learnt t learnt bt ants lbd =
  if Array.length learnt = 1
  then (
    unchecked_enqueue t learnt.(0) r_decision;
    match t.trace with
    | Some tr ->
      tr.on_learned ~id:(fresh_id t) ~clause:learnt ~antecedents:ants ~btlevel:bt
    | None -> ())
  else (
    let c = mk_clause t learnt true in
    cl_set_lbd t c lbd;
    attach t c;
    cla_bump t c;
    unchecked_enqueue t learnt.(0) c (* [Implied_by c]: the cref IS the reason *);
    match t.trace with
    | Some tr ->
      tr.on_learned ~id:(cl_id t c) ~clause:learnt ~antecedents:ants ~btlevel:bt
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
         unchecked_enqueue t l r_theory;
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
         then ignore (theory_reason_clause t l : tclause);
         go true rest
       | _ ->
         `Confl (H_transient (theory_prop_conflict_clause t l))
         (* forced true but already false *))
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
  (* Returns [Some (clause, is_theory)]: [is_theory] distinguishes a BOOLEAN BCP conflict
     ([propagate]) from a THEORY conflict (T_conflict / a falsified theory reason / an
     empty-at-level-0 lemma). The flag drives the level-0 terminal choice in
     [handle_confl] (base #53): a level-0 theory conflict routes to an empty-core E3, not
     E2. *)
  let confl = ref None in
  let again = ref true in
  while !again do
    again := false;
    match propagate t with
    | Some c -> confl := Some (c, false)
    | None ->
      (match t.theory with
       | None -> ()
       | Some th ->
         (match th.check ~final:false with
          | T_consistent [] -> ()
          | T_consistent lits ->
            (match enqueue_theory_lits t lits with
             | `Confl c -> confl := Some (c, true)
             | `Progress p -> again := p)
          | T_conflict premises ->
            confl := Some (H_transient (theory_conflict_clause t premises), true)
          | T_lemma clauses ->
            (* D3: Split is a Final-effort result; a Propagate-effort lemma is a contract
               deviation but still sound to add, so we accept it and re-propagate. *)
            add_theory_lemmas t clauses;
            (* a lemma that simplified to the empty clause at level 0 makes the instance
               unsat; surface it as an (empty, always-false) conflict so [handle_confl]
               concludes unsat rather than letting search run on to a spurious model *)
            if t.ok
            then again := true
            else
              confl
              := Some
                   ( H_transient (note_theory_clause t Conflict (transient_clause t [||]))
                   , true )))
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
  let handle_confl ~theory confl =
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
    (* Post-analyze bookkeeping shared by both modes: the LBD EMAs (seeded on the first
       conflict of the solve, codex M2), the restart warm-up counter, the learned clause,
       the activity decays, and the conflict-count reduceDB schedule. [cancel] chooses the
       backtrack (standard [bt] vs the chrono target). *)
    let learn learnt bt ants ~cancel =
      let lbd = clause_lbd t learnt in
      let lbd_f = float_of_int lbd in
      t.lbd_ema_fast
      <- (if first_conflict
          then lbd_f
          else ema_step t.lbd_ema_fast ~alpha:lbd_ema_alpha_fast ~sample:lbd_f);
      t.lbd_ema_slow
      <- (if first_conflict
          then lbd_f
          else ema_step t.lbd_ema_slow ~alpha:lbd_ema_alpha_slow ~sample:lbd_f);
      t.conflicts_since_restart <- t.conflicts_since_restart + 1;
      cancel ();
      record_learnt t learnt bt ants lbd;
      var_decay_bump t;
      cla_decay_bump t;
      (* LBD-based reduceDB on the conflict-count schedule (decoupled from restarts). *)
      if t.conflicts >= t.next_reduce
      then (
        reduce_db t;
        t.next_reduce <- t.next_reduce + reduce_inc)
    in
    let conclude_unsat () =
      t.ok <- false;
      (* ADR-0013 §4.0 terminal for a level-0 conflict.
         - BOOLEAN level-0 conflict -> E2 [Level0_conflict]: [confl] is a real clause
           whose literals are ALL false in the level-0 closure, so the checker falsifies
           it directly (it does not self-propagate). [confl.id] resolves via [on_input] /
           [on_learned].
         - THEORY level-0 conflict, UNDER BASE-L0 CERT MODE -> E3 [Failed_assumption] with
           an EMPTY assumption core (base #53). The theory conflict clause is a valid
           T-lemma with all-but-one literals already false at level 0; added to the
           checker's closure as an axiom it UNIT-PROPAGATES its last literal
           (self-satisfying) instead of being falsified in a consistent closure — so the
           E2 [falsified] test cannot see it and the closure is contradictory. E3's
           [refutes_under] over the whole DB (which includes the theory leaf) derives ⊥ BY
           CONSTRUCTION — it asks exactly "does BCP refute the assumptions", and a
           contradictory DB refutes the empty set. This is how the pre-base-l0 build
           certified these (the base ASSUMPTION made them E3); base-l0 removed the
           assumption but the refutation is still an empty-core E3. No antecedents are
           cited (the DB carries the refutation; the E3 antecedent list is a RUP hint and
           [] is valid), so no [Ktheory Conflict] id reaches the checker's Reason-only E3
           allow-list.

         GATE (codex #53 bounce): the E3 route fires ONLY in base-l0 cert mode — signalled
         by [t.base_l0_cert_mode] (the session sets that true under OXSMT_BASE_L0; see
         [Sat.create]). Strict OFF (the default, [base_l0_cert_mode = false]) keeps a
         level-0 THEORY conflict on the pre-existing E2 route, so the raw-Sat layer is
         BYTE-IDENTICAL to trunk BY CONSTRUCTION — not merely by the reachability argument
         that the product OFF path (base assumed at L1) never reaches a level-0 theory
         conflict. OFF's E2 on such a conflict is the pre-#53 behaviour (fail-safe INVALID
         if a checker ever saw it; unreachable from Session OFF). *)
      if t.trace <> None
      then
        t.terminal
        <- Some
             (if theory && t.base_l0_cert_mode
              then Failed_assumption { antecedents = [] }
              else Level0_conflict { conflict_id = ch_id t confl });
      emit_terminal t;
      result := Some R_unsat
    in
    if not t.chrono
    then (
      (* ---- standard, byte-identical to the pre-CB core ---- *)
      if t.theory <> None
      then (
        let maxl = ref 0 in
        for i = 0 to ch_len t confl - 1 do
          let lv = Dynarray.get t.level (var_of_lit (ch_lit t confl i)) in
          if lv > !maxl then maxl := lv
        done;
        if !maxl < decision_level t then cancel_until t !maxl);
      if decision_level t = 0
      then conclude_unsat ()
      else (
        let learnt, bt, ants = analyze t confl in
        learn learnt bt ants ~cancel:(fun () -> cancel_until t bt)))
    else (
      (* ---- chronological backtracking (task #41 §10.4) ---- *)
      (* No pre-cancel realignment: [analyze] works natively at the conflict clause's max
         level, which under CB may be below the current decision level. *)
      let conflict_level =
        let m = ref 0 in
        for i = 0 to ch_len t confl - 1 do
          let lv = Dynarray.get t.level (var_of_lit (ch_lit t confl i)) in
          if lv > !m then m := lv
        done;
        !m
      in
      if conflict_level = 0
      then conclude_unsat ()
      else (
        let learnt, bt, ants = analyze t confl in
        (* Learned units (bt = 0) always take a NORMAL backjump to 0 (§10.1: units settle
           at level 0). Otherwise backtrack chronologically to [conflict_level - 1] unless
           the gap exceeds the threshold, when a standard backjump to [bt] is worth it
           (Nadel–Ryvchin: chronological iff [conflict_level - bt <= chrono_threshold]).
           The asserting literal lands at level [bt] regardless, via [enqueue_level]
           (§10.1). *)
        let is_unit = Array.length learnt = 1 in
        let gap = conflict_level - bt in
        let target =
          if is_unit || gap > t.chrono_threshold then bt else conflict_level - 1
        in
        learn learnt bt ants ~cancel:(fun () -> cancel_until t target)))
  in
  while !result = None do
    match propagate_theory t with
    | Some (confl, theory) -> handle_confl ~theory confl
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
              unchecked_enqueue t l r_decision;
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
                     | `Confl c -> handle_confl ~theory:true c
                     | `Progress true -> () (* re-check at the new fixpoint *)
                     | `Progress false ->
                       save_model t;
                       result := Some R_sat)
                  | T_conflict premises ->
                    handle_confl
                      ~theory:true
                      (H_transient (theory_conflict_clause t premises))
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
            unchecked_enqueue t !next r_decision;
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

(* ------------------------------------------------------------------ *)
(* CNF preprocessing (DESIGN.md A10; Jacobs 2021 "Bounded clause elimination"). Env-gated
   (OXSMT_SATPRE), OFF when a certificate trace is installed, run at [solve] entry (level
   0). Two passes over a working copy of the original clauses: forward subsumption (delete
   a clause another clause subsumes) then bounded variable elimination on ELIMINABLE vars
   only (the note's [elim]: add all non-tautological resolvents, delete the pivot's
   clauses, record them on the reconstruction stack). The clause DB is rebuilt from the
   survivors.

   Phase 1 restricts to the pre-search state: it runs only with NO learned clauses (so a
   var we eliminate is guaranteed absent from every clause — a learned clause referencing
   an eliminated var would be a wrong-Unsat risk since it is no longer entailed by the
   reduced set). Phase 2 (inprocessing) will re-run a learnt-aware engine at restart level
   0. *)

(* A working clause: a deduped, ascending-sorted literal array plus a death flag. [wl] is
   REPLACED (never mutated in place) when self-subsuming resolution strengthens the clause
   — always before BVE, so the value the reconstruction stack later aliases is the final
   one. *)
type wclause =
  { mutable wl : lit array
  ; mutable wdead : bool
  }

(* Membership of literal [x] in ascending sorted array [a]. *)
let sorted_mem x a =
  let lo = ref 0
  and hi = ref (Array.length a - 1)
  and found = ref false in
  while (not !found) && !lo <= !hi do
    let mid = (!lo + !hi) / 2 in
    let v = a.(mid) in
    if v = x then found := true else if v < x then lo := mid + 1 else hi := mid - 1
  done;
  !found
;;

let bve_size_cap = 20 (* skip a resolvent longer than this *)
let bve_product_cap = 64 (* skip a var whose |pos|*|neg| exceeds this (bound the work) *)
let bve_margin = 0 (* eliminate only if #resolvents <= #deleted + margin (classic BVE) *)
let vivify_size_cap = 30 (* skip vivifying a learned clause longer than this *)
let vivify_budget = 2000 (* max learned clauses vivified per round (effort bound) *)

(* Is sorted array [a] a subset of sorted array [b] (both ascending, deduped)? Linear
   merge. *)
let sorted_subset a b =
  let la = Array.length a
  and lb = Array.length b in
  if la > lb
  then false
  else (
    let i = ref 0
    and j = ref 0 in
    while !i < la && !j < lb && a.(!i) >= b.(!j) do
      if a.(!i) = b.(!j)
      then (
        incr i;
        incr j)
      else incr j (* b.(!j) < a.(!i): skip it *)
    done;
    !i = la)
;;

(* Resolvent of [a] (contains literal [pos v]) and [b] (contains [neg v]) on variable [v]:
   the union of [a\{pos v}] and [b\{neg v}], deduped and sorted. [None] if it is a
   tautology (contains some literal and its negation). *)
let resolve_on a b v =
  let pv = pos v
  and nv = neg v in
  let seen = Hashtbl.create 16 in
  let add l = if l <> pv && l <> nv then Hashtbl.replace seen l () in
  Array.iter add a;
  Array.iter add b;
  let taut =
    Hashtbl.fold (fun l () acc -> acc || Hashtbl.mem seen (neg_lit l)) seen false
  in
  if taut
  then None
  else (
    let lits = Hashtbl.fold (fun l () acc -> l :: acc) seen [] in
    let arr = Array.of_list lits in
    Array.sort compare arr;
    Some arr)
;;

(* Vivification / distillation of a learned clause [c] (the charter's win-bearing round
   component). At decision level 0 with NO theory plugged: walk [c]'s literals, assuming
   the negation of each (as a decision) and BCP-ing. Three outcomes shorten [c] to an
   entailed sub-clause:
   - a literal is already TRUE under the assumed prefix ⇒ the prefix entails it;
   - assuming its negation CONFLICTS ⇒ the prefix + it is entailed (F ∧ ¬prefix unsat);
   - a literal is already FALSE ⇒ it is redundant given the prefix, so it is dropped (the
     assumed prefix stays the sub-clause). The produced sub-clause is a SUBSET of [c]'s
     literals, so it implies [c]; and it is entailed by the formula (the conflict/forced
     criteria are exactly the RUP/asymmetric entailment conditions). Replacing [c] by it
     is therefore equivalence-preserving — and because it touches only a LEARNED
     (redundant) clause, the reconstruction/witness stack is untouched. Returns the
     shortened array (>= 2 literals, strictly shorter than [c]) or [None]; restores the
     trail to level 0 on every exit.

   Gated to [t.theory = None] by the caller: assuming a literal fires [on_assign] to a
   plugged theory, so on the theory-plugged path this would drive theory asserts/pops it
   is not yet audited for — the pure-propositional / bit-blasted path (bv's own solver) is
   the target and where the literature says vivification pays. Refs: Piette, Hamadi &
   Saïs, "Vivifying propositional clausal formulas" (ECAI 2008); the CDCL inprocessing
   form (CaDiCaL/Kissat). *)
let vivify_learnt t (cr : cref) =
  let lits = cl_lits t cr in
  let n = Array.length lits in
  let kept = Dynarray.create () in
  let shortened =
    ref false
    (* set once a conflict / forced-true proves entailment *)
  in
  let i = ref 0 in
  let stop = ref false in
  while (not !stop) && !i < n do
    let li = lits.(!i) in
    (match lit_val t li with
     | 1 ->
       (* prefix already entails [li]: sub-clause = assumed prefix ++ [li] *)
       Dynarray.add_last kept li;
       shortened := true;
       stop := true
     | -1 ->
       () (* [li] redundant under the assumed prefix: drop it (not assumed, not kept) *)
     | _ ->
       new_decision_level t;
       unchecked_enqueue t (neg_lit li) r_decision;
       (match propagate t with
        | Some _ ->
          Dynarray.add_last kept li;
          shortened := true;
          stop := true
        | None -> Dynarray.add_last kept li));
    incr i
  done;
  cancel_until t 0;
  if !shortened && Dynarray.length kept >= 2 && Dynarray.length kept < n
  then Some (Dynarray.to_array kept)
  else None
;;

let flp_probe_budget = 500 (* variables probed per round (effort bound) *)

(* Failed-literal probing, the last cheap charter component. For a candidate literal [l]:
   assume it (a decision) and BCP; if that conflicts then [F ∧ l] is unsat, so [¬l] is
   entailed and is enqueued as a level-0 unit and propagated. Reuses the exact
   assume/propagate/[cancel_until] trail machinery of {!vivify_learnt} (the
   team-lead-noted overlap). Derives FORCED units — the variable is assigned, not
   eliminated — so there is NO reconstruction. Gated to [t.theory = None] by the caller
   (assuming a literal fires [on_assign] into a plugged theory seam, the same reason
   vivification is gated), and effort-bounded. On a forced unit whose level-0 propagation
   conflicts, the formula is unsat: set [t.ok] false — the round's callers ([solve]/[go])
   conclude [Unsat]. Ref: the classic failed-literal / probing preprocessing (e.g. Le
   Berre; Lynce/Marques-Silva). *)
let failed_literal_probing t =
  (* returns [true] if assuming [lit] conflicts under BCP (⇒ [¬lit] entailed); restores L0 *)
  let probe lit =
    new_decision_level t;
    unchecked_enqueue t lit r_decision;
    let c = propagate t in
    cancel_until t 0;
    c <> None
  in
  let budget = ref flp_probe_budget in
  let v = ref 0 in
  while !budget > 0 && !v < t.nvars && t.ok do
    let vv = !v in
    incr v;
    if Dynarray.get t.assigns vv = 0 && not (Dynarray.get t.eliminated vv)
    then (
      decr budget;
      (* a forced literal to enqueue at level 0, if either polarity is a failed literal *)
      let forced =
        if probe (pos vv)
        then Some (neg vv)
        else if
          (* re-check: probing [pos vv] enqueued nothing (it restored L0), so [vv]
                   is still free unless a prior iteration's forced unit propagated onto it *)
          Dynarray.get t.assigns vv = 0 && probe (neg vv)
        then Some (pos vv)
        else None
      in
      match forced with
      | None -> ()
      | Some u ->
        if lit_val t u = 0
        then (
          unchecked_enqueue t u r_decision;
          t.stat_flp <- t.stat_flp + 1);
        (match propagate t with
         | Some _ -> t.ok <- false (* the forced unit closes a level-0 conflict: unsat *)
         | None -> ()))
  done
;;

(* Equivalent-literal substitution (ELS), a purely propositional round component. Literals
   in one strongly connected component of the BINARY-IMPLICATION GRAPH (edges [¬a → b] and
   [¬b → a] for each binary clause [a ∨ b]) are all logically equivalent; substituting
   each equivalent literal by one representative merges the variables. This function
   analyses [work]'s binary clauses, finds the SCCs (iterative Tarjan — deterministic, no
   recursion depth risk, I6), and, when safe, rewrites [work] to substitute every
   ELIMINABLE non-representative variable onto its representative. The representative is
   chosen FROZEN if the SCC has a frozen variable (so eliminable vars map onto a stable
   survivor), else the lowest literal.

   Soundness: [x ↔ rep] holds in the formula, so substituting is equivalence-preserving;
   the eliminated [x] is reconstructed definitionally ([x := value(rep)], via {!t.equiv},
   applied after the flip stack in [save_model]). Within a round a representative is never
   itself ELS-eliminated, but [t.equiv] persists across rounds/solves, so a representative
   chosen in one round can be ELS-eliminated in a later one — a cross-round chain that
   [save_model] resolves to a fixpoint (see there). Only ELIMINABLE vars are substituted
   (frozen theory/model vars stay). ELS's only trail effect is enqueuing FORCED level-0
   units (a rewritten clause collapsing to a unit); it never opens a decision level or
   backtracks (unlike vivification / failed-literal probing), so there is no
   assume-under-a- plugged-theory hazard and it runs on every path. Those enqueues do fire
   [on_assign] into any plugged theory seam — benign, exactly as a level-0 propagation
   would, and ELS never substitutes a frozen theory-atom var — and they are propagated to
   closure at the tail of [run_round] before any decision-level-opening component runs.

   To keep the round's [ok] invariant simple, a DRY RUN first classifies the rewritten
   clauses; if any would become the EMPTY clause or a level-0-conflicting unit (i.e. the
   substitution exposes unsat), ELS is SKIPPED ENTIRELY this round and ordinary search
   finds the unsat on the un-substituted formula. Otherwise the rewrite is applied and any
   forced unit is enqueued at level 0. Refs: standard CDCL preprocessing (e.g.
   Gebhardt/Biere; Heule/Järvisalo/Biere inprocessing). *)
let els_pass t work =
  let n2 = 2 * t.nvars in
  if n2 = 0
  then ()
  else (
    (* binary-implication adjacency *)
    let adj = Array.make n2 [] in
    Dynarray.iter
      (fun wc ->
         if (not wc.wdead) && Array.length wc.wl = 2
         then (
           let a = wc.wl.(0)
           and b = wc.wl.(1) in
           adj.(neg_lit a) <- b :: adj.(neg_lit a);
           adj.(neg_lit b) <- a :: adj.(neg_lit b)))
      work;
    let adj = Array.map Array.of_list adj in
    (* iterative Tarjan SCC over the 2*nvars literal nodes *)
    let idx = Array.make n2 (-1) in
    let low = Array.make n2 0 in
    let onstk = Array.make n2 false in
    let comp = Array.make n2 (-1) in
    let tstack = ref [] in
    let counter = ref 0 in
    let ncomp = ref 0 in
    let dfs root =
      let call = Stack.create () in
      idx.(root) <- !counter;
      low.(root) <- !counter;
      incr counter;
      onstk.(root) <- true;
      tstack := root :: !tstack;
      Stack.push (root, ref 0) call;
      while not (Stack.is_empty call) do
        let v, ci = Stack.top call in
        if !ci < Array.length adj.(v)
        then (
          let w = adj.(v).(!ci) in
          incr ci;
          if idx.(w) = -1
          then (
            idx.(w) <- !counter;
            low.(w) <- !counter;
            incr counter;
            onstk.(w) <- true;
            tstack := w :: !tstack;
            Stack.push (w, ref 0) call)
          else if onstk.(w)
          then low.(v) <- min low.(v) idx.(w))
        else (
          ignore (Stack.pop call : lit * int ref);
          if not (Stack.is_empty call)
          then (
            let p, _ = Stack.top call in
            low.(p) <- min low.(p) low.(v));
          if low.(v) = idx.(v)
          then (
            let continue = ref true in
            while !continue do
              match !tstack with
              | w :: rest ->
                tstack := rest;
                onstk.(w) <- false;
                comp.(w) <- !ncomp;
                if w = v then continue := false
              | [] -> continue := false
            done;
            incr ncomp))
      done
    in
    for v = 0 to n2 - 1 do
      if idx.(v) = -1 then dfs v
    done;
    (* representative literal per component: prefer a FROZEN var's literal, else lowest
       lit *)
    let rep = Array.make !ncomp (-1) in
    for l = 0 to n2 - 1 do
      let c = comp.(l) in
      let cur = rep.(c) in
      let frozen l = not (Dynarray.get t.eliminable (var_of_lit l)) in
      if cur = -1
      then rep.(c) <- l
      else if frozen l && not (frozen cur)
      then rep.(c) <- l (* upgrade to a frozen rep *)
      else if Bool.equal (frozen l) (frozen cur) && l < cur
      then rep.(c) <- l
    done;
    (* substitution map (identity except for eliminable non-rep vars), + the equiv record *)
    let subst = Array.init n2 (fun l -> l) in
    let to_elim = ref [] in
    let complementary = ref false in
    for v = 0 to t.nvars - 1 do
      if comp.(pos v) = comp.(neg v) then complementary := true
    done;
    if not !complementary
    then
      for v = 0 to t.nvars - 1 do
        if
          Dynarray.get t.eliminable v
          && (not (Dynarray.get t.eliminated v))
          && Dynarray.get t.assigns v = 0
        then (
          let r = rep.(comp.(pos v)) in
          if var_of_lit r <> v && Dynarray.get t.assigns (var_of_lit r) = 0
          then (
            subst.(pos v) <- r;
            subst.(neg v) <- neg_lit r;
            to_elim := (v, r) :: !to_elim))
      done;
    if (not !complementary) && !to_elim <> []
    then (
      (* map a clause's literals through [subst]: sorted-unique, [None] if tautological *)
      let rewrite wl =
        let seen = Hashtbl.create 8 in
        Array.iter (fun l -> Hashtbl.replace seen subst.(l) ()) wl;
        let taut =
          Hashtbl.fold (fun l () a -> a || Hashtbl.mem seen (neg_lit l)) seen false
        in
        if taut
        then None
        else (
          let arr = Hashtbl.fold (fun l () a -> l :: a) seen [] |> Array.of_list in
          Array.sort compare arr;
          Some arr)
      in
      (* DRY RUN: skip ELS entirely if any rewritten clause exposes unsat (empty clause or
         a unit whose negation is another forced unit / already level-0 false). *)
      let unsafe = ref false in
      let units = Hashtbl.create 16 in
      Dynarray.iter
        (fun wc ->
           if (not wc.wdead) && not !unsafe
           then (
             match rewrite wc.wl with
             | None -> ()
             | Some arr ->
               (match Array.length arr with
                | 0 -> unsafe := true
                | 1 ->
                  let u = arr.(0) in
                  if lit_val t u = -1 || Hashtbl.mem units (neg_lit u)
                  then unsafe := true
                  else Hashtbl.replace units u ()
                | _ -> ())))
        work;
      if not !unsafe
      then (
        (* APPLY: rewrite survivors, record eliminations, enqueue forced units. *)
        let enq = ref [] in
        Dynarray.iter
          (fun wc ->
             if not wc.wdead
             then (
               match rewrite wc.wl with
               | None -> wc.wdead <- true (* tautology *)
               | Some arr ->
                 (match Array.length arr with
                  | 0 -> wc.wdead <- true (* unreachable: dry run ruled it out *)
                  | 1 ->
                    wc.wdead <- true;
                    enq := arr.(0) :: !enq
                  | _ -> wc.wl <- arr)))
          work;
        List.iter
          (fun (v, r) ->
             Dynarray.set t.eliminated v true;
             Hashtbl.replace t.equiv v r;
             t.stat_els <- t.stat_els + 1)
          !to_elim;
        List.iter
          (fun u -> if lit_val t u = 0 then unchecked_enqueue t u r_decision)
          (List.sort_uniq compare !enq))))
;;

(* One inprocessing ROUND, shared by solve-entry preprocessing and restart-boundary
   inprocessing. It runs a SEQUENCE of simplification COMPONENTS over a working copy of
   the IRREDUNDANT (original) clauses —
   [equivalent-literal substitution; subsumption; self-subsuming strengthening; bounded variable elimination]
   — then, after rebuilding the clause DB and closing the level-0 trail under BCP, runs
   the two decision-level-opening components on the learned DB (no-theory path only):
   learned-clause VIVIFICATION and failed-literal PROBING. The full charter component list
   is now implemented; the loop is still an extensible SEQUENCE (a further component slots
   in without reshaping it). Refs: Järvisalo, Heule & Biere, "Inprocessing Rules" (IJCAR
   2012); Fazekas, Biere & Scholl, "Incremental Inprocessing SAT Solving" (SAT 2019).

   {b Learn/forget discipline (the reason it is sound with learned clauses present).}
   Elimination runs on the irredundant set only. Learned clauses are REDUNDANT (entailed
   by the originals), so a learned clause mentioning an eliminated pivot is simply DELETED
   (never resolved) — always sound. A kept learned clause names no eliminated var, and
   since elimination only WEAKENS the formula (F ⟹ elim(F)) every model of the reduced set
   extends to a model of F, which satisfies the learned clause; agreeing on the
   non-eliminated vars, the reduced model satisfies it too — so kept learned clauses stay
   entailed and are re-attached. New learned clauses can never mention an eliminated var
   (it is off the trail: in no clause, never decided), so no future conflict analysis
   reintroduces one. *)
let run_round t =
  if t.satpre && t.trace = None && t.ok
  then (
    match propagate t with
    | Some _ -> () (* level-0 conflict: bail and let search conclude unsat uniformly *)
    | None ->
      (* Build the working set from the original clauses, simplified against the level-0
         unit closure: drop clauses a level-0-true literal satisfies, drop level-0-false
         literals. After full propagation every surviving clause has >= 2 unassigned
         literals (a would-be unit already propagated), so a size <= 1 survivor is
         unexpected — treat it defensively as a reason to bail. *)
      let work = Dynarray.create () in
      let bail = ref false in
      Dynarray.iter
        (fun cr ->
           if (not (cl_deleted t cr)) && not !bail
           then (
             let clits = cl_lits t cr in
             let satisfied = Array.exists (fun l -> lit_val t l = 1) clits in
             if not satisfied
             then (
               let ls =
                 Array.to_list clits
                 |> List.filter (fun l -> lit_val t l <> -1)
                 |> List.sort_uniq compare
               in
               match ls with
               | [] | [ _ ] -> bail := true
               | _ -> Dynarray.add_last work { wl = Array.of_list ls; wdead = false })))
        t.clauses;
      if not !bail
      then (
        let n2 = 2 * t.nvars in
        (* Equivalent-literal substitution FIRST (rewrites [work] to merge equivalent
           vars), so subsumption / strengthening / BVE below see the merged clause set. *)
        els_pass t work;
        (* Occurrence lists, indexed by literal: working-clause indices containing it. A
           clause holds each literal at most once, so no duplicate indices are added; dead
           clauses are filtered lazily at use. Resolvents append here as they are created,
           so a later var's elimination sees the updated clause set (chained elimination). *)
        let occ = Array.make n2 [] in
        Dynarray.iteri
          (fun i wc -> Array.iter (fun l -> occ.(l) <- i :: occ.(l)) wc.wl)
          work;
        let add_wclause lits =
          let i = Dynarray.length work in
          Dynarray.add_last work { wl = lits; wdead = false };
          Array.iter (fun l -> occ.(l) <- i :: occ.(l)) lits;
          i
        in
        let live j = not (Dynarray.get work j).wdead in
        (* ---- Forward subsumption: mark a clause dead if another clause subsumes it.
           ---- *)
        Dynarray.iteri
          (fun i wc ->
             if not wc.wdead
             then (
               (* scan candidates off the literal with the fewest occurrences *)
               let lmin = ref wc.wl.(0) in
               Array.iter
                 (fun l ->
                    if List.length occ.(l) < List.length occ.(!lmin) then lmin := l)
                 wc.wl;
               let subsumed = ref false in
               List.iter
                 (fun j ->
                    if (not !subsumed) && j <> i && live j
                    then (
                      let d = Dynarray.get work j in
                      if
                        Array.length d.wl <= Array.length wc.wl
                        && sorted_subset d.wl wc.wl
                      then subsumed := true))
                 occ.(!lmin);
               if !subsumed
               then (
                 wc.wdead <- true;
                 t.stat_deleted_clauses <- t.stat_deleted_clauses + 1)))
          work;
        (* ---- Self-subsuming resolution (strengthening): if a clause [d] contains [¬l]
           and [d \ {¬l} ⊆ c \ {l}], the resolvent of [c] and [d] on [l] subsumes [c], so
           remove [l] from [c] (equivalence-preserving, no reconstruction). Only shrink
           clauses that stay >= 2 literals (never create a unit here — that would open a
           propagation this prototype does not thread). The [¬l ∈ d] guard keeps it
           correct against an [occ] entry made stale by an earlier strengthening. The
           subset test is SELF-CONTAINED — [m = nl || (m <> l && sorted_mem m cwl)] — so
           it stays sound even if the working set ever stopped being tautology-free (today
           [add_clause] guarantees it; the [m <> l] guard means a hypothetical [c] holding
           both [l] and [¬l] can't spuriously satisfy the membership; review rider). ---- *)
        Dynarray.iteri
          (fun i wc ->
             if not wc.wdead
             then (
               let progress = ref true in
               while !progress && Array.length wc.wl >= 3 do
                 progress := false;
                 let cwl = wc.wl in
                 Array.iter
                   (fun l ->
                      if not !progress
                      then (
                        let nl = neg_lit l in
                        List.iter
                          (fun j ->
                             if (not !progress) && j <> i && live j
                             then (
                               let d = (Dynarray.get work j).wl in
                               if
                                 Array.exists (fun m -> m = nl) d
                                 && Array.for_all
                                      (fun m -> m = nl || (m <> l && sorted_mem m cwl))
                                      d
                               then (
                                 wc.wl
                                 <- Array.of_list
                                      (List.filter (( <> ) l) (Array.to_list cwl));
                                 progress := true)))
                          occ.(nl)))
                   cwl
               done))
          work;
        (* Rebuild occurrence lists: subsumption killed clauses and strengthening rewrote
           [wl]s, so the incremental [occ] is stale. A fresh rebuild over live clauses is
           O(total literals); BVE then appends resolvents to it incrementally. *)
        Array.fill occ 0 n2 [];
        Dynarray.iteri
          (fun i wc ->
             if not wc.wdead then Array.iter (fun l -> occ.(l) <- i :: occ.(l)) wc.wl)
          work;
        (* ---- Bounded variable elimination on eliminable vars. ---- *)
        for v = 0 to t.nvars - 1 do
          if
            Dynarray.get t.eliminable v
            && (not (Dynarray.get t.eliminated v))
            && Dynarray.get t.assigns v = 0
          then (
            let ps = List.filter live occ.(pos v) in
            let ns = List.filter live occ.(neg v) in
            let np = List.length ps
            and nn = List.length ns in
            let do_eliminate deleted_idxs resolvents =
              List.iter (fun r -> ignore (add_wclause r : int)) resolvents;
              List.iter
                (fun (j, piv) ->
                   let wc = Dynarray.get work j in
                   wc.wdead <- true;
                   Dynarray.add_last t.elim_stack (wc.wl, piv))
                deleted_idxs;
              t.stat_elim_vars <- t.stat_elim_vars + 1;
              t.stat_deleted_clauses <- t.stat_deleted_clauses + List.length deleted_idxs;
              t.stat_resolvents <- t.stat_resolvents + List.length resolvents;
              Dynarray.set t.eliminated v true;
              Hashtbl.replace
                t.restore_map
                v
                (List.map (fun (j, _) -> (Dynarray.get work j).wl) deleted_idxs)
            in
            if np = 0 && nn = 0
            then (
              (* In no clause: nothing to delete. Record an EMPTY [restore_map] entry so
                 an (out-of-contract) incremental re-add naming [v] takes the [Some []]
                 restore path — un-eliminate [v], re-add zero clauses — rather than the
                 [None] path, which is reserved for ELS-substituted vars (fail loud) and
                 would otherwise leave [v] frozen out of [pick_branch], making the
                 re-added clause unsatisfiable (a wrong Sat). Keeps every BVE sub-case
                 honoring the [sat.mli] "BVE restores on re-add" contract. *)
              Dynarray.set t.eliminated v true;
              Hashtbl.replace t.restore_map v [])
            else if np = 0
            then
              (* pure literal (only negative): delete the neg clauses, pivot [neg v] *)
              do_eliminate (List.map (fun j -> j, neg v) ns) []
            else if nn = 0
            then do_eliminate (List.map (fun j -> j, pos v) ps) []
            else if np * nn <= bve_product_cap
            then (
              (* general elimination: compute resolvents, disqualify on a unit/empty
                 resolvent (would open a propagation this prototype does not thread) or an
                 over-long one, then apply the classic count bound. *)
              let resolvents = ref [] in
              let disqualified = ref false in
              List.iter
                (fun pi ->
                   List.iter
                     (fun ni ->
                        if not !disqualified
                        then (
                          match
                            resolve_on
                              (Dynarray.get work pi).wl
                              (Dynarray.get work ni).wl
                              v
                          with
                          | None -> () (* tautological resolvent: drops out (BCE) *)
                          | Some r ->
                            if Array.length r <= 1 || Array.length r > bve_size_cap
                            then disqualified := true
                            else resolvents := r :: !resolvents))
                     ns)
                ps;
              if (not !disqualified) && List.length !resolvents <= np + nn + bve_margin
              then (
                let deleted =
                  List.map (fun j -> j, pos v) ps @ List.map (fun j -> j, neg v) ns
                in
                do_eliminate deleted !resolvents)))
        done;
        (* ---- Rebuild the clause DB from the survivors. ---- *)
        for l = 0 to n2 - 1 do
          let ws = Dynarray.get t.watches l in
          Dynarray.clear ws.wc;
          Dynarray.clear ws.wb
        done;
        Dynarray.clear t.clauses;
        Dynarray.iter
          (fun wcl ->
             if (not wcl.wdead) && Array.length wcl.wl >= 2
             then (
               let c = mk_clause t wcl.wl false in
               attach t c))
          work;
        (* Learn/forget over the learned-clause DB (see the header). The watch lists were
           just cleared and rebuilt for the originals, so every KEPT learned clause must
           be re-attached; a learned clause mentioning an eliminated var is dropped
           (marked deleted + removed from [learnts]). Deleting a dropped clause is safe
           even if it is the reason for a level-0 literal (a learned clause CAN be a
           level-0 reason — a post-backjump BCP records [Implied_by] at level 0): the
           dangling reason is never dereferenced, because conflict analysis
           ([analyze]/[analyze_final]) only walks literals at level > 0 and [reduce_db]'s
           [locked] check ranges over the surviving [learnts] (the dropped clause is gone
           from it). A no-op when nothing was eliminated (the [exists] is false for every
           clause) beyond the re-attach, which restores the exact prior watch state. *)
        if Dynarray.length t.learnts > 0
        then (
          let kept = Dynarray.create () in
          Dynarray.iter
            (fun c ->
               if
                 Array.exists
                   (fun l -> Dynarray.get t.eliminated (var_of_lit l))
                   (cl_lits t c)
               then cl_set_deleted t c
               else (
                 attach t c;
                 Dynarray.add_last kept c))
            t.learnts;
          Dynarray.clear t.learnts;
          Dynarray.append t.learnts kept);
        (* ---- Close the level-0 trail under BCP before any component that opens a
           decision level (vivification, failed-literal probing). [els_pass] may have
           enqueued forced units (equivalent-literal substitution rewrote an original
           clause to a unit) WITHOUT propagating them. A downstream component uses the
           assume / [propagate] / [cancel_until 0] pattern; run against the stale [qhead]
           it would process those pending units first, mis-attribute the level-0 conflict
           they cause to its own probe/assume decision, and then discard it —
           [cancel_until 0] resets [qhead] PAST the level-0 units, orphaning a real
           conflict (⇒ wrong Sat on an Unsat formula; the adjudication gadget). Establish
           the round invariant here: the level-0 unit closure is fully propagated, and a
           level-0 conflict concludes Unsat ([t.ok <- false]; [solve]/[go] finish Unsat
           when [t.ok] is false). The [qhead] still points at the first ELS-enqueued unit
           (the round-entry [propagate] advanced it only to the pre-ELS trail end, and
           nothing since touches the real trail), and the clause DB was just rebuilt, so
           this propagates the units against the current (substituted) clauses.
           Vivification and FLP below are both guarded by [t.ok], so a conflict here makes
           them no-ops. ---- *)
        (match propagate t with
         | Some _ -> t.ok <- false
         | None -> ());
        (* ---- Vivification component: shorten learned clauses by re-propagation. Runs
           only on the pure-propositional / bit-blasted path (no theory seam to drive),
           over a snapshot of the current learned DB, effort-bounded. A shortened clause
           replaces the original: mark the old deleted (its watches lazy-sweep) and attach
           a fresh learned clause with the sub-clause. Only clauses of length in [3, cap]
           are tried (a length-2 clause could only shorten to a unit, which this cut does
           not thread). Sound: each replacement is by an entailed sub-clause (see
           {!vivify_learnt}); it never touches original clauses or the reconstruction
           stack. ---- *)
        if t.theory = None && t.ok && Dynarray.length t.learnts > 0
        then (
          let snapshot = Dynarray.to_array t.learnts in
          let budget = ref vivify_budget in
          Array.iter
            (fun c ->
               if
                 !budget > 0
                 && (not (cl_deleted t c))
                 && cl_len t c >= 3
                 && cl_len t c <= vivify_size_cap
               then (
                 decr budget;
                 match vivify_learnt t c with
                 | None -> ()
                 | Some sub ->
                   cl_set_deleted t c;
                   let nc = mk_clause t sub true in
                   cl_set_lbd t nc (clause_lbd t sub);
                   attach t nc;
                   t.stat_vivified <- t.stat_vivified + 1))
            snapshot;
          (* drop the replaced (deleted) learned clauses; the fresh shortened ones were
             already appended to [t.learnts] by [mk_clause] and stay. *)
          let kept = Dynarray.create () in
          Dynarray.iter
            (fun c -> if not (cl_deleted t c) then Dynarray.add_last kept c)
            t.learnts;
          Dynarray.clear t.learnts;
          Dynarray.append t.learnts kept);
        (* Failed-literal probing component (no-theory path only; derives forced level-0
           units, may set [t.ok] false on an exposed conflict — callers conclude Unsat). *)
        if t.theory = None && t.ok then failed_literal_probing t))
;;

(* Solve-entry preprocessing (Phase 1): one round at decision level 0 before search. *)
let preprocess t = run_round t

let solve ?(assumptions = []) t =
  t.failed <- [];
  (* Stage-1 CB scope guard (task #41 §10.2). The relevancy trail unwind assumes monotone
     levels, so CB and a branch filter are mutually exclusive until the relevancy trail is
     made remove-by-level (§3.7). Fail-loud [invalid_arg], never a silent degrade.

     F2 (assumptions under CB): assumptions were also guarded here originally, because the
     only assumption-specific CB hazard is [analyze_final]'s failed-assumption-core walk,
     which read [trail_lim.(0)] as a POSITION (stale under CB). That is now fixed:
     [analyze_final] walks the whole trail under CB and gates marking by per-literal
     level, so the core is correct (the VERDICT never depended on it). Assumptions are
     otherwise CB-safe — each is a DECISION at a fixed level [i+1], never chronologically
     relocated, and the assumption placement loop indexes by [decision_level] (=
     [trail_lim] length, valid under CB). Permitting them is REQUIRED for the product:
     every [Session] solve passes [List.map Sat.pos t.frames] (a nonempty base selector),
     so the old guard made CB unreachable through the CLI (it tripped the firewall to
     [Unknown] on every solve). *)
  if t.chrono && t.branch_filter <> None
  then
    invalid_arg
      "Sat.solve: OXSMT_CHRONO and a decision branch filter (relevancy) are mutually \
       exclusive (task #41 Stage 1)";
  List.iter (fun l -> ensure_var t (var_of_lit l)) assumptions;
  (* A10 assumptions guard (Phase-2 review rider). The assumptions path reaches the trail
     via [ensure_var]+search, NOT [add_clause], so it bypasses the restore-on-mention
     hook: an assumption naming an eliminated var would be solved against a clause set
     from which that var's clauses were removed (a wrong verdict), and an
     eliminable-but-not-yet- eliminated var could be eliminated by [preprocess] below and
     then assumed. Both are a caller-contract violation ("no eliminable var is ever
     assumed" — true for every current caller: assumptions are frame selectors / bv has
     none, all frozen). Make the violation LOUD rather than latent: raise (not [assert],
     survives -noassert). Fires only on a contract-violating caller, so every conforming
     solve — and the whole OXSMT_SATPRE-off path — is unaffected. (A full incremental
     solver would REACTIVATE instead, per Fazekas– Biere–Scholl SAT'19; that lands when
     Phase-2 interleaves elimination with theory lemmas.) *)
  List.iter
    (fun l ->
       let v = var_of_lit l in
       if
         v < Dynarray.length t.eliminable
         && (Dynarray.get t.eliminable v || Dynarray.get t.eliminated v)
       then
         invalid_arg
           "Sat.solve: an assumption names a variable marked eliminable/eliminated by \
            CNF preprocessing (A10); assumptions must be over frozen variables")
    assumptions;
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
    (* F1: drop any preserved theory-propagation reasons from a prior solve; a level-0
       survivor's reason is re-served by the theory's own (base-frame) cache. Empty and
       untouched unless [chrono]. *)
    Hashtbl.reset t.chrono_reason;
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
    (* Phase-2 inprocessing schedule, relative to this solve's conflict base (geometric
       back-off). [max_int] when the gate is off => never fires (bit-identical). *)
    t.inproc_interval <- inproc_interval_base;
    t.inproc_next
    <- (if t.satpre && t.trace = None
        then t.conflicts + inproc_first_offset ()
        else max_int);
    (* CNF preprocessing (A10): env-gated, cert-OFF, level 0. A no-op when OXSMT_SATPRE is
       off (bit-identical); when on it may simplify the clause DB and eliminate marked aux
       vars (reconstructed at [save_model]). *)
    preprocess t;
    let rec go restart_no =
      (* Luby conflict-count cap kept alongside the adaptive trigger (S3): a loose upper
         bound that forces a restart if the LBD trigger never fires. *)
      let lim = luby restart_no * t.restart_base in
      match search t assumps lim with
      | R_restart ->
        (* Restart boundary (search has cancelled to level 0): fire a scheduled
           inprocessing round, then step the geometric back-off. Guarded by [inproc_next]
           = [max_int] when the gate is off, so the pure core never calls [run_round]. *)
        if t.conflicts >= t.inproc_next
        then (
          run_round t;
          t.inproc_next <- t.conflicts + t.inproc_interval;
          t.inproc_interval <- t.inproc_interval * 2);
        (* a round component can expose unsat by setting [t.ok] false — failed-literal
           probing on a forced-unit conflict, or the level-0 BCP closure of ELS's forced
           units (ELS itself never sets it: its dry run skips substitution that would
           expose unsat). Conclude immediately rather than search on to a spurious model. *)
        if not t.ok then Unsat else go (restart_no + 1)
      | R_sat -> Sat
      | R_unsat -> Unsat
    in
    (* solve-entry [preprocess] can also expose unsat (failed-literal probing, or the
       level-0 closure of ELS's forced units); conclude if so. *)
    let r = if not t.ok then Unsat else go 0 in
    cancel_until t 0;
    (* A10 elimination-stats side channel (measurement only): emit cumulative counts to
       stderr when OXSMT_SATPRE_STATS is truthy, for the A/B per-family report. Gated on
       [t.satpre] as well, so with preprocessing off the line is never emitted (nothing to
       measure); an unset env costs one [getenv_opt] and no output — never on the
       pure-core path. *)
    (match if t.satpre then Sys.getenv_opt "OXSMT_SATPRE_STATS" else None with
     | Some ("1" | "true" | "yes" | "on") ->
       Printf.eprintf
         "satpre-stats elim_vars=%d deleted_clauses=%d resolvents=%d vivified=%d els=%d \
          flp=%d\n\
          %!"
         t.stat_elim_vars
         t.stat_deleted_clauses
         t.stat_resolvents
         t.stat_vivified
         t.stat_els
         t.stat_flp
     | Some _ | None -> ());
    r)
;;

(* ------------------------------------------------------------------ *)
(* Model and stats accessors. *)

let value t v = v < Dynarray.length t.saved_model && Dynarray.get t.saved_model v = 1
let model t = Array.init (Dynarray.length t.saved_model) (fun v -> value t v)
let failed_assumptions t = t.failed

(* Read-only VSIDS activity of a variable (sat.mli var_activity); 0.0 for an out-of-range
   var. A pure side channel for a branch-filter/relevancy client — never mutated here, no
   effect on search. *)
let var_activity t v =
  if v >= 0 && v < Dynarray.length t.var_act then Dynarray.get t.var_act v else 0.0
;;

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
