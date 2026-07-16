(* LIA THEORY adapter. See lia_adapter.mli.

   A thin binding of the {!Lia} decision engine (Dutertre-de Moura simplex + branch-and-
   bound) to the frozen ADR-0005 [Theory.THEORY] seam. All reasoning lives in {!Lia}; this
   layer only:
   - translates the engine currency ([Atom.t]/[Lit.t]) to/from the [Term.t]s {!Lia}
     speaks, instantiating {!Lia}'s opaque premise token ['tok] to [Lit.t];
   - folds [propagate] + rational/integer [check] into the single [check effort] verdict,
     delegating integer branching to CDCL(T) via a [Split] (never running {!Lia}'s own
     internal B&B in the loop — that keeps conflict-driven learning in the SAT core);
   - caches each theory-propagated literal's premise set at propagation time so [explain]
     is O(1) and precedence-valid (CONTRACT-EX), with the cache scoped to the [push]/[pop]
     frame that produced it;
   - upholds CONTRACT-POISON: an escaped {!Rational.Overflow} (or a {!Lia.Poisoned} from a
     bricked instance, or a {!Lia.Unsupported}) is never turned into a sat/unsat verdict —
     it propagates out of the THEORY op so the engine degrades the query to [unknown]. The
     adapter additionally counts overflow-induced degradations ({!overflows_to_unknown})
     as the design's distinct native-int-ceiling stat. *)

open Oxsmt_core

(* GCD / Diophantine integer-feasibility test before b&b branching. Default ON; set
   OXSMT_NO_DIOPHANTINE to disable (A/B). Read once (module scope is fine — no term/id
   currency, just a boolean policy). *)
let diophantine_on =
  match Sys.getenv_opt "OXSMT_NO_DIOPHANTINE" with
  | None | Some ("0" | "false" | "no" | "") -> true
  | Some _ -> false
;;

(* Stage B HNF integer cuts (DARK; charter logs/lia-cuts-charter.md, spec
   logs/lia-cuts-hnf-spec.md). Default OFF — byte-identical to trunk BY CONSTRUCTION: when
   off, {!Lia.hnf_cut} is never called and no counter is touched, so this whole lane is
   inert. When ON, at an integer-infeasible [Final] that {!Lia.diophantine_conflict} did
   NOT refute, on every [hnf_cut_period]-th such Final a multi-row lattice cut is derived
   over the tight constraint rows (asserted equalities AND active one-sided bounds) and
   emitted through the CONTRACT-LEMMA seam as [Lemma [(cut, true); ¬antᵢ …]] (z3's
   assign(cut, core)), tightening the LP toward the ring-lattice conflict. A miss (no cut
   / self-check fail / unmappable antecedent) falls back to the B&B branch. *)
let hnf_cuts_on =
  match Sys.getenv_opt "OXSMT_HNF_CUTS" with
  | Some ("1" | "true" | "yes" | "on") -> true
  | _ -> false
;;

(* Stage B3 Chvátal–Gomory SEPARATION cut (default-ON since #68; sibling flag to
   {!hnf_cuts_on}). When ON, the cut site calls {!Lia.cg_cut} (the sign-shifted multiplier
   search) instead of {!Lia.hnf_cut}: B3 emits cuts B2's sign discipline rejects (charter
   logs/lia-cuts-b2-log.md §next rung), staying a self-checked, fail-closed T-valid cut.
   Independently measurable from B2 so B2's OFF-identity story is untouched. If both flags
   are set, B3 (the superset) takes it.

   Tri-state (established flip family, mirrors {!Combine.model_repair_on}): unset → ON
   (the new default); [OXSMT_CG_CUTS=0]/false/no → OFF, the byte-for-byte pre-flip path
   (B&B branch, no {!Lia.cg_cut} call, no counter touched — trunk-exact); anything else →
   ON. Only this env→bool read changed at the flip; the cut machinery is untouched. *)
let cg_cuts_on =
  match Sys.getenv_opt "OXSMT_CG_CUTS" with
  | Some ("0" | "false" | "no") -> false
  | Some _ | None -> true
;;

(* z3-parity throttle (util/lp/lp_settings.h m_hnf_cut_period); mirrored in {!Hnf.cut_period}. *)
let hnf_cut_period = Hnf.cut_period

(* Adapter-lifetime budget on CG-cut ATTEMPTS (B3 only) — the counter is NOT reset per
   query; a per-query reset is a tracked follow-up (task #53). An exact
   Hermite-Normal-Form over the rank-reduced tight system costs ~O(coefficient blow-up)
   per call, and on files where the lattice cut is productive it collapses the search
   within a handful of cuts (measured: the rings prize cracks in ≤ 7 attempts); on files
   where it is NOT productive the cut fires repeatedly without progress and its cost
   dominates the 2 s wall. Bounding the attempts to a small constant keeps the prize gains
   and caps the worst-case tax — a proportional cost guard, not a solver heuristic.
   Overridable ([OXSMT_CG_MAX_CUTS]) for measurement. *)
let cg_max_cuts =
  match Option.bind (Sys.getenv_opt "OXSMT_CG_MAX_CUTS") int_of_string_opt with
  | Some n when n >= 0 -> n
  | _ -> 12
;;

(* task #60 cut-policy SPARSITY GATE. The rings prize is carried by SPARSE cuts (few rows,
   few coefficients); the broad-QF_LIA losers (cut_lemmas, rings_preproc tail) are DENSE
   GLOBAL cuts — measured (logs/cut-policy-diagnosis.md, counted): every losing cut
   combines ALL tight rows ([ant_count = m]) with [nnz ≈ 0.5–0.8·n], and each such lemma
   lengthens search (cut_lemma_02_002: OFF 2054 → ON 3552 counted effort; suppressing the
   cuts recovers OFF exactly) without changing the verdict. Winners never use all rows
   ([ant_count < m], ≤ 0.65·m observed) and stay sparse ([nnz ≤ ~0.25·n]). The gate
   REJECTS a dense best-cut so the adapter branches instead — SOUNDNESS-NEUTRAL (a branch
   is a strictly weaker action; the cut is only forgone). Only consulted on the B3
   [cg_cut] path (byte-identical to trunk when [cg_cuts_on] is off, and unused by the B2
   [hnf_cut] path).

   Default: reject iff the cut uses EVERY tight row ([ant_count ≥ m], i.e.
   [ants_pct = 100]) — the crisp measured separator. The coefficient-density knob is
   disabled by default ([nnz_pct = 101] ⇒ [nnz > 1.01·n], never true). All three tunable
   for the A/B: [OXSMT_CG_CUT_GATE=0] disables the gate (= pre-policy B3);
   [OXSMT_CG_ANTS_PCT] / [OXSMT_CG_NNZ_PCT] set the density thresholds (percent). *)
let cut_gate_on =
  match Sys.getenv_opt "OXSMT_CG_CUT_GATE" with
  | Some ("0" | "false" | "no" | "off") -> false
  | _ -> true
;;

let cut_gate_ants_pct =
  match Option.bind (Sys.getenv_opt "OXSMT_CG_ANTS_PCT") int_of_string_opt with
  | Some n when n >= 0 -> n
  | _ -> 100
;;

let cut_gate_nnz_pct =
  match Option.bind (Sys.getenv_opt "OXSMT_CG_NNZ_PCT") int_of_string_opt with
  | Some n when n >= 0 -> n
  | _ -> 101
;;

(* [true] = EMIT the cut, [false] = reject (branch). A cut is dense — hence rejected —
   when its antecedent support reaches [ants_pct]% of the rows OR its coefficient count
   exceeds [nnz_pct]% of the columns. *)
let cut_gate ~nnz ~ants ~m ~n =
  let dense_ants = ants * 100 >= cut_gate_ants_pct * m in
  let dense_nnz = nnz * 100 > cut_gate_nnz_pct * n in
  not (dense_ants || dense_nnz)
;;

type t =
  { lia : Fabric.justification Lia.t
  ; term_of_atom : Term.t Atom.Table.t (* engine atom id -> its registered [Term.t] *)
  ; atom_of_term : Atom.t Term.Table.t
      (* reverse map, for turning a propagated term back into its literal *)
  ; mutable explain_cache :
      Fabric.Explanation.t Lit.Map.t (* propagated lit -> its lazy reason *)
  ; mutable frames : Lit.t list list
      (* per-frame lits cached, head = current frame; used to drop stale reasons on [pop] *)
  ; mutable overflows : int (* overflow-induced degradations to unknown (adapter side) *)
  ; mutable hnf_final_cuttable : int
      (* count of integer-infeasible [Final]s reaching the branch fallback — the throttle
         phase for HNF cuts (only touched when [hnf_cuts_on], so OFF is byte-identical). *)
  ; mutable hnf_cuts_emitted : int (* HNF cuts emitted as a Lemma (instrumentation) *)
  ; mutable cg_attempts : int
  (* CG-cut ATTEMPTS this query (each an exact HNF); bounded by [cg_max_cuts] so an
     unproductive lattice cut cannot dominate the wall. Only touched on the B3 path. *)
  }

let create ctx _env =
  { lia = Lia.create ctx
  ; term_of_atom = Atom.Table.create 64
  ; atom_of_term = Term.Table.create 64
  ; explain_cache = Lit.Map.empty
  ; frames = [ [] ]
  ; overflows = 0
  ; hnf_final_cuttable = 0
  ; hnf_cuts_emitted = 0
  ; cg_attempts = 0
  }
;;

(* Run a state-touching engine op, counting (once) a native-int overflow that degrades
   this query to unknown, then re-raising so the engine's CONTRACT-POISON handler bricks
   the instance and returns [unknown] (I8). Only {!Rational.Overflow} — the documented
   ceiling event — is counted; a later {!Lia.Poisoned} on the already-bricked instance is
   the aftermath, not a new overflow, and propagates uncounted. {!Lia.Unsupported}
   likewise propagates (out-of-fragment -> unknown), uncounted. *)
let guard t thunk =
  try thunk () with
  | Rational.Overflow as e ->
    t.overflows <- t.overflows + 1;
    raise e
;;

let register_atom t atom term =
  guard t (fun () ->
    (* Idempotent (C7): record the atom<->term maps once; re-register is a no-op on them.
       [Lia.register_atom] is itself idempotent and only records [Le] atoms for
       propagation (equalities are not propagation targets in v1). *)
    if not (Atom.Table.mem t.term_of_atom atom)
    then (
      Atom.Table.replace t.term_of_atom atom term;
      Term.Table.replace t.atom_of_term term atom);
    Lia.register_atom t.lia term)
;;

let assert_lit t lit =
  guard t (fun () ->
    let atom = Lit.atom lit in
    match Atom.Table.find_opt t.term_of_atom atom with
    | Some term ->
      Lia.assert_atom t.lia term ~polarity:(Lit.sign lit) ~premise:(Fabric.Real lit)
    | None ->
      (* CONTRACT: [assert_lit]'s atom was registered first. A miss is a driver bug; fail
         loud -> engine degrades to unknown rather than reasoning on an unmapped atom. *)
      failwith "Lia_adapter.assert_lit: literal's atom was not registered")
;;

(* ADR-0014 Stage 2 [new_eq] notification (§A.3): the hub merged two Int classes shared
   with LIA, so assert the entailed equality [s = t] into the tableau directly (a pair of
   bounds), attributed to the fabric edge whose Γ is the EUF congruence proof. A LIA
   conflict later citing this premise expands (combinator F2 chokepoint) to the real trail
   literals behind the merge. The assertion rides LIA's own trail, so an ordinary [pop]
   reverses it via LIA's own frame pop (F3 co-location, ADR §C Stage 0 item 5). Overflow
   during the pair-of-bounds construction degrades the query via CONTRACT-POISON, exactly
   like an ordinary [assert_lit]. The combinator does the fallible work — building the
   [eq] term and recording the edge Γ — BEFORE calling this, and this op is pure mutation
   (a pair of bounds on LIA's trail), so a skipped notification leaves zero partial state
   (H5). Uses {!Lia.notify_equality}, which no-ops a [0 = 0] TAUTOLOGY re-notification
   (congruence can re-surface an equality LIA already relates — turning that into a
   query-wide [unknown] was a latent Stage-2 gap) but keeps raising on an unsatisfiable
   [0 = k] equality, so a genuine constant contradiction is never silently dropped. *)
let notify_eq t ~edge_id eq =
  guard t (fun () -> Lia.notify_equality t.lia eq ~premise:(Fabric.Fabric edge_id))
;;

(* LIA parity with {!Euf_adapter}'s codex AP4 tripwire: an EMPTY premise set is an
   unconditional entailment (for a propagation) or an unconditional [false] (for a
   conflict) — a soundness bug either way. UNCONDITIONAL guard, not [assert]: like AP4 it
   must survive the release [-noassert] build, because feeding 1UIP a premise-free
   conflict would learn the empty clause (a spurious [unsat]). Raising here degrades the
   query to [unknown] via CONTRACT-POISON instead. Unconstructible from the engine — a
   Farkas conflict's infeasible core always cites >= 1 asserted bound, and a bound
   propagation always carries its single entailing bound (see
   {!Lia.propagate}/{!Lia.check}) — so this only fires on a corrupted reason set. *)
let checked_premises what premises =
  if premises = []
  then
    failwith (Printf.sprintf "Lia_adapter: empty %s (unsound) [codex AP4 tripwire]" what);
  premises
;;

let conflict_explanation (c : Lit.t Lia.conflict) : Explanation.t =
  (* Premises are the [Lit.t] tokens of the infeasible bound set; the Farkas multipliers
     stay engine-internal (self-checked at production, DESIGN §7) and route to the
     off-core M5 certificate module, never onto the frozen [Explanation] (ADR-0005 D7 /
     ADR-0006). *)
  { premises = checked_premises "conflict premise set" c.premises
  ; rule = Explanation.Rule_tag.Lia_farkas
  }
;;

let propagation_reason premises : Explanation.t =
  { premises = checked_premises "propagation reason" premises
  ; rule = Explanation.Rule_tag.Lia_bound
  }
;;

let fabric_conflict_explanation (c : Fabric.justification Lia.conflict)
  : Fabric.Explanation.t
  =
  { premises = checked_premises "conflict premise set" c.premises
  ; rule = Explanation.Rule_tag.Lia_farkas
  }
;;

let fabric_propagation_reason premises : Fabric.Explanation.t =
  { premises = checked_premises "propagation reason" premises
  ; rule = Explanation.Rule_tag.Lia_bound
  }
;;

let ordinary_explanation (e : Fabric.Explanation.t) =
  let premises =
    List.map
      (function
        | Fabric.Real lit -> lit
        | Fabric.Fabric _ ->
          failwith "Lia_adapter: fabric handle crossed the direct THEORY seam")
      e.premises
  in
  { Explanation.premises; rule = e.rule }
;;

(* Cache a propagated literal's reason in the current frame so [explain] can serve it and
   [pop] can drop it when its decision level unwinds. FIRST-WINS, and this is load-bearing
   for CONTRACT-EX: the reason from the FIRST propagation is the precedence-valid one —
   its premises were all asserted strictly before the literal was first put on the trail.
   A later re-propagation of the same literal (e.g. once a tighter bound is asserted at a
   deeper level) reports the NEWER entailing bound, which was asserted AFTER the literal
   was already assigned; overwriting with it would (a) feed 1UIP a premise that violates
   precedence and (b) re-scope the entry to the newer frame, so a [pop] that should not
   touch this reason would drop it (spurious [explain] failure -> whole-query unknown).
   Keeping the first reason is sound: its bound is still on the trail — its frame is at or
   below the current one, so it cannot have been popped without also popping (and
   uncaching) this entry. A re-propagation AFTER the first frame is popped finds no entry
   and caches afresh, which is correct (the old reason was unwound with its frame). *)
let cache_reason t lit expl =
  if not (Lit.Map.mem lit t.explain_cache)
  then (
    t.explain_cache <- Lit.Map.add lit expl t.explain_cache;
    match t.frames with
    | fr :: rest -> t.frames <- (lit :: fr) :: rest
    | [] -> t.frames <- [ [ lit ] ])
;;

(* Bound-to-bound propagations the engine has not yet assigned, each cached with its
   single entailing bound as reason ([Lia_bound]). A propagated term with no atom mapping
   is skipped (sound: fewer propagations, the SAT core will decide it). *)
let propagations t =
  Lia.propagate t.lia
  |> List.filter_map (fun (term, polarity, premises) ->
    match Term.Table.find_opt t.atom_of_term term with
    | None -> None
    | Some atom ->
      let lit = Lit.make atom polarity in
      cache_reason t lit (fabric_propagation_reason premises);
      Some lit)
;;

(* Map a HNF cut ([cut_atom] + antecedent premise tokens) into a CONTRACT-LEMMA
   [Fabric.Lemma]: the head [(cut_atom, true)] followed by each antecedent as its NEGATED
   trail literal [(atom_term, not sign)]. The antecedents are currently true, so the
   clause is unit on [cut_atom] and BCP propagates it (z3's assign(cut, core)). [None] —
   falling back to the branch — if any antecedent token is not a plain trail literal
   ([Fabric.Real]): a combined-child [Fabric] edge has no single atom term and a [Lemma]
   carries only terms, so such a cut cannot be represented (soundly skipped, never emitted
   with a missing premise); likewise if an atom is absent from the term map, or the
   antecedent set is empty (never emit a premise-free cut). *)
let hnf_lemma t : Fabric.check_result option =
  (* B3 (CG separation) is a strict superset of B2's cut-finding, so it takes precedence
     when its flag is set; otherwise the B2 HNF-row cut. *)
  let cut =
    if cg_cuts_on
    then
      Lia.cg_cut
        ~cut_gate:(if cut_gate_on then cut_gate else fun ~nnz:_ ~ants:_ ~m:_ ~n:_ -> true)
        t.lia
    else Lia.hnf_cut t.lia
  in
  match cut with
  | None -> None
  | Some (cut_atom, ant_tokens) ->
    let rec map acc = function
      | [] -> Some (List.rev acc)
      | Fabric.Real lit :: rest ->
        (match Atom.Table.find_opt t.term_of_atom (Lit.atom lit) with
         | Some tm -> map ((tm, not (Lit.sign lit)) :: acc) rest
         | None -> None)
      | Fabric.Fabric _ :: _ -> None
    in
    (match map [] ant_tokens with
     | None | Some [] -> None
     | Some ants -> Some (Fabric.Lemma ((cut_atom, true) :: ants)))
;;

(* At an integer-infeasible [Final] the fallback is the B&B split [x<=v ; x>=v+1]. With
   HNF cuts ON, on every [hnf_cut_period]-th such Final try a lattice cut and emit it as a
   [Lemma] instead; a miss falls back to the branch. OFF returns the plain branch and
   touches no counter — byte-identical to trunk. ON at a non-period Final increments
   [hnf_final_cuttable] only, then returns the plain branch. *)
let branch_or_hnf_cut t le_atom ge_atom : Fabric.check_result =
  let branch () = Fabric.Split [ le_atom; ge_atom ] in
  if not (hnf_cuts_on || cg_cuts_on)
  then branch ()
  else (
    t.hnf_final_cuttable <- t.hnf_final_cuttable + 1;
    (* B3 attempt budget (per adapter INSTANCE): after [cg_max_cuts] exact-HNF attempts,
       fall back to plain b&b so an unproductive lattice cut cannot dominate the wall. B2
       (no [cg_cuts_on]) is unbudgeted — behaviour unchanged.

       Scoping (task #53 H3 finding): this IS per-query on the corpus and on the
       batch/reset paths — a non-incremental query runs in its own [create]d adapter
       (cg_attempts=0), and a datatype/array registry change between queries
       drops+recreates the theory ([Cdclt.reset_for_new_query] nulls it), so the budget is
       fresh. The one residual is a PERSISTING-theory INCREMENTAL session
       (push;assert;check-sat;…;check-sat with no registry change): the adapter survives
       across check-sats, so the budget accumulates and later check-sats under-fire. A
       truly per-check-sat reset would need a "new query" notification reaching the
       theory, which neither the frozen [Theory] interface nor the [Sat.theory] callback
       record exposes (the only per-solve-ish hook, [on_backtrack] level 0, also fires on
       mid-solve RESTARTS — resetting there would REFRESH the budget mid-solve and change
       the batch rings result, so it is unsound as a per-query proxy). Wiring such a hook
       is a follow-up (it touches the internalization combinator), tracked for the
       cut-policy lane; the corpus/headline are unaffected either way. *)
    let budget_exhausted = cg_cuts_on && t.cg_attempts >= cg_max_cuts in
    if t.hnf_final_cuttable mod hnf_cut_period <> 0 || budget_exhausted
    then branch ()
    else (
      if cg_cuts_on then t.cg_attempts <- t.cg_attempts + 1;
      match hnf_lemma t with
      | Some lemma ->
        t.hnf_cuts_emitted <- t.hnf_cuts_emitted + 1;
        lemma
      | None -> branch ()))
;;

let check_fabric t (effort : Theory.effort) : Fabric.check_result =
  guard t (fun () ->
    match Lia.check t.lia with
    | Conflict c -> Fabric.Conflict (fabric_conflict_explanation c)
    | Sat_candidate ->
      (match effort with
       | Theory.Propagate -> Fabric.Propagations (propagations t)
       | Theory.Final ->
         (* Rational-feasible: integral -> genuine ℤ model -> Sat; else ask CDCL(T) to
            branch on the two distinct, currently-false atoms [x<=floor v] /
            [x>=floor v+1] (CONTRACT-SPLIT: >=2 distinct atoms, genuinely constraining —
            not the discarded [Eq v ¬Eq] tautology). *)
         (match Lia.suggest_branch t.lia with
          | None -> Fabric.Sat
          | Some (le_atom, ge_atom) when diophantine_on ->
            (* Integer-feasibility (GCD) test before branching: a ℚ-feasible but
               ℤ-infeasible equality row (e.g. [4s+4x=6]) is refuted here immediately
               rather than left to b&b, which would otherwise wander. Sound conflict
               (premises are ℤ-unsatisfiable); on [None] proceed exactly as before. *)
            (match Lia.diophantine_conflict t.lia with
             | Some c -> Fabric.Conflict (fabric_conflict_explanation c)
             | None ->
               (match Lia.cube_model t.lia with
                | Some _ -> Fabric.Sat
                | None -> branch_or_hnf_cut t le_atom ge_atom))
          | Some (le_atom, ge_atom) ->
            (* Before branching, try the Bromberger-Fleury unit cube test: a fat feasible
               region yields an integer model in one shrink+re-solve, skipping b&b (which
               may not terminate on unbounded directions — the Bromberger family's
               design). Sound: the cube model is re-verified by the simplex and the
               session R1 check; a miss falls back to the split. *)
            (match Lia.cube_model t.lia with
             | Some _ -> Fabric.Sat
             | None -> branch_or_hnf_cut t le_atom ge_atom))))
;;

let check t effort =
  match check_fabric t effort with
  | Fabric.Sat -> Theory.Sat
  | Fabric.Propagations lits -> Theory.Propagations lits
  | Fabric.Conflict e -> Theory.Conflict (ordinary_explanation e)
  | Fabric.Split terms -> Theory.Split terms
  | Fabric.Lemma l -> Theory.Lemma l
;;

let explain_fabric t lit =
  match Lit.Map.find_opt lit t.explain_cache with
  | Some expl -> expl
  | None ->
    (* [explain] is only defined for a literal THIS theory propagated and still on the
       trail; its reason was cached at propagation time. A miss is a driver/contract
       violation — fail loud rather than fabricate an unsound premise set. *)
    failwith "Lia_adapter.explain: no cached reason for literal (not theory-propagated?)"
;;

let explain t lit = ordinary_explanation (explain_fabric t lit)

let fixed_bounds t term =
  guard t (fun () ->
    match Lia.fixed_bounds t.lia term with
    | None -> None
    | Some (value, lower, upper) ->
      Some { Fabric.value = Rational.to_string value; lower; upper })
;;

(* ADR-0014 Stage 1b F1-SEM independent semantic verifier (§B.1 C1). Re-derive, via
   {!Lia.oriented_bound_value} — a code path SEPARATE from the {!fixed_bounds} tuple the
   fix-trigger produced — that [term] really is fixed to [value] with [lo]/[hi] as its
   oriented bound premises. Rejects a wrong value, a swapped/foreign token, or a
   dropped/non-exact bound (so the ADR's weak-Γ mutant is non-vacuously caught). Both
   premises must be genuine trail literals ([Real]); a [Fabric]-handle bound cannot be a
   fixed-value witness in Stage 1b. *)
let fabric_verify t term value lo hi =
  guard t (fun () ->
    match lo, hi with
    | Fabric.Real lo_lit, Fabric.Real hi_lit ->
      (match
         ( Lia.oriented_bound_value t.lia term `Lower
         , Lia.oriented_bound_value t.lia term `Upper )
       with
       | Some (lt, lv), Some (ut, uv) ->
         String.equal (Rational.to_string lv) value
         && String.equal (Rational.to_string uv) value
         &&
           (match lt, ut with
           | Fabric.Real l, Fabric.Real u -> Lit.equal l lo_lit && Lit.equal u hi_lit
           | _ -> false)
       | _ -> false)
    | _ -> false)
;;

let model t =
  (* Valid only after [check Final] returned [Sat] (all problem vars integral);
     [Lia.model] raises otherwise. LIA emits only [Int] values. *)
  Lia.model_bigint t.lia
  |> List.map (fun (term, v) -> term, Model.Int v)
  |> Model.of_alist
;;

let push t =
  Lia.push t.lia;
  t.frames <- [] :: t.frames
;;

let pop t n =
  Lia.pop t.lia n;
  (* Drop the last [n] frames, uncaching every reason they hold (a propagation's reason is
     valid only at the level it was made). Keep at least a root frame. *)
  let rec drop k frames =
    if k = 0
    then frames
    else (
      match frames with
      | fr :: rest ->
        List.iter (fun l -> t.explain_cache <- Lit.Map.remove l t.explain_cache) fr;
        drop (k - 1) rest
      | [] -> [])
  in
  t.frames
  <- (match drop n t.frames with
      | [] -> [ [] ]
      | fs -> fs)
;;

(* Diagnostics (off the frozen contract; for tests/metrics). Safe on a poisoned instance. *)
let is_poisoned t = Lia.is_poisoned t.lia
let overflows_to_unknown t = t.overflows
let pivot_count t = Lia.pivot_count t.lia
let hnf_cuts_emitted t = t.hnf_cuts_emitted

(* Reset the per-query CG-cut attempt budget (task #53 H3). Zeroes [cg_attempts] so the
   [cg_max_cuts] cap starts fresh — the intent is one budget per top-level query. It is
   ALREADY fresh on the corpus/reset paths (fresh adapter per query;
   [Cdclt.reset_for_new_query] nulls+recreates the theory), so this is the mechanism for
   the residual PERSISTING-theory INCREMENTAL case, where the adapter survives across
   check-sats. Wiring a per-check-sat call reaching the theory is a documented follow-up
   (neither the frozen [Theory] interface nor the [Sat.theory] callback record exposes a
   "new query" hook; a decision-level-0/restart-based proxy would over-fire mid-solve).
   This is proven by the direct adapter-level test [cut_budget_test] and is otherwise
   unwired, so it changes no solver behavior (OFF and ON byte-identical — never called in
   the solve path). *)
let reset_cut_budget t = t.cg_attempts <- 0

(* CG-cut attempts consumed on the current budget (task #53 H3 test observability,
   symmetric with {!hnf_cuts_emitted}). Bounded by [cg_max_cuts] between
   {!reset_cut_budget}s. *)
let cut_attempts t = t.cg_attempts
