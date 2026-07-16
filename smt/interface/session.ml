(* Session layer wiring the frozen core, preprocessing/clausification, the CDCL SAT core,
   and — the M4 change — the Nelson-Oppen combined EUF+LIA theory stack into a full
   CDCL(T) check-sat loop (DESIGN.md §3, §5, §6). See session.mli for the contract, in
   particular THE SOUNDNESS RULE (rewritten for the theory-plugged regime).

   Everything threads one Context/Env (ADR-0003 Decision 6): terms asserted across
   [assert_term]/[push]/[pop] share the tag stream and hash-consing, so the same atom maps
   to the same SAT variable — and the same theory atom — throughout the session. The
   theory is installed on the pristine SAT core at [create] (before any clause), per the
   seam's attach contract; {!Cdclt} owns the seam glue. *)

open Oxsmt_core
module Sat = Oxsmt_solver.Sat
module Preprocess = Oxsmt_preprocess.Preprocess
module Cnf = Oxsmt_preprocess.Cnf
module Lia = Oxsmt_lia.Lia
module Rational = Oxsmt_lia.Rational
module Combine = Oxsmt_combine.Combine
module Ematch = Oxsmt_ematch
module Manager = Oxsmt_ematch.Manager
module Qvar = Oxsmt_ematch.Qvar
module Instance = Oxsmt_ematch.Instance
module Lemma = Oxsmt_ematch.Lemma

(* Generous, deterministic per-check-sat cap on theory splits (B&B branches / N-O equality
   splits). Exhaustion routes to [unknown] — never a verdict from an unfinished search
   (I6: a counter, never wall-clock). Overridable at {!create} (tests drive the exhaustion
   path with a tiny budget). *)
let default_split_budget = 10_000

type verdict =
  | Sat
  | Unsat
  | Unknown

type model_value = Cdclt.value =
  | VBool of bool
  | VInt of Bigint.t
  | VUninterp of int

type fun_table = Cdclt.fun_table =
  { default : model_value
  ; cases : (model_value list * model_value) list
  }

type model_binding = Cdclt.binding =
  | Const of string * model_value
  | Fun of string * fun_table

type sort_card = Cdclt.sort_card =
  { sort_name : string
  ; card : int
  }

(* The full reconstructed model: uninterpreted-sort cardinalities + symbol bindings. *)
type model = sort_card list * model_binding list

type t =
  { env : Env.t
  ; cap : Env.reserved_cap
      (* ADR-0012 R1: the reserved-minting capability for [env], kept private (never
         returned by [Session.env]) and threaded only to the legitimate minters —
         preprocessing and the lemma tier's [Qvar.mint]. *)
  ; ctx : Context.t
  ; registry : Oxsmt_core.Datatype_defs.t ref
      (* datatype declarations (GOALS Datatypes); empty unless [set_datatypes] was called.
         A ref SHARED with [cdclt] (same ref), so a [set_datatypes] after [create] is
         visible when cdclt reads it lazily at the first theory-atom intern to pick the
         standalone DT theory over the EUF+LIA combined stack. *)
  ; array_registry : Oxsmt_core.Array_defs.t ref
      (* array select/store symbols (arrays lane); empty unless [set_arrays] was called. A
         ref SHARED with [cdclt] (same ref), read lazily at the first theory-atom intern
         to pick the standalone arrays theory. *)
  ; mutable has_arrays : bool
      (* [set_arrays] installed a non-empty array registry. A [Final]->[Sat] on an array
         problem degrades to [Unknown] in v1: the ROW/extensionality saturation is sound
         for refutation but the model is not self-checked, so [sat] is withheld rather
         than risk a wrong-[sat]. UNSAT flows through unchanged. *)
  ; pp : Preprocess.t
  ; sat : Sat.t
  ; cdclt : Cdclt.t
  ; mgr : Manager.t
      (* ADR-0012 lemma tier: the store + instantiation manager, threaded alongside the
         Context/Cdclt. Frame-scoped in lockstep with [frames] via [Manager.on_pop]. *)
  ; prop_to_var : Sat.var Term.Table.t
      (* one SAT var per distinct propositional-variable term (nullary Bool [App]);
         auxiliary Tseitin variables are per-formula. Shared via hash-cons identity. *)
  ; mutable bool_consts : (string * Sat.var) list
      (* nullary Bool-App atoms (propositional variables), for the pure-Boolean
         [get_model] *)
  ; mutable frames : Sat.var list
      (* selector stack, innermost first; base always present (the outermost / last
         element) *)
  ; base_at_level0 : bool
      (* OXSMT_BASE_L0 (DEFAULT-ON; set to 0/false/no to opt out): the unpoppable base
         frame is forced TRUE by a permanent unit clause at level 0 instead of being
         ASSUMED positive on every solve. Removes the artificial level-1 [base] decision
         and keeps [not base] out of learned clauses (a sound search/encoding change — NOT
         a no-op: it shifts decisions/LBD). When true, [base] is omitted from the solve
         and certificate assumption sets; pushed frame selectors and the symmetry
         activation selector are unaffected. *)
  ; base_var : Sat.var (* the base-frame selector, for the level-0 forcing unit *)
  ; mutable base_unit_emitted : bool
      (* under [base_at_level0], the permanent [base] unit is added LAZILY on the first
         [check_sat] rather than at [create] — so that if a certificate trace was
         installed (which happens on a pristine session, after [create] but before any
         solve) the unit is captured as a genuine cert Input (the definitional
         selector-unit), and the checker DERIVES [base] by BCP over inputs. Emitted
         exactly once. *)
  ; mutable has_theory : bool
      (* any theory atom (Le / non-Bool Eq / applied predicate) has been asserted: the
         verdict's model comes from the theory, and a Sat is theory-validated *)
  ; mutable degraded : bool
      (* Overflow/Unsupported/poison/budget seen: verdict must be Unknown (I8,
         CONTRACT-POISON) *)
  ; mutable degraded_reason : string
      (* census (task #78): the FIRST cause that set [degraded] (first-wins, sticky, never
         reset), consulted at the pre-solve degraded gate. Empty until [degrade] fires.
         Diagnostic-only (surfaced by the dev CLI on stderr); no verdict/counter effect. *)
  ; mutable unknown_reason : string
      (* census (task #78): the resolved cause of the MOST RECENT check_sat's [Unknown],
         reset to "" at each check_sat entry and set at every giveup site. Surfaced by
         {!last_unknown_reason}; diagnostic-only, never read by the solver itself. *)
  ; mutable last_verdict : verdict
      (* verdict of the most recent check_sat, for get_model *)
  ; mutable last_model : model option
      (* the self-checkable model of the most recent [Sat], reconstructed in [check_sat] *)
  ; mutable asserted : Term.t list
      (* the ACTIVE ORIGINAL asserted terms (pre-preprocessing), for the R1 in-process
         model self-check. Frame-scoped in lockstep with [frames] (F3): a [push] snapshots
         it onto [asserted_saved] and a [pop] restores that snapshot, so a retracted
         frame's assertions do NOT linger — [Model_check] evaluates the current active
         set, never a popped assertion (which would spuriously reject a valid post-pop
         [Sat]). *)
  ; mutable asserted_saved : Term.t list list
      (* [asserted] snapshots saved at each [push], innermost first; one per non-base
         frame (so [length asserted_saved = length frames - 1]). Restored by [pop]. *)
  ; mutable last_splits : int (* splits used by the most recent check_sat (stat) *)
  ; mutable budget_exhausted : bool (* the most recent check_sat hit the split budget *)
  ; mutable last_effort : int
      (* effort consumed by the most recent check_sat (board #60) *)
  ; mutable effort_exhausted : bool
      (* the most recent check_sat hit the effort budget (BUDGET tag). Per-check,
         poison-free: distinct from [degraded]/[budget_exhausted], NOT sticky. *)
  ; mutable elim_defs : Presolve.def list
      (* W1b equality-elimination presolve: the variables {!assert_presolved} eliminated,
         in elimination order. [build_model] re-derives each one's value from its
         definition and splices it into the model so the R1 checker (which evaluates the
         ORIGINAL assertions in [asserted]) and [get_model] both bind it. Empty unless the
         batch {!assert_presolved} path eliminated something. *)
  ; relevancy : Relevancy.t option
      (* dynamic relevancy driver (task #24, QF_UF), [None] unless the [OXSMT_RELEVANCY]
         gate is on (or {!create} is told to enable it). When [Some], {!assert_clausified}
         feeds it the boolean-skeleton graph and the SAT core's branch filter consults it;
         when [None] the whole feature is dark and byte-identical to trunk. *)
  ; mutable cert_active : bool
      (* set by {!install_cert_trace}: a certificate trace is installed. Pass A
         (entailed-equality extraction, task #7) is gated OFF while true — a derived unit
         would otherwise enter the cert as a trusted [Input], laundering a preprocessing
         consequence into the query and blinding the gate (codex MED-3/4). Cert corpus
         runs are a SOUNDNESS gate, not a solve-rate target, so forgoing Pass A there is
         free. *)
  ; sym_counter : int ref
      (* symmetry breaking (task #25): a PER-SESSION monotone counter for the reserved
         [.oxsmt.sym.*] aux-var names, so a second [assert_presolved] emission does not
         reuse a name from the first (F2: idempotent [declare_reserved] would rebind it to
         a conflicting definition). *)
  ; mutable lemmas_registered : bool
      (* symmetry breaking (task #25, R2/codex B2): set once any lemma is registered. The
         emission restriction refuses to emit when true — a during-solve lemma instance
         ([assert_instance_at_frame]) extends the formula and can break the detected
         symmetry, and [check_sat] builds its assumption list once, so an emission could
         not be retracted mid-solve. *)
  ; mutable sym_sel : Sat.var option
      (* symmetry breaking (task #25, F1): the ACTIVATION SELECTOR guarding the current
         emission's lex clauses. The clauses are asserted as [(¬sym_sel ∨ C)] (via
         [assert_clausified ~sel]); [check_sat] assumes [sym_sel] POSITIVE while [Some],
         so the clauses are active. [sym_sel] occurs only negatively (a pure literal), so
         once a later assertion clears it to [None] the clauses become vacuous — sound
         retraction of a NON-MONOTONIC break without touching the permanent clause DB. Any
         assertion after emission (assert_term / a further assert_presolved / push) clears
         it. *)
  ; mutable sym_sel_in_core : Sat.var option
  (* symmetry breaking (task #25, R3 minor): the activation selector assumed by the MOST
     RECENT [check_sat], captured at solve time. [failed_assumptions] filters by THIS, not
     the live [sym_sel] — a later assertion clears [sym_sel] to [None] while the SAT core
     still holds the selector from the previous solve, so a read-time filter keyed on the
     live [sym_sel] would leak it. *)
  }

let create
  ?(split_budget = default_split_budget)
  ?max_effort
  ?lemma_gen_budget
  ?(enable_relevancy = Relevancy.enabled_from_env ())
  ?seed_lemmas:seed_lemmas_override
  ()
  =
  (* ADR-0012 R1: the session is the SOLE caller of [create_with_cap] in solver code (the
     documented convention); it keeps the cap private and threads it to the
     reserved-symbol minters. [Session.env] returns only the [env], never the cap. *)
  let env, cap = Env.create_with_cap () in
  let ctx = Context.create env in
  (* OXSMT_BASE_L0 (DEFAULT-ON): force the unpoppable base frame TRUE with a permanent
     level-0 unit rather than assuming it every solve. Read the gate ONCE here (before
     [Sat.create] so the emitter knob can be set); [check_sat] / [cert_assumptions]
     consult [base_at_level0] to omit [base] from their assumption sets. [OXSMT_BASE_L0]
     set to 0/false/no opts out and is byte-identical to the pre-flip trunk. *)
  let base_at_level0 =
    (* DEFAULT-ON (pair-measured +113 on main; QF_LIA +87 / QF_UF +29 / UFLIA −3, 0 flips,
       0 z3 disagreements). [OXSMT_BASE_L0] set to 0/false/no opts out (byte-identical to
       the pre-flip trunk); any other value / unset ⇒ ON. The opt-out token set is the
       SAME as [OXSMT_SYMBREAK] (whose flip precedent this mirrors), so [=false] / [=no]
       behave as expected rather than surprisingly turning the flag ON. The cert-emitter
       (#53) makes the ON path 33/33 cert-VALID, so default-ON is safe for the certificate
       pipeline. *)
    match Sys.getenv_opt "OXSMT_BASE_L0" with
    | Some ("0" | "false" | "no") -> false
    | Some _ | None -> true
  in
  (* OXSMT_LEMMA_SEED (DEFAULT-ON, chunk 3): MBQI-lite ground-term seeding of a
     trigger-inert universal lemma (see {!Oxsmt_ematch.Manager}). Same opt-out token set
     as the flip precedents above; [=0]/[=false]/[=no] builds the seeding-disabled mutant
     (the RED baseline). Seeding only ADDS sound ground consequences of live lemmas, so it
     can only turn an [unknown] into [unsat] (a refutation the inert lemma enables), never
     change a verdict unsoundly. *)
  let seed_lemmas =
    match seed_lemmas_override with
    | Some b -> b (* explicit test override wins over the env gate *)
    | None ->
      (match Sys.getenv_opt "OXSMT_LEMMA_SEED" with
       | Some ("0" | "false" | "no") -> false
       | Some _ | None -> true)
  in
  (* Under base-l0 the redundant level-0-unit cert DECLARATIONS ([on_unit]) are suppressed
     (base #53): a base-frame input unit that a level-0 theory conflict retracts in the
     checker's contradictory closure would otherwise spuriously fail the "declared level-0
     unit entailed" check, though the E3 refutation is valid. Emitter-only; no verdict/
     counter effect. The opt-out (not base-l0) keeps every declaration => byte-identical
     to the pre-flip trunk. *)
  let sat = Sat.create ~base_l0_cert_mode:base_at_level0 () in
  (* One shared effort budget for the session (board #60). [max_effort = None] is
     unbounded — it still COUNTS (for instrumentation) but never cuts off, so the default
     / interactive / [make test] path is byte-identical (the count is never printed). *)
  let budget = Budget.create ?max:max_effort () in
  let registry = ref Oxsmt_core.Datatype_defs.empty in
  let array_registry = ref Oxsmt_core.Array_defs.empty in
  (* Install the seam callbacks on the pristine core BEFORE any clause (pristine-attach);
     the theory itself is chosen lazily from [registry] / [array_registry] at the first
     intern. The refs are shared with [cdclt]. *)
  let cdclt =
    Cdclt.create ctx env sat ~split_budget ~budget ~registry ~array_registry ~cap
  in
  let base = Sat.new_var sat in
  (* [base_at_level0] is read above (before [Sat.create]). The base forcing-unit is NOT
     added here — it is deferred to the first [check_sat] (see [base_unit_emitted]) so a
     certificate trace, installed after [create] on the pristine session, records it as a
     genuine Input. *)
  (* Dynamic relevancy (task #24): when enabled, create the driver, route the trail seam
     events through [cdclt] to it, and install the SAT branch filter that consults it.
     Disabled by default => the filter is never installed and the glue is byte-identical
     to trunk. *)
  let relevancy =
    if enable_relevancy
    then Some (Relevancy.create ~activity:(fun v -> Sat.var_activity sat v) ())
    else None
  in
  (match relevancy with
   | None -> ()
   | Some rel ->
     Cdclt.set_relevancy cdclt (Some rel);
     Sat.set_branch_filter sat (Some (fun v -> Relevancy.should_branch rel v)));
  { env
  ; cap
  ; ctx
  ; registry
  ; array_registry
  ; has_arrays = false
  ; pp = Preprocess.create cap env ctx
  ; sat
  ; cdclt
  ; mgr = Manager.create ?gen_budget:lemma_gen_budget ~seed:seed_lemmas ctx env
  ; prop_to_var = Term.Table.create 256
  ; bool_consts = []
  ; frames = [ base ]
  ; base_at_level0
  ; base_var = base
  ; base_unit_emitted = false
  ; has_theory = false
  ; degraded = false
  ; degraded_reason = ""
  ; unknown_reason = ""
  ; last_verdict = Unknown
  ; last_model = None
  ; asserted = []
  ; asserted_saved = []
  ; last_splits = 0
  ; budget_exhausted = false
  ; last_effort = 0
  ; effort_exhausted = false
  ; elim_defs = []
  ; relevancy
  ; cert_active = false
  ; sym_counter = ref 0
  ; sym_sel = None
  ; lemmas_registered = false
  ; sym_sel_in_core = None
  }
;;

(* census (task #78): degrade the session to [Unknown] (I8 / CONTRACT-POISON) and record
   the FIRST cause (first-wins, so the earliest / most specific reason survives). This is
   a pure diagnostic wrapper over the pre-existing [t.degraded <- true] discipline: the
   [degraded] bit and its verdict effect are unchanged; only [degraded_reason] is added. *)
let degrade t reason =
  t.degraded <- true;
  if String.length t.degraded_reason = 0 then t.degraded_reason <- reason
;;

(* census (task #78): sanitize an exception string into a short reason-safe token — the
   first identifier-ish run, no spaces / parens / newlines (the CLI's reason grammar stops
   at ')' and one line). Keeps the poison sub-cause visible without leaking payloads. *)
let san_token s =
  let b = Buffer.create 32 in
  (try
     String.iter
       (fun c ->
         if Buffer.length b >= 40 then raise Exit;
         match c with
         | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '.' | '-' -> Buffer.add_char b c
         | ' ' | '(' -> raise Exit
         | _ -> ())
       s
   with
   | Exit -> ());
  Buffer.contents b
;;

let exn_tag e = san_token (Printexc.to_string e)

(* Pass A (task #7 entailed-equality extraction) toggle. Default ON (both review legs
   cleared default-ON; the win is ~84-95 eq_diamond files and OFF forfeits it).
   [OXSMT_PRESOLVE_EQ=0] turns it OFF for the A/B baseline. Cert-OFF gating (below) is
   INDEPENDENT of this flag: a live cert trace disables Pass A regardless. Read once. *)
let pass_a_flag =
  lazy
    (match Sys.getenv_opt "OXSMT_PRESOLVE_EQ" with
     | Some ("0" | "false" | "no") -> false
     | _ -> true)
;;

(* Pass A runs only when enabled AND no certificate trace is installed (§cert-OFF gating,
   team-lead ruling): a derived unit must never enter a cert as a trusted [Input]. *)
let pass_a_enabled t = Lazy.force pass_a_flag && not t.cert_active

(* Contextual simplification (task #13) toggle. Default OFF (both review legs concur): the
   pass collapses the nested-ITE verification conditions (nec-smt / Dartagnan) that
   CDCL(T) otherwise thrashes on, and its win direction is UNSAT where the R1 self-check
   does not run — so, matching the Pass A precedent, it ships OFF (byte-identical to
   trunk) until a fires-inclusive ON/OFF 0-mismatch corpus sweep is recorded; a follow-up
   then flips the default. [OXSMT_PRESOLVE_CTX=1] turns it ON (the A/B ON leg and the
   wiring-test gate). Read once. *)
let ctx_simp_flag =
  lazy
    (match Sys.getenv_opt "OXSMT_PRESOLVE_CTX" with
     | Some ("1" | "true" | "yes") -> true
     | Some _ | None -> false)
;;

(* Contextual simplification runs only when enabled AND no certificate trace is installed:
   the certificate measures the UNSIMPLIFIED assertion path, so the rewrite must be off
   while a trace is live (the same cert-OFF discipline as Pass A). The rewrite is
   model-preserving, so this gate protects the certificate contract, not verdict
   soundness. *)
let ctx_simp_enabled t = Lazy.force ctx_simp_flag && not t.cert_active

(* Equality-over-ITE projection (task #34) toggle. Default ON as of the recorded A/B.
   MEASURED BASIS (builder-cc-incremental, quiesced box, 2s W=1, 101-file tier-weighted
   nec sample, OXSMT_PRESOLVE_PROJ 0 vs 1 interleaved): OFF solved=2 -> ON solved=9 (NET
   +7), 0 verdict disagreements, 0 regressions; all conversions fast timeout->unsat, and
   oxsmt(ON) beats z3 4.8.5 on 2 of them (int_from_list/prp-39-34,
   handler_sigchld/prp-22-47: z3 times out at 2s, we prove unsat). Zero regressions
   confirms the neutral-abort keeps the deepest ITE-chain files (which the 500K budget
   forfeits) at OFF-equivalent behaviour. (The proj: fire-count on the large tier is
   under-observed because a still-timing-out file is SIGKILL'd before the once-per-solve
   stderr line flushes — measurement-only; verdicts/net come off stdout and are
   unaffected.) [OXSMT_PRESOLVE_PROJ=0] turns it OFF (byte-identical to trunk); read once.
   Distinct flag from OXSMT_PRESOLVE_CTX so the two ITE passes stay independent (the
   contextual pass is a banked negative). *)
let proj_flag =
  lazy
    (match Sys.getenv_opt "OXSMT_PRESOLVE_PROJ" with
     | Some ("0" | "false" | "no") -> false
     | Some _ | None -> true)
;;

(* Runs only when enabled AND no certificate trace is installed: the certificate measures
   the UNSIMPLIFIED assertion path (the same cert-OFF discipline as Pass A / ctx). The
   rewrite is model-preserving, so this gate protects the certificate contract, not
   verdict soundness. *)
let proj_enabled t = Lazy.force proj_flag && not t.cert_active

(* Symmetry breaking (task #25, quf-symmetry-experiment.md §6) toggle. Default ON as of
   the quiesced A/B (logs/symbreak-arbiter.md, be8e516b8b binary): the SOUND size-capped
   lex-leader gives NET +972 on QF_UF (81.5% -> 94.4%), 0 verdict disagreements, 0
   verdict-vs-:status contradictions; ON lost only 12 files, all near-wall Goel-BMC
   timeouts (none wrong) — a known follow-up (a GENERAL detection-cost budget, NOT a
   family/shape guard). Mirrors the ITE-projection flip precedent. [OXSMT_SYMBREAK=0]
   turns it OFF (byte-identical to pre-flip trunk). Read once. *)
let symbreak_flag =
  lazy
    (match Sys.getenv_opt "OXSMT_SYMBREAK" with
     | Some ("0" | "false" | "no") -> false
     | Some _ | None -> true)
;;

(* Runs only when enabled AND no certificate trace is installed: the breaking constraints
   REMOVE symmetric models (equisatisfiable, not equivalent), so a lex-leader clause is
   not resolution-derivable — it must never enter a cert as a trusted input (the same
   cert-OFF discipline as Pass A). Cert-OFF here protects soundness of the certificate,
   not the verdict (the added constraints are equisatisfiable). *)
let symbreak_enabled t = Lazy.force symbreak_flag && not t.cert_active
let env t = t.env
let context t = t.ctx

(* board #58 O-MINTER — the MARKER-GRAMMAR REGISTRATION SITE.
   [parse_sanctioned_marker name] is the [admit] gate for the front-end minter
   {!parse_minter}: exactly the parse-time theory-internal names a session lets the
   SMT-LIB parser mint. It admits the arrays and bit-vector marker grammars (below); a
   caller holding only a [Session.t] can mint those names and nothing else (the O-MINTER
   close).

   {b PAIRING CONTRACT — a theory migration widening this MUST read it (see
     Oxsmt_core.Internal_minter.create).}
   Admitting a marker grammar here lets any [Session.t] holder mint those names via
   [Internal_minter.mint]. That is sound ONLY IF the theory's CONSUMING side classifies
   its markers by something the holder cannot forge to a harmful effect — REGISTRY
   MEMBERSHIP (arrays: a marker-shaped-but-unregistered op gets no ROW) or RANK AGREEMENT
   (bv: a mis-ranked marker is inert) — so a forged-but-admitted marker degrades to
   [unknown], never a wrong verdict. Banked lesson (mint-exemption-tcb-hole): an admission
   is a wrong-[unsat] hole precisely when the consuming theory classifies on the SAME
   forgeable thing it admits. So DO NOT add a grammar arm here without a paired
   consuming-side inertness check, and NEVER admit the sensitive reserved namespaces
   ([.oxsmt.arr.ext.*], datatype testers [.oxsmt.is-*]/[.oxsmt.dt.*], qvars
   [.oxsmt.qvar.*], preprocessing witnesses [.oxsmt.ite/q/r.*]) — those are minted
   directly via [Env.declare_reserved] by trusted code and have no inertness guard.

   ADMITTED GRAMMARS (one predicate per line; each PAIRED with its consuming-side
   inertness check per the contract above):
   - arrays op symbols ({!Array_defs.is_op_name}: the [.oxsmt.arr.] prefix with a [|]
     sort-key separator). PAIRED check = REGISTRY MEMBERSHIP: the theory classifies an
     [App] head only via {!Array_defs.role_of_sym}, and {!Array_defs.add} refuses any
     entry whose name is not the canonical [op_symbol_name], so an
     admitted-but-unregistered op-shaped mint is inert. EXCLUDES the ext witness
     [.oxsmt.arr.ext.N] (no [|]).
   - bit-vector markers ({!Oxsmt_core.Bv.is_bv_name}: the [.oxsmt.bv|...] prefix). PAIRED
     check = RANK AGREEMENT: {!Oxsmt_core.Bv.view} verifies the decoded op's
     operand/result sorts and arity against the term's actual sorts, so a mis-ranked
     admitted marker decodes to [None] (ordinary uninterpreted, at worst [unknown]), never
     reinterpreted. *)
let parse_sanctioned_marker name = Array_defs.is_op_name name || Bv.is_bv_name name
let parse_minter t = Internal_minter.create ~admit:parse_sanctioned_marker t.cap t.env

(* Declarations reject the reserved fresh-symbol namespace (board #48 / #58): every
   theory-internal symbol — a preprocessing witness, a coerced qvar, and (board #58) the
   bit-vector vocabulary's [.oxsmt.bv.*] operator/literal symbols ({!Oxsmt_core.Bv}) —
   lives under the [.oxsmt.] prefix, which [Preprocess.is_reserved_name] rejects here.
   This is the PRIMARY guard: a user symbol can never collide with an internal one, and
   the only door that mints a reserved name is the cap-gated {!Env.declare_reserved} (the
   bit-vector builders mint through it), which the public [Env] door below this guard
   cannot reach.

   They ALSO reject any name containing '\' or '|' (F2, codex BLOCKER) — retained as
   DEFENSE IN DEPTH. No SMT-LIB symbol form (simple or [|...|]-quoted) can contain either
   byte (the lexer forbids them), so a name carrying one can only arrive programmatically;
   this second, independent barrier covers the ['|'] field separator inside a bit-vector
   or arrays marker name and any future marker scheme, even were the [.oxsmt.] prefix
   guard ever weakened. The same rejection lives at the root [Env] door (board #58), so
   both the Session and raw-[Env] programmatic paths are closed. *)
let has_marker_byte name =
  String.exists (fun c -> Char.equal c '\\' || Char.equal c '|') name
;;

let guard_name name =
  if Preprocess.is_reserved_name name
  then
    invalid_arg
      (Printf.sprintf "Session: cannot declare reserved internal symbol %s" name);
  if has_marker_byte name
  then
    invalid_arg
      (Printf.sprintf
         "Session: cannot declare symbol %s (contains a reserved marker byte '\\' or '|')"
         name)
;;

let declare_sort t name =
  guard_name name;
  Env.declare_sort t.env name
;;

let declare_fun t name rank =
  guard_name name;
  Env.declare_fun t.env name rank
;;

let declare_const t name sort = declare_fun t name (Rank.create [] sort)

(* RESET-PER-QUERY theory invalidation (task #54, contract-A ruling — the
   correct-everywhere replacement for the #51 interim fail-closed guard). A datatype/array
   registry mutation after a prior query has already instantiated + cached a theory means
   the cached theory is stale for the new query (none->DT, DT->arrays, or a loader
   overwrite that re-ranks a symbol — the #51 codex wrong-[unsat] landmine). Invalidate
   it: drop the theory instance and the SAT-var<->atom bijection
   ({!Cdclt.reset_for_new_query}) plus the session-side per-query interning/model state,
   so the next [intern] rebuilds the theory fresh from the new registry and re-interns
   every (possibly re-used) term against it — no stale classification can survive, and the
   old [Dt.t]'s session-lifetime [ctor_terms] are discarded rather than met by a
   differently-populated registry.

   FAIL-LOUD, never a silent rebuild under live state: the reset is sound only BETWEEN
   self-contained queries. Any live state BOUND to the bijection we would drop makes the
   reset unsound, so we raise a documented [Invalid_argument] rather than reset under it.
   TWO such channels, both treated identically: (1) live ground assertions
   ([asserted <> []]); (2) a live quantified lemma ([Manager.has_live_lemma]) — the lemma
   Manager is USER-INPUT state (the ADR-0012 store fed by {!assert_lemma}), NOT a derived
   consequence, and it lives OUTSIDE [asserted] (a base-frame lemma is never added to
   [asserted] and survives [pop]), so silently dropping it in the new era would be a
   wrong-[sat] channel. The self-contained-VC pattern (declare -> assert -> check -> pop,
   no live lemma) reaches here clean and resets; the SAT core is at level 0 between
   queries and the prior query's now-inert vars/clauses cannot affect a later solve (their
   frame selector is free, and they are absent from the cleared bijection). *)
let invalidate_theory_for_registry_change t =
  (* FAIL-LOUD on ANY live state bound to the bijection we are about to drop. Two
     channels:
     (1) live ground assertions ([asserted <> []]); (2) a live quantified lemma
         ([Manager.has_live_lemma]) — the codex/fable CRITICAL. The lemma Manager is the
         ADR-0012 lemma store fed by {!assert_lemma}: USER-INPUT state, NOT a derived
         consequence, and it is OUTSIDE [asserted] (a base-frame lemma is never added to
         [asserted] and survives [pop]). Silently dropping a user-asserted quantifier in
         the new era would be a wrong-[sat] channel, so a live lemma is treated EXACTLY
         like a live assertion: the registry replacement raises rather than resetting
         under it. The self-contained-VC pattern (declare -> assert -> check -> pop, no
         live lemma) reaches here clean and resets. *)
  if (match t.asserted with
      | [] -> false
      | _ :: _ -> true)
     || Manager.has_live_lemma t.mgr
  then
    invalid_arg
      "Session: datatype/array registry replaced with live assertions or a live \
       quantified lemma (task #54 contract-A: each query's declarations must precede its \
       assertions / lemmas; pop the prior query before redeclaring for a new one)";
  Cdclt.reset_for_new_query t.cdclt;
  (* Session-side per-query state that maps terms -> SAT vars or caches the last verdict:
     cleared so a re-used term re-interns fresh and the new query starts unpoisoned.
     Frames / [asserted] / [asserted_saved] are NOT touched — an empty pushed frame is
     legitimate and its matching [pop] must still balance. *)
  Term.Table.clear t.prop_to_var;
  t.bool_consts <- [];
  t.has_theory <- false;
  (* Re-DERIVE [has_arrays] from the LIVE array registry rather than forcing it false: a
     DT-triggered reset must not drop a still-valid array mode (codex MEDIUM). The array
     registry is unchanged by a datatype mutation, so this preserves [has_arrays] across a
     [set_datatypes]/[declare_datatype] reset; [set_arrays] overwrites it from its own
     [defs] on the line after this call. *)
  t.has_arrays <- not (Oxsmt_core.Array_defs.is_empty !(t.array_registry));
  t.degraded <- false;
  t.last_model <- None;
  t.last_verdict <- Unknown;
  t.elim_defs <- [];
  t.sym_sel <- None;
  t.lemmas_registered <- false
;;

(* Install the algebraic-datatype shapes (GOALS Datatypes) the front end parsed. The
   caller has already declared the sorts/constructors/selectors/testers as ordinary
   symbols in {!env}; this records their datatype structure into the shared registry ref,
   which flips the session onto the DT theory at its first check-sat. Must precede
   [assert_term] (a datatype must be known before its atoms are interned). *)
let set_datatypes t defs =
  (* Install-door defense (mirrors [set_arrays] / [Array_defs.validate_ranks]): reject a
     registry that marks a symbol as a constructor/selector/tester without that role's
     canonical datatype rank in the env — e.g. a forged registry marking an
     uninterpreted-sort constant as a constructor, which would otherwise slip the
     symmetry-breaking free-constant test and drive other DT wrong-verdicts. Every
     well-formed registry (parser / [declare_datatype]) installs cleanly. *)
  Oxsmt_core.Datatype_defs.validate_ranks defs ~rank_of:(fun sym ->
    match Oxsmt_core.Env.rank t.env sym with
    | r -> Some r
    | exception Not_found -> None);
  (* Task #54 reset-per-query. Invalidate the cached theory when this REPLACE actually
     involves datatypes (new or currently-installed) — never on a pure-logic no-op
     ([set_datatypes empty] on a session with no datatypes), which keeps the batched
     pure-logic path byte-identical (the #51 interim guard wrongly degraded it too). *)
  if Cdclt.theory_instantiated t.cdclt
     && not
          (Oxsmt_core.Datatype_defs.is_empty defs
           && Oxsmt_core.Datatype_defs.is_empty !(t.registry))
  then invalidate_theory_for_registry_change t;
  t.registry := defs
;;

let uses_datatypes t = not (Oxsmt_core.Datatype_defs.is_empty !(t.registry))
let uses_arrays t = t.has_arrays

(* Install the array [select]/[store] symbol registry (arrays lane) the front end parsed.
   Records it into the shared registry ref, which flips the session onto the standalone
   arrays theory at its first theory-atom intern. Must precede [assert_term]. A non-empty
   registry also arms the v1 sat-degrade ([has_arrays]). *)
let set_arrays t defs =
  (* Defence in depth: reject a registry whose operators were minted at a non-canonical
     rank (a canonical [.oxsmt.arr.*] NAME can be minted at any arity via the internal
     minter, whose admit gate is name-shape only, and [Array_defs.add] validates the name
     but not the rank). Without this the arrays theory could apply read-over-write to an
     extended-arity uninterpreted function — a wrong verdict. The arrays theory's
     consuming-side arity guards are the second layer. *)
  Oxsmt_core.Array_defs.validate_ranks defs ~rank_of:(fun sym ->
    match Oxsmt_core.Env.rank t.env sym with
    | r -> Some r
    | exception Not_found -> None);
  (* Task #54 reset-per-query (same as [set_datatypes]): invalidate the cached theory when
     this REPLACE actually involves arrays (new or currently-installed); a pure-logic
     no-op ([set_arrays empty] on a session with no arrays) resets nothing and stays
     byte-identical. [invalidate_theory_for_registry_change] RE-DERIVES [has_arrays] from
     the live array registry (still the OLD one at that point); the line below then
     overwrites it from [defs], so a non-array query following an array query is not left
     with a stale [has_arrays]. *)
  if Cdclt.theory_instantiated t.cdclt
     && not
          (Oxsmt_core.Array_defs.is_empty defs
           && Oxsmt_core.Array_defs.is_empty !(t.array_registry))
  then invalidate_theory_for_registry_change t;
  t.array_registry := defs;
  t.has_arrays <- not (Oxsmt_core.Array_defs.is_empty defs)
;;

(* One constructor for the programmatic {!declare_datatype} door: its name and each
   field's (selector name, sort). A nullary constructor (an enum case) has [fields = []]. *)
type ctor_decl =
  { ctor_name : string
  ; fields : (string * Oxsmt_core.Sort.t) list
  }

(* Declare an ADT and its constructors programmatically (the Session-API path, distinct
   from the .smt2 parser which builds a Datatype_defs itself). Constructor and selector
   symbols mint normally; each TESTER mints in the RESERVED [.oxsmt.*] namespace via the
   session cap (ADR-0012), so a user function cannot forge [is-C] and silently shadow the
   tester in the printed session the Lean oracle checks — the TCB printer-suppression
   hole. [sort] must be the datatype's [Sort.Datatype] (declared first via
   {!declare_sort} + [Sort.datatype_], so a recursive field can reference it). Returns the
   built {!Oxsmt_core.Datatype_defs.datatype} (all minted symbols) and adds it to the
   session registry, installing the DT theory. Must precede [assert_term]. *)
let declare_datatype t sort constructors =
  let sort_sym =
    match (sort : Oxsmt_core.Sort.t) with
    | Datatype s -> s
    | Bool | Int _ | Uninterpreted _ | Array _ | BitVec _ ->
      invalid_arg "Session.declare_datatype: sort must be a Sort.Datatype"
  in
  let ctors =
    List.map
      (fun { ctor_name; fields } ->
        let ctor_sym = declare_fun t ctor_name (Rank.create (List.map snd fields) sort) in
        let selectors =
          List.mapi
            (fun i (sel_name, field_sort) ->
              let sym = declare_fun t sel_name (Rank.create [ sort ] field_sort) in
              { Oxsmt_core.Datatype_defs.sym; index = i; field_sort })
            fields
        in
        (* Reserved tester: minted through the session's private cap so its
           [.oxsmt.is-<C>] name is un-forgeable on the public declaration doors. *)
        let tester_name = Printf.sprintf "%sis-%s" Env.reserved_prefix ctor_name in
        let tester =
          Env.declare_reserved t.cap t.env tester_name (Rank.create [ sort ] Sort.bool)
        in
        { Oxsmt_core.Datatype_defs.sym = ctor_sym; selectors; tester })
      constructors
  in
  let dt = { Oxsmt_core.Datatype_defs.sort_sym; constructors = ctors } in
  (* Task #54 reset-per-query. The additive door also invalidates a stale cached theory:
     adding the FIRST datatype after a pure-logic query instantiated the combined theory
     (none->DT), or a further datatype after the DT theory was cached (the #51 accumulate
     pattern), leaves the cached theory unable to serve the new query. A fresh reset
     re-picks the DT theory against the grown registry at the next intern. Only fires once
     a theory is instantiated (i.e. between queries — a single query declares before its
     first [check_sat], so this is inert there and single-query behavior is unchanged). *)
  if Cdclt.theory_instantiated t.cdclt then invalidate_theory_for_registry_change t;
  t.registry := Oxsmt_core.Datatype_defs.add !(t.registry) dt;
  dt
;;

(* A theory atom is anything the propositional core cannot itself reason about: an order
   atom, a non-Bool equality, or an applied (arity >= 1) predicate. A nullary Bool [App]
   is a plain propositional variable, and [Bool_const] is a constant — neither is a theory
   atom. *)
let is_theory_atom (a : Term.t) =
  match a.node with
  | Le _ -> true
  | Eq _ -> true (* atom Eq always has non-Bool args (Bool-Eq is a connective) *)
  | App (_, args) -> Iarr.length args > 0
  | Bool_const _ -> false
  | Int_const _ | Arith _ | Not _ | And _ | Or _ | Ite _ -> false
;;

let current_selector t = List.hd t.frames

(* The frame selectors to ASSUME positive on a solve. Normally all of [frames]; under
   [base_at_level0] the base frame (the last / outermost element) is forced true by a
   permanent unit at level 0 (see [create]) and is therefore NOT assumed — dropping it
   removes the artificial [base] decision level. Pushed frame selectors are always kept,
   so their retraction-on-[pop] contract is unchanged. *)
let assumed_frames t =
  if t.base_at_level0
  then (
    match List.rev t.frames with
    | _base :: pushed_rev -> List.rev pushed_rev
    | [] -> [])
  else t.frames
;;

(* The persistent propositional SAT var for a NON-theory atom (a nullary Bool [App] / a
   [Bool_const]), shared per distinct hash-consed term via [prop_to_var]. A nullary Bool
   variable is also recorded in [bool_consts] so the model carries its value. Extracted so
   {!register_bool_terms} can obtain (or mint) the SAME var it later binds into EUF —
   keeping one SAT variable per term. *)
let prop_var_of t (atom : Term.t) =
  match Term.Table.find_opt t.prop_to_var atom with
  | Some sv -> sv
  | None ->
    let sv = Sat.new_var t.sat in
    Term.Table.add t.prop_to_var atom sv;
    (match atom.node with
     | App (sym, args) when Iarr.length args = 0 && Sort.equal atom.sort Sort.bool ->
       t.bool_consts <- (Symbol.name sym, sv) :: t.bool_consts
     | _ -> ());
    sv
;;

(* Map a clausified formula's local variable to a persistent SAT variable. Theory atoms go
   through {!Cdclt} (one SAT var 1:1 with a theory atom, registered with the combined
   theory); a propositional variable (nullary Bool [App]) shares one SAT var per distinct
   term; auxiliary Tseitin variables are fresh per formula (kept in [local]). *)
let assert_clausified ?sel ~root t cnf =
  let n = Cnf.num_vars cnf in
  let local = Array.make (n + 1) None in
  let sat_var v =
    if Cnf.is_atom_var cnf v
    then (
      let atom = Cnf.subterm_of_var cnf v in
      if is_theory_atom atom
      then (
        t.has_theory <- true;
        Cdclt.intern_atom t.cdclt atom)
      else prop_var_of t atom)
    else (
      match local.(v) with
      | Some sv -> sv
      | None ->
        let sv = Sat.new_var t.sat in
        (* A10: this is the SOLE site that mints a pure Tseitin auxiliary variable — fresh
           per formula, never recorded in [prop_to_var]/[bool_consts]/[frames], never a
           theory-seam atom (those go through [Cdclt.intern_atom] above), never named by a
           later clause or a theory lemma. Mark it eliminable so SAT preprocessing may
           eliminate it (no-op unless OXSMT_SATPRE is on). Every other [Sat.new_var] in
           the session stays frozen by default (read by a model path or the theory seam). *)
        Sat.set_eliminable t.sat sv;
        local.(v) <- Some sv;
        sv)
  in
  let lit_of (l : Cnf.Lit.t) =
    let sv = sat_var (Cnf.Lit.var l) in
    if Cnf.Lit.is_positive l then Sat.pos sv else Sat.neg sv
  in
  (* Default to the innermost frame's selector (an ordinary user assertion). A lemma
     instance overrides [sel] with its OWNING lemma's frame selector (ADR-0012 §1.4 R2),
     so the instance retracts with the lemma's frame, not whatever frame is innermost when
     it is drawn. *)
  let sel =
    match sel with
    | Some s -> s
    | None -> current_selector t
  in
  Cnf.iter_clauses
    (fun clause ->
      (* frame activation: clause holds only when the frame selector is assumed true *)
      Sat.add_clause t.sat (Sat.neg sel :: List.map lit_of clause))
    cnf;
  (* Dynamic relevancy graph (task #24): recover the boolean-skeleton And/Or/iff/Ite DAG
     over PERSISTENT SAT vars and hand it to the driver. Built AFTER clause emission so
     every var already exists (via [lit_of] above) and [sat_var] is a pure lookup here —
     the SAT var numbering is therefore identical to relevancy-off; only the branch order
     differs. A no-op when relevancy is disabled. *)
  match t.relevancy with
  | None -> ()
  | Some rel ->
    (* Invert [subterm_of_var] so a compound's Bool children resolve to their vars. *)
    let rev = Term.Table.create ((2 * n) + 1) in
    for v = 1 to n do
      Term.Table.replace rev (Cnf.subterm_of_var cnf v) v
    done;
    (* The (persistent SAT var, polarity) of a Bool child term, peeling [Not] for parity;
       [None] only if a child never took a var (not reachable for a well-formed skeleton —
       defensive). *)
    let rec child_lit (tm : Term.t) positive =
      match tm.node with
      | Not a -> child_lit a (not positive)
      | _ ->
        (match Term.Table.find_opt rev tm with
         | Some cv -> Some (sat_var cv, positive)
         | None -> None)
    in
    for v = 1 to n do
      let sv = sat_var v in
      if Cnf.is_atom_var cnf v
      then Relevancy.register_atom rel sv
      else (
        let node = Cnf.subterm_of_var cnf v in
        let kind, child_terms =
          match node.node with
          | And xs -> Some Relevancy.KAnd, Iarr.to_list xs
          | Or xs -> Some Relevancy.KOr, Iarr.to_list xs
          | Eq (a, b) -> Some Relevancy.KIff, [ a; b ]
          | Ite (c, a, b) -> Some Relevancy.KIte, [ c; a; b ]
          | Bool_const _ | Int_const _ | App _ | Arith _ | Le _ | Not _ -> None, []
        in
        match kind with
        | None -> ()
        | Some kind ->
          let opt_children = List.map (fun ct -> child_lit ct true) child_terms in
          if List.for_all Option.is_some opt_children
          then (
            let children = List.map Option.get opt_children in
            Relevancy.register_node rel ~var:sv ~kind ~children))
    done;
    (* Seed the top-level formula's root var relevant at level 0. *)
    (match child_lit root true with
     | Some (rv, _) -> Relevancy.seed_root rel rv
     | None -> ())
;;

(* Bool-cardinality rule (TODO Predicates §2; the one sanctioned finite sort). [Bool] has
   exactly two values, so every Bool-sorted term is true or false in every model. The
   clausifier ({!Cnf.clausify}) only surfaces Bool-sorted terms it reaches through the
   Boolean skeleton (top-level atoms + connective children); a Bool-sorted term that
   occurs ONLY buried in an uninterpreted-function ARGUMENT position gets no truth channel
   into EUF, so it stays a third opaque Boolean class distinct from
   [true_const]/[false_const]. When [n >= 3] such classes are forced pairwise-distinct by
   congruence the instance is pigeonhole-impossible, yet EUF cannot see it: {!Combine}'s
   H2 guard ([require_bool_args_bound]) degrades that to [unknown] (sound, never
   wrong-SAT), but it is INCOMPLETE. This walk closes the completeness half by giving
   every such buried Bool term a truth channel:

   - an APPLIED Bool predicate [p(x…)] (arity >= 1) becomes its own theory atom via
     {!Cdclt.intern_atom} (a fresh SAT var on the decision heap; EUF also propagates its
     truth by congruence). This is the original rule.

   - a BARE nullary Bool variable [b] used as a UF argument (e.g. [Concat (b, x)]) gets no
     fresh var — it is bound via {!Cdclt.bind_bool_var_atom} to the SAME [prop_to_var] SAT
     var that carries it propositionally, so the model reads its value from [bool_consts]
     while EUF binds it from the identical var (one variable per term, no
     propositional/EUF divergence). [prop_var_of] mints that var here if the variable
     never surfaced at top level. This is the bare-variable analogue the original rule
     missed — a nullary [App] is not a theory atom ({!is_theory_atom}), so it never
     reached [intern_atom], and a fresh theory var would have collided with the
     propositional one.

   Pigeonhole over the two Bool values is then discharged by congruence + the
   [true <> false] axiom. Both operations are idempotent, so a term that already surfaced
   is a no-op. UF-argument position is tracked top-down ([~under_uf], set when descending
   into an applied [App]'s arguments); the [seen] key includes it so a term reached both
   buried and at top level is visited in both contexts. Runs under the same
   try/CONTRACT-POISON discipline as clause registration (an out-of-fragment atom
   degrades, never crashes). *)
let register_bool_terms t (pterm : Term.t) =
  let seen : (int, unit) Hashtbl.t = Hashtbl.create 64 in
  let rec go ~under_uf (term : Term.t) =
    let key = (term.Term.tag lsl 1) lor Bool.to_int under_uf in
    if not (Hashtbl.mem seen key)
    then (
      Hashtbl.add seen key ();
      match term.node with
      | App (_, args) ->
        let is_uf = Iarr.length args >= 1 in
        if Sort.equal term.sort Sort.bool
        then
          if is_uf
          then (
            t.has_theory <- true;
            ignore (Cdclt.intern_atom t.cdclt term : Oxsmt_solver.Sat.var))
          else if under_uf
          then (
            t.has_theory <- true;
            Cdclt.bind_bool_var_atom t.cdclt term (prop_var_of t term));
        (* The arguments of an uninterpreted application are in UF-argument position. *)
        Iarr.iter (go ~under_uf:is_uf) args
      | Eq (a, b) ->
        go ~under_uf:false a;
        go ~under_uf:false b
      | Le a | Not a -> go ~under_uf:false a
      | And xs | Or xs -> Iarr.iter (go ~under_uf:false) xs
      | Ite (c, a, b) ->
        go ~under_uf:false c;
        go ~under_uf:false a;
        go ~under_uf:false b
      | Arith l -> Iarr.iter (fun (tm, _c) -> go ~under_uf:false tm) l.coeffs
      | Bool_const _ | Int_const _ -> ())
  in
  go ~under_uf:false pterm
;;

(* Preprocess -> clausify -> register a Bool term into the frame guarded by [sel]
   (default: the current innermost frame). Shared by [assert_term] and
   [assert_instance_at_frame]; the exception handling is the I8/CONTRACT-POISON
   assert-time discipline. *)
let assert_bool_at ?sel t pterm =
  match Cnf.clausify pterm with
  | exception _ -> degrade t "clausify-fail"
  | cnf ->
    (* Atom registration walks the theory engines; a rejected / out-of-fragment atom or an
       overflow escaping here degrades the whole session to unknown (I8). The
       internalization combinator raises [Combine.Incomplete] from [register_atom] (e.g. a
       structured Bool compound under a UF argument, ADR-0010 §3.6 case (ii)) — a
       DELIBERATE completeness degrade, distinct from a [Combination_unsound] fault, and
       it surfaces HERE at assert-time registration, so it must be caught on this ingress
       path too. *)
    (try
       assert_clausified ?sel ~root:pterm t cnf;
       (* Bool-cardinality rule: surface every buried Bool-sorted predicate application as
          its own SAT atom so the finite Bool sort is decided, not left opaque (see
          {!register_bool_terms}). Same term, same try-block, so an out-of-fragment buried
          atom degrades identically to a clause-borne one. *)
       register_bool_terms t pterm
     with
     | Combine.Incomplete msg -> degrade t ("combine-incomplete-register:" ^ san_token msg)
     | Term.Overflow
     | Term.Unsupported _
     | Rational.Overflow
     | Lia.Poisoned
     | Lia.Unsupported _
     | Invalid_argument _ -> degrade t "register-poison")
;;

(* ADR-0012 R1 / codex C1: the load-bearing assert-side gate rejects a user term carrying
   ANY reserved [.oxsmt.*] symbol — not just [.oxsmt.qvar.*]. [Symbol.intern] is public
   and preprocessing witnesses ([.oxsmt.ite/q/r.*]) acquire a rank in the env once minted,
   so a client could intern one and build a term that CAPTURES an internal witness ->
   wrong verdict (codex's ite-capture trigger). The single source of truth for the
   reservation is [Env.is_reserved_name]. [allowed] whitelists a specific lemma's own qvar
   symbols — the ONLY reserved symbols legitimately present in a lemma body/trigger (a
   ground user assertion whitelists nothing).

   board #58: array [select]/[store] op symbols also live in the reserved namespace
   ([.oxsmt.arr.<op>|<sortkey>|<sortkey>]) and DO appear as App heads in ordinary parsed
   assertions, so they must pass this gate: [bad_sym] exempts [Array_defs.is_op_sym]. That
   exemption is a name-shape test, sound because provenance is enforced at the minting
   door — only the cap-gated [Env.declare_reserved] grants a [.oxsmt.arr.*] name a rank,
   so an op-named symbol that reaches a built term is one the parser/theory minted, never
   a user alias (see [Array_defs.is_op_sym]). It does not touch the qvar/witness
   namespaces. *)
let term_has_reserved ?(allowed = []) (t0 : Term.t) =
  (* MEMOIZED over the hash-cons DAG. A user term is a maximally-shared DAG (the SMT-LIB
     [let] reader binds each value to one hash-consed node and reuses it by reference), so
     the naive per-path recursion re-walks a shared subterm once per path to it —
     exponential on let-heavy inputs (e.g. the nec-smt bounded-model-checking VCs). Keep a
     visited set of tags already proven CLEAN; [allowed] is fixed within one call, so a
     [false] result is a pure function of the subterm and safe to cache. A [true] short-
     circuits immediately (never cached). The sibling engine walks (Cdclt.collect,
     Combine.add_subterms / interface_walk) all guard the same way. *)
  let visited : (int, unit) Hashtbl.t = Hashtbl.create 256 in
  (* The bit-vector operator/literal symbols live in the reserved [.oxsmt.bv.*] namespace
     (board #58) but, unlike a preprocessing witness or a coerced qvar, they are theory
     VOCABULARY that legitimately appears in a user assertion — the interpreted-symbol
     analogue of [div]/[mod] (which [is_reserved_name] also excludes). They cannot be
     user-forged: the public declaration doors reject [.oxsmt.*], and a bare
     [Symbol.intern] of a bit-vector name has no rank so {!Context.app} refuses it — the
     only door that grants one is the cap-gated {!Env.declare_reserved} the bit-vector
     builders mint through. So exempting them here is sound and restores the pre-#58
     behaviour (before the migration these names were outside [.oxsmt.*], so the gate
     already let them through); it is what routes a pure-[QF_BV] assertion to the
     bit-blaster instead of degrading it. A mixed bit-vector/uninterpreted term still
     degrades at the combinator ([Combine.require_no_bitvec_terms], sort-keyed). *)
  let bad_sym s =
    Env.is_reserved_name (Symbol.name s)
    && (not (Bv.is_bv_sym s))
    && (not (Array_defs.is_op_sym s))
    && not (List.exists (Symbol.equal s) allowed)
  in
  (* A SORT carries a symbol too: an [Uninterpreted] sort over a reserved [.oxsmt.*] name,
     minted via the public [Symbol.intern] / [Sort.uninterpreted] doors, captures an
     internal reservation just as an [App] head does — a user term whose ONLY reserved
     symbol is sort-carried (e.g. a nullary constant of a reserved uninterpreted sort,
     whose [App] head is an innocuous user name) would otherwise slip the App-head-only
     walk. No [allowed] whitelist for sorts: [allowed] whitelists a lemma's own qvar
     App-head symbols, and a reserved uninterpreted sort is never a legitimate qvar (nor
     is one ever minted internally). *)
  let rec bad_sort (s : Sort.t) =
    match s with
    | Sort.Uninterpreted sym | Sort.Datatype sym -> Env.is_reserved_name (Symbol.name sym)
    (* An array sort carries its index/element sorts; recurse so a reserved symbol buried
       in one is caught. *)
    | Sort.Array (index, element) -> bad_sort index || bad_sort element
    | Sort.Bool | Sort.Int _ | Sort.BitVec _ -> false
  in
  (* Every subterm's own sort is checked here, so a reserved sort appearing anywhere in
     the term — in result OR argument position — is caught (an argument is itself a
     recursed subterm carrying that sort). *)
  let rec rec_ (t : Term.t) =
    if Hashtbl.mem visited t.Term.tag
    then false
    else (
      let r =
        bad_sort t.sort
        ||
        match t.node with
        | App (sym, args) -> bad_sym sym || Iarr.exists rec_ args
        | Arith l -> Iarr.exists (fun (tm, _c) -> rec_ tm) l.coeffs
        | Le a | Not a -> rec_ a
        | Eq (a, b) -> rec_ a || rec_ b
        | And xs | Or xs -> Iarr.exists rec_ xs
        | Ite (c, a, b) -> rec_ c || rec_ a || rec_ b
        | Bool_const _ | Int_const _ -> false
      in
      if not r then Hashtbl.replace visited t.Term.tag ();
      r)
  in
  rec_ t0
;;

(* Symmetry breaking (task #25, F1): drop the current activation selector so its lex
   clauses go vacuous on every future solve. Every assertion entry point that can EXTEND
   the formula after an emission calls this: a later assertion may break the detected
   symmetry, and the (permanent) lex clauses would then be NON-MONOTONIC — a wrong-unsat.
   The selector occurs only negatively in the clauses, so once it is no longer assumed
   positive the clauses are trivially satisfiable — sound retraction without touching the
   permanent clause DB. *)
let deactivate_symbreak t = t.sym_sel <- None

let assert_term t term =
  (* F1: an assertion after a symmetry-breaking emission may break the detected symmetry;
     retract the (non-monotonic) lex clauses first. *)
  deactivate_symbreak t;
  (* Load-bearing assert-side gate (R1 POINT 4 + codex C1): a user term carrying ANY
     reserved [.oxsmt.*] symbol (a coerced/interned qvar OR a captured preprocessing
     witness) degrades to a clean [Unknown] via the I8 Unsupported discipline (NOT a raw
     [Failure]) — never registered, never in a model, never capturing an internal aux. *)
  if term_has_reserved term
  then degrade t "reserved-symbol"
  else (
    t.asserted <- term :: t.asserted;
    match Preprocess.run t.pp term with
    | exception Term.Overflow -> degrade t "preprocess-overflow"
    | exception Term.Unsupported _ -> degrade t "preprocess-unsupported"
    | pterm -> assert_bool_at t pterm)
;;

(* Internalize a single (already-presolved) term WITHOUT recording it in [t.asserted]:
   preprocess -> clausify -> register, with the same I8/CONTRACT-POISON assert-time
   discipline as [assert_term]. Used by [assert_presolved] for the REDUCED conjuncts —
   [t.asserted] holds the ORIGINAL assertions (for R1), not the reduced ones. *)
let internalize_reduced t term =
  match Preprocess.run t.pp term with
  | exception Term.Overflow -> degrade t "overflow"
  | exception Term.Unsupported _ -> degrade t "unsupported"
  | pterm -> assert_bool_at t pterm
;;

(* Like {!internalize_reduced} but guards every emitted clause with [sel] (task #25 F1:
   the symmetry-breaking activation selector). *)
let internalize_reduced_at ~sel t term =
  match Preprocess.run t.pp term with
  | exception Term.Overflow -> degrade t "overflow"
  | exception Term.Unsupported _ -> degrade t "unsupported"
  | pterm -> assert_bool_at ~sel t pterm
;;

(* W1b equality-elimination presolve (logs/w1b-design.md). The BATCH entry point: given
   the whole asserted set at once (the CLI's parse result), run the {!Presolve} pass, then
   internalize the REDUCED set while keeping the ORIGINAL terms in [t.asserted] for the R1
   self-check and re-deriving the eliminated variables at model-build ([build_model]).

   Distinct from {!assert_term} (which internalizes one term eagerly): the pass needs the
   full set to collect aliases, so it cannot run term-by-term. Only the batch CLI path
   uses it; the incremental API ({!assert_term}/{!push}/{!pop}/lemmas) is unchanged. The
   reserved-symbol gate (R1 / codex C1) applies per term exactly as in {!assert_term}. On
   a zero-alias input the pass is a no-op ([reduced = originals], [defs = []]) and this is
   byte-identical to asserting each original with {!assert_term}. *)
let assert_presolved t terms =
  (* F1: a further batch after a prior emission may break that symmetry; retract the prior
     lex clauses before this batch (possibly) emits its own. *)
  deactivate_symbreak t;
  (* B4: capture whether the formula is EMPTY before this batch. Symmetry detection sees
     only [terms]; if prior assertions exist, a symmetry of the batch need not be a
     symmetry of [prior ∧ batch]. Captured before the originals below are recorded. *)
  let no_prior_assertions =
    match t.asserted with
    | [] -> true
    | _ -> false
  in
  if List.exists term_has_reserved terms
  then degrade t "reserved-symbol"
  else (
    (* Record the ORIGINALS for R1 (order-insensitive; [Model_check.check] folds over
       all). The pre-preprocessing terms are exactly what the R1 checker must satisfy. *)
    List.iter (fun term -> t.asserted <- term :: t.asserted) terms;
    (* I8/CONTRACT-POISON (codex H1): the substitution builds terms through the arithmetic
       smart constructors, so composing coefficients can raise [Term.Overflow] (e.g.
       [x = 2y] substituted into [C·x = 0] with [2·C] out of int63) — or an [Unsupported]
       operand. That must degrade to a clean [Unknown], never escape [assert_presolved] as
       a crash. Same discipline as {!internalize_reduced}. *)
    (* Pass A (task #7, gated: flag + cert-OFF): the equalities entailed by top-level
       disjunctions, added as extra top-level unit assertions. They are equisatisfiable
       consequences of [terms], so they are NOT recorded in [t.asserted] (the R1 set stays
       the ORIGINAL assertions); they are internalized alongside the reduced conjuncts. On
       eq_diamond these units chain [x0=…=xn] at the EUF level 0, refuting [x0≠xn] with no
       search. Bypassed entirely when OFF (byte-identical to trunk — no augmentation, so
       the LOW-7 zero-alias early-return in [Presolve.run] is unaffected). *)
    let extra =
      if pass_a_enabled t
      then (
        match Presolve.entailed_equalities t.ctx terms with
        | exception Term.Overflow -> []
        | exception Term.Unsupported _ -> []
        | eqs -> eqs)
      else []
    in
    (* Symmetry breaking (task #25, gated: flag + cert-OFF): full-action lex-leader
       constraints over interchangeable same-sort constants, added as extra top-level
       assertions. They are equisatisfiable (they REMOVE symmetric models, keeping >=1 per
       orbit), so — like Pass A — they are NOT recorded in [t.asserted] (the R1 set stays
       the ORIGINAL assertions; any found model still satisfies them). Bypassed entirely
       when OFF (byte-identical to trunk). Neutral-abort inside [symmetry_break] returns
       [[]]; the Overflow/Unsupported firewall matches Pass A. *)
    let sym_extra =
      (* R2/R3 EMISSION RESTRICTION (codex B1/B2/B4): emit ONLY when the formula being
         solved is EXACTLY this batch — the base frame, no lemmas registered, and no prior
         assertions. Then a symmetry of [terms] is a symmetry of the whole formula, which
         is the entire soundness story:
         - B1: under a pushed frame the lex clauses (guarded by [sym_sel], not the frame
           selector) would survive the [pop] that retracts the assertions making the batch
           symmetric;
         - B2: with lemmas a during-solve instance would extend the formula un-retractably
           ([check_sat] fixes its assumptions once);
         - B4: with prior assertions a symmetry of [terms] need not be one of
           [prior ∧ terms]. The post-emission incremental case is still handled by
           [deactivate_symbreak] at every assertion entry. *)
      let formula_is_exactly_this_batch =
        (match t.frames with
         | [ _ ] -> true
         | _ -> false)
        && (not t.lemmas_registered)
        && no_prior_assertions
        (* Belt (task #63 pre-ON): decouple symmetry breaking from the datatype registry /
           Env well-formedness entirely — never emit on a datatype-using session. B3
           already excludes datatype-sorted candidates, so this only skips the free
           (uninterpreted) constants of a mixed QF_UFDT problem; the measured win is pure
           QF_UF (no datatypes), so this costs nothing while removing any dependence of
           symmetry soundness on DT registry correctness. *)
        && not (uses_datatypes t)
      in
      if symbreak_enabled t && formula_is_exactly_this_batch
      then (
        (* F2: per-session name counter (persists across batches). F3 FINAL: catch ONLY
           the expected fragment exceptions ([Sort_error] from a would-be cross-sort
           candidate, [Overflow]/[Unsupported] from arithmetic rebuild) → "no breaking".
           Everything else — Out_of_memory, Stack_overflow (the DAG rebuild is
           non-tail-recursive), Sys.Break, any unexpected soundness raise — PROPAGATES; it
           must never be swallowed into a silent no-op. *)
        match Presolve.symmetry_break ~counter:t.sym_counter t.cap t.env t.ctx terms with
        | cs -> cs
        | exception (Term.Sort_error _ | Term.Overflow | Term.Unsupported _) -> [])
      else []
    in
    match Presolve.run t.ctx terms with
    | exception Term.Overflow -> degrade t "presolve-overflow"
    | exception Term.Unsupported _ -> degrade t "presolve-unsupported"
    | { Presolve.reduced; defs } ->
      t.elim_defs <- defs;
      (* Equality-over-ITE projection (task #34, gated: flag + cert-OFF): a
         model-preserving local DAG rewrite over the reduced conjuncts —
         [(= (ite c x y) d)] projected into the branches, plus Bool-ITE and local selector
         collapse — turning nec-smt [(= chain_ite literal)] conditions into boolean
         functions of the original atoms before clausification. Eliminates no variable, so
         [t.elim_defs] / model reconstruction and the R1 set (the ORIGINAL [t.asserted])
         are untouched. On the hard budget it neutral-aborts to [reduced] unchanged.
         Builds through [t.ctx]'s smart constructors, so the same Overflow/Unsupported
         firewall as {!internalize_reduced} applies. *)
      let reduced =
        if proj_enabled t
        then (
          match Presolve.simplify_projection t.ctx reduced with
          | exception Term.Overflow ->
            degrade t "presolve-proj-overflow";
            reduced
          | exception Term.Unsupported _ ->
            degrade t "presolve-proj-unsupported";
            reduced
          | simplified -> simplified)
        else reduced
      in
      (* Contextual simplification (task #13, gated: flag + cert-OFF): a model-preserving
         term rewrite over the reduced conjuncts (assume each ITE condition within its own
         branch), collapsing the nested-ITE VCs before clausification. It eliminates no
         variable, so [t.elim_defs] / model reconstruction and the R1 set (the ORIGINAL
         [t.asserted]) are untouched. On the hard budget it neutral-aborts to [reduced]
         unchanged. It builds through [t.ctx]'s smart constructors, so the same
         Overflow/Unsupported firewall as {!internalize_reduced} applies. *)
      let reduced =
        if ctx_simp_enabled t
        then (
          match Presolve.simplify_contextual t.ctx reduced with
          | exception Term.Overflow ->
            degrade t "presolve-ctx-overflow";
            reduced
          | exception Term.Unsupported _ ->
            degrade t "presolve-ctx-unsupported";
            reduced
          | simplified -> simplified)
        else reduced
      in
      List.iter (internalize_reduced t) reduced;
      List.iter (internalize_reduced t) extra;
      (* F1: guard the symmetry-breaking clauses with a fresh activation selector so a
         later incremental assertion can retract them soundly (they are non-monotonic).
         The selector is assumed positive by [check_sat] while [t.sym_sel = Some _]; it
         occurs only negatively in the clauses, so clearing it makes them vacuous.
         CONTRACT: emission is expected at the BASE frame (the batch [assert_presolved]
         path runs before any [push]). The clauses are guarded by [sym_sel] alone, not the
         frame selector, so retraction relies on [deactivate_symbreak] at every assertion
         entry AND at [pop] (see [pop]) — not on frame scoping. *)
      (match sym_extra with
       | [] -> ()
       | _ :: _ ->
         let sel = Sat.new_var t.sat in
         t.sym_sel <- Some sel;
         List.iter (internalize_reduced_at ~sel t) sym_extra))
;;

(* ADR-0012 §1.4 (R2 / codex POINT 6): assert a ground lemma instance guarded by its
   OWNING lemma's frame selector [frame], NOT the current innermost selector — so
   [Session.pop] of the lemma's frame retracts the lemma AND every instance drawn from it
   together. PRIVATE: it takes an [Instance.t] (a re-checked ground term), not a public
   [Term.t], so it cannot be used to bypass the assert gate or to select an arbitrary SAT
   var. The instance flows through the SAME preprocess -> clausify -> register_atom ->
   Combine pipeline as a user assertion (§1.4); it is NOT added to [t.asserted] (the R1
   model self-check set) — an instance is a consequence of a live lemma, and a
   client-reported [Sat] only ever occurs with NO live lemma, hence with no active
   instance. *)
let assert_instance_at_frame t ~frame (inst : Instance.t) =
  (* R2 defensive belt: an instance extends the formula mid-solve. The emission
     restriction already forbids emitting when lemmas are registered (and an instance only
     exists under a registered lemma), so [sym_sel] is always [None] here — but clearing
     it keeps the invariant local rather than relying on that reasoning. *)
  deactivate_symbreak t;
  match Preprocess.run t.pp (Instance.to_term inst) with
  | exception Term.Overflow -> degrade t "overflow"
  | exception Term.Unsupported _ -> degrade t "unsupported"
  | pterm -> assert_bool_at ~sel:frame t pterm
;;

type lemma = Lemma.t

type origin = Lemma.origin =
  | Named of string
  | Anonymous

(* What [assert_lemma]'s [build] returns: the well-sorted Bool [body] over the minted
   qvars plus ground symbols, and the (possibly empty) multi-triggers. A record rather
   than the ADR's object sketch (< body; triggers >) — cleaner OCaml, same content. *)
type lemma_def =
  { body : Term.t
  ; triggers : Term.t list list
  }

(* ADR-0012 §1.3: mint-before-build binder-builder. The session mints the qvar handles
   FIRST (via [Qvar.mint], reserved-namespace, cap-gated in Phase B), hands them to
   [build], and the caller constructs the body/triggers USING those handles — so
   occurrence-binding is by construction, never by the caller re-spelling a reserved name
   (R1 defect (2)). The lemma is recorded in the CURRENT assertion frame (§1.5), so [pop]
   retracts it (and its instances) together. Returns the stored [lemma] handle (the ADR's
   [unit] is widened additively so the tranche-1 manual path — {!instantiate} — can name
   the lemma; a caller may ignore it). *)
let assert_lemma t ~qvars ~build =
  (* F1/R2: a lemma extends the formula (its instances assert during solve). Retract any
     active emission, and mark lemmas registered so a LATER [assert_presolved] refuses to
     emit (codex B2 — instances would break the symmetry mid-solve, un-retractably). *)
  deactivate_symbreak t;
  t.lemmas_registered <- true;
  let id = Manager.fresh_id t.mgr in
  let qv =
    Array.of_list
      (List.mapi
         (fun k (_name, sort) -> Qvar.mint t.cap t.env t.ctx ~lemma_id:id ~index:k sort)
         qvars)
  in
  let { body; triggers } = build qv in
  if not (Sort.equal (body : Term.t).sort Sort.bool)
  then invalid_arg "Session.assert_lemma: lemma body must be Bool-sorted";
  (* codex C1 (lemma path): the body/triggers may reference THIS lemma's qvars but no
     other reserved [.oxsmt.*] symbol — a captured preprocessing witness smuggled into a
     lemma body would let its instance capture the internal aux (wrong verdict). Reject
     foreign reserved symbols; the lemma's own qvars are whitelisted. *)
  let qvar_syms =
    Array.to_list
      (Array.map
         (fun q ->
           match (Qvar.to_term q).Term.node with
           | App (s, _) -> s
           | _ -> assert false (* a qvar is a nullary App by construction *))
         qv)
  in
  let foreign tm = term_has_reserved ~allowed:qvar_syms tm in
  if foreign body || List.exists (List.exists foreign) triggers
  then
    invalid_arg
      "Session.assert_lemma: body/trigger references a reserved (.oxsmt.*) symbol that \
       is not one of this lemma's qvars";
  (* ADR-0012 L3: a trigger pattern must be an UNINTERPRETED application (an [App] with
     arity >= 1) — arithmetic/order/equality-headed triggers are rejected at
     [assert_lemma], not silently ignored by the matcher. Arithmetic lives in the lemma
     BODY (handled by the assert-time pipeline), never as a trigger root; a bare qvar or
     ground constant (nullary [App]) is not a usable trigger either. Rejecting here is
     spec-conformance + fail-loud on a brand-new API, cheaper than letting callers depend
     on accept-and-ignore (codex MED). *)
  let uf_application (p : Term.t) =
    match p.node with
    | App (_, args) -> Iarr.length args > 0
    | _ -> false
  in
  if List.exists (List.exists (fun p -> not (uf_application p))) triggers
  then
    invalid_arg
      "Session.assert_lemma: a trigger must be an uninterpreted application f(...) \
       (arity >= 1); arithmetic/order/equality-headed triggers are not supported \
       (ADR-0012 L3)";
  let lemma =
    { Lemma.qvars = qv
    ; body
    ; triggers
    ; id
    ; frame = current_selector t
    ; origin = Anonymous
    }
  in
  Manager.add_lemma t.mgr lemma;
  lemma
;;

(* TRANCHE-1 SCAFFOLD (ADR-0012 §8 manual-instances path). Seed a ground instance of
   [lemma] at [sigma] (ground terms in the lemma's qvars order); the next {!check_sat}
   loop drains it through the real dedup + frame-scoped assertion pipeline. The tranche-2
   matcher replaces this producer (it will generate substitutions by E-matching), at which
   point this entry point is retired. *)
let instantiate t lemma sigma = Manager.seed_instance t.mgr lemma sigma

let push t =
  (* F1: a new frame's assertions may break a prior emission's symmetry; retract its lex
     clauses. (A later [pop] does not resurrect them — a re-emission would be needed,
     which the batch path does not do; sound, only forgoes the bonus.) *)
  deactivate_symbreak t;
  (* Snapshot the active assertion set BEFORE opening the frame, so the matching [pop]
     restores exactly the pre-frame set (F3: keeps [asserted] = the active set). *)
  t.asserted_saved <- t.asserted :: t.asserted_saved;
  t.frames <- Sat.new_var t.sat :: t.frames
;;

let pop t =
  match t.frames with
  | [ _ ] | [] -> invalid_arg "Session.pop: no matching push"
  | popped :: rest ->
    t.frames <- rest;
    (* F1 defensive: the symmetry-breaking lex clauses are guarded by [sym_sel], NOT by a
       frame selector, so a [pop] would not retract an emission made inside the popped
       frame. The batch-once contract (assert_presolved runs at the base frame, before any
       push) makes that unreachable in the shipped path, but clearing [sym_sel] here makes
       the F1 soundness independent of that contract: after any pop the lex clauses go
       vacuous. *)
    deactivate_symbreak t;
    (* ADR-0012 §1.5: retract the lemmas added in this frame AND every instance drawn from
       them together (dedup entries + pending seeds owned by [popped] are dropped too), by
       disabling the frame's selector. Soundness-load-bearing (a stranded pushed-frame
       instance is the C1 wrong-[unsat]). *)
    Manager.on_pop t.mgr popped;
    (* Restore the assertion set to the matching [push]'s snapshot, dropping the frame's
       assertions in lockstep (asserted_saved has one entry per non-base frame). *)
    (match t.asserted_saved with
     | s :: srest ->
       t.asserted <- s;
       t.asserted_saved <- srest
     | [] -> ())
;;

(* The self-checkable model of the just-decided [Sat]. It has two disjoint parts:
   - the combined theory's nullary-symbol model (Int / uninterpreted-sort constants; see
     {!Cdclt.model_bindings}), present only for a theory query;
   - a [Bool] per propositional variable (the nullary Bool [App]s in {!bool_consts}),
     which the SAT core owns — these NEVER appear in the theory snapshot, so a mixed
     Boolean/theory query must union them in or the §8 evaluator rejects the model as
     omitting a declared Bool constant. Reserved preprocessing witnesses ([.oxsmt.*], e.g.
     an ITE lift) are hash-consed internal symbols that never existed in the user's query;
     they are filtered out so the external model names only user-declared symbols. A name
     can in principle appear in both parts (a Bool constant that is also an argument of an
     applied predicate, hence a theory subterm); the SAT assignment is authoritative for a
     propositional variable, so {!bool_consts} wins the union. [None] (→ [unknown]) when
     no table-free model is reconstructable (any applied uninterpreted symbol is
     constrained). *)
let name_of = function
  | Const (n, _) -> n
  | Fun (n, _) -> n
;;

(* The nullary-variable leaves of [term] (a W1b def value, which is UF-free — so every
   leaf variable is a nullary [App]) as [(name, sort)] pairs, deduplicated by name. Used
   to default any surviving-but-unconstrained variable a def references. Codex M1: an Int
   def value can carry NON-Int leaves — e.g. [(= x (ite b 1 2))] has a Bool [b] in the
   guard, and an [Ite] guard can compare uninterpreted-sort variables — so this must
   collect leaves of EVERY sort, not just Int, or such a variable is left unbound and R1
   spuriously rejects a satisfiable model. *)
let free_var_leaves (term : Term.t) =
  let seen = Hashtbl.create 16 in
  let acc = ref [] in
  let rec go (u : Term.t) =
    match u.node with
    | App (sym, args) when Iarr.length args = 0 ->
      let n = Symbol.name sym in
      if not (Hashtbl.mem seen n)
      then (
        Hashtbl.add seen n ();
        acc := (n, u.sort) :: !acc)
    | App (_, args) -> Iarr.iter go args
    | Arith lin -> Iarr.iter (fun (tm, _c) -> go tm) lin.coeffs
    | Le a | Not a -> go a
    | Eq (a, b) ->
      go a;
      go b
    | And xs | Or xs -> Iarr.iter go xs
    | Ite (c, a, b) ->
      go c;
      go a;
      go b
    | Bool_const _ | Int_const _ -> ()
  in
  go term;
  !acc
;;

(* Raised by {!default_value} when a free variable of a datatype sort has no scalar
   default. Caught in {!build_model}, which degrades to no-model (-> [Unknown]). *)
exception No_default_value

(* A canonical default value for an unconstrained variable of [sort], used when a def
   references a surviving variable the theory never valued (any value satisfies an
   otherwise-free variable; uninterpreted sorts are inhabited so element 0 exists).

   EXHAUSTIVE match, not an [if]/[else] chain, on purpose (codex): the old fallthrough
   [else VUninterp 0] silently handed a DATATYPE-sorted free variable an uninterpreted
   witness [VUninterp 0] — a model value the datatype sort cannot take (its values are
   constructor trees). Under presolve elimination a datatype const can be eliminated (so
   it never reaches the datatype theory) yet resurface here during model reconstruction;
   the fabricated value would then be evaluated by R1 against the original assertions and
   could spuriously satisfy them (silent wrong-[Sat] class). There is no sound scalar
   default for a datatype, so fail closed: raise, and let {!build_model} drop the model to
   [Unknown]. ([VInt] carries a core-bignum [Bigint.t], W2.) *)
let default_value (sort : Sort.t) : model_value =
  match sort with
  | Sort.Bool -> VBool false
  | Sort.Int _ -> VInt Bigint.zero
  | Sort.Uninterpreted _ -> VUninterp 0
  | Sort.Datatype _ | Sort.Array _ | Sort.BitVec _ -> raise No_default_value
;;

(* W1b model reconstruction (logs/w1b-design.md, constraint 4). Splice the eliminated
   variables back into the reduced model: for each [def] (processed in REVERSE elimination
   order, matching the design note — harmless here since each [def.value] is already
   resolved over surviving variables only), evaluate [def.value] under the model built so
   far and bind [def.name] to it. Both re-derivation and R1 share [Model_check]'s
   evaluator (identical overflow guards). A variable that [def.value] references but that
   the theory left UNCONSTRAINED (its only constraints were dropped defs) is bound to a
   canonical sort-default FIRST (Bool [false] / Int [0] / uninterpreted element [0]) and
   that binding is kept, so R1 evaluates the original assertions under the same value (any
   value satisfies an otherwise-free variable). If [eval_value] returns [None] the
   variable is left unbound and R1 will fail closed to [unknown] — never a wrong value.
   No-op when nothing was eliminated. *)
let splice_elim_defs t (sort_cards, bindings) =
  match t.elim_defs with
  | [] -> sort_cards, bindings
  | _ :: _ ->
    (* Every re-derivation evaluates [d.value] under the model built so far. Rather than
       rebuild the evaluator's tables from the growing binding list per def (O(defs x
       bindings) — the SMPT quadratic), build them ONCE and mutate them in lockstep with
       [acc]: each Const added to [acc] is also written into [tbls], so [eval_in] reads
       the same model [(sort_cards, !acc)] would. Only Const bindings are ever added here
       (free-variable defaults and re-derived def values are both nullary), so [add_const]
       covers every mutation; [bound] still tracks membership for the default guard. Names
       across the originals and the added bindings are unique — an eliminated def
       references only SURVIVING variables (w1b design note), so a def name never
       coincides with a free-variable default, and the default guard makes each default
       at-most-once — so the last-writer-wins table is unaffected by the incremental order
       (byte-identical model to the former per-def rebuild). *)
    let bound = Hashtbl.create 64 in
    List.iter (fun b -> Hashtbl.replace bound (name_of b) ()) bindings;
    let tbls = Model_check.tables_of_bindings bindings in
    let acc = ref bindings in
    let add_const name v =
      acc := Const (name, v) :: !acc;
      Hashtbl.replace bound name ();
      Model_check.add_const tbls name v
    in
    List.iter
      (fun (d : Presolve.def) ->
        List.iter
          (fun (name, sort) ->
            if not (Hashtbl.mem bound name) then add_const name (default_value sort))
          (free_var_leaves d.Presolve.value);
        match Model_check.eval_in tbls d.Presolve.value with
        | Some v -> add_const d.Presolve.name v
        | None -> ())
      (List.rev t.elim_defs);
    sort_cards, !acc
;;

let build_model t =
  let keep name = not (Preprocess.is_reserved_name name) in
  let by_name a b = String.compare (name_of a) (name_of b) in
  let bool_bindings =
    List.filter_map
      (fun (name, sv) ->
        if keep name then Some (Const (name, VBool (Sat.value t.sat sv))) else None)
      t.bool_consts
  in
  (* Membership set for the bool-const dedup: O(1) lookups instead of the former
     [List.mem] over [bool_names] (which made [assemble] O(theory_bindings x bool_consts)
     — the same SMPT quadratic {!splice_elim_defs} above avoids). *)
  let bool_name_set : (string, unit) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun b -> Hashtbl.replace bool_name_set (name_of b) ()) bool_bindings;
  let assemble sort_cards theory_bindings =
    let theory_bindings =
      List.filter
        (fun b ->
          let n = name_of b in
          keep n && not (Hashtbl.mem bool_name_set n))
        theory_bindings
    in
    (* No sort here: [assemble] always flows through [finalize], which sorts by [by_name];
       binding names are unique so [by_name] is tie-free and [splice_elim_defs]'s output
       set is order-independent, so the single final sort is byte-identical to the former
       assemble+finalize double sort. *)
    sort_cards, theory_bindings @ bool_bindings
  in
  (* W1b: splice the eliminated variables (and any defaulted free variable) into the
     assembled model, then re-sort so the external model stays name-sorted. A no-op when
     nothing was eliminated (byte-identical to the pre-W1b model). *)
  let finalize m =
    match splice_elim_defs t m with
    | sort_cards, bindings -> Some (sort_cards, List.sort by_name bindings)
    | exception No_default_value ->
      (* A presolve-eliminated datatype variable resurfaced during reconstruction with no
         sound scalar default (see {!default_value}). Degrade to no-model -> [Unknown]
         rather than emit a fabricated datatype value. *)
      None
  in
  if t.has_theory
  then (
    match Cdclt.model t.cdclt with
    | None -> None
    | Some (sort_cards, theory_bindings) -> finalize (assemble sort_cards theory_bindings)
    | exception Rational.Overflow ->
      (* core-bignum R1 output-boundary: a [Big] LIA model value is integral but exceeds
         int63, so it cannot be projected to the native-int [Model.Int] sink without
         truncating. BELT-AND-SUSPENDERS (dual-review F1): in the CURRENT pipeline this
         arm is UNREACHABLE — LIA model/branch projection is EAGER inside [Sat.solve] (the
         [Cdclt]/[Combine] model snapshot happens during the theory-driving solve), so a
         [Big] model value's [Rational.num] overflow is raised and caught by the
         CONTRACT-POISON firewall wrapping [check_sat]'s solve, degrading to [Unknown]
         BEFORE [build_model] ever runs. The actual R1 mechanism is that firewall, not
         this catch. This catch is retained as defense-in-depth: it is the
         correctly-placed guard should model extraction ever move OUTSIDE [Sat.solve]
         (then [build_model] would run unprotected). Either way: degrade to no-model ->
         [Unknown], never a truncated model, no [Model.t] unfreeze. *)
      None)
  else finalize (assemble [] [])
;;

(* One ground CDCL(T) decision under the CONTRACT-POISON firewall — the RAW verdict only,
   no model build. This is what drives the outer lemma loop (§1.4): a [Sat] means "the
   ground core is satisfiable, keep instantiating if a lemma is live", NOT a client
   answer. Model reconstruction + the R1 self-check happen only at the committing [Sat]
   ({!commit_sat}), so a live-lemma round is never blocked by a model the reconstructor
   cannot yet build (e.g. a UFLIA function table) — that would be a spurious completeness
   loss. The firewall wraps ONLY [Sat.solve] (the untrusted theory callbacks);
   [commit_sat] runs outside it. *)
let raw_solve t assumptions =
  match Sat.solve ~assumptions t.sat with
  | Sat.Unsat -> Unsat (* theory conflicts only strengthen unsat; still sound *)
  | Sat.Sat -> Sat (* the ground core is satisfiable; model build is deferred to commit *)
  | exception Cdclt.Split_budget_exceeded ->
    (* Not a fault: the deterministic split cap fired. Distinct stat, sticky. *)
    degrade t "split-budget";
    t.budget_exhausted <- true;
    t.unknown_reason <- "split-budget";
    Unknown
  | exception Budget.Exceeded ->
    (* Board #60: the deterministic effort cap fired. NOT sticky, does NOT set [degraded];
       the same query is re-runnable at a larger [max_effort]. A distinct BUDGET tag. *)
    t.effort_exhausted <- true;
    t.unknown_reason <- "effort-budget";
    Unknown
  | exception Combine.Incomplete msg ->
    (* DELIBERATE completeness degrade (ADR-0010 §3.6 case (ii)); a NAMED arm, not the
       catch-all. register_atom can raise it mid-solve. Sticky → Unknown. *)
    let tag = "combine-incomplete-solve:" ^ san_token msg in
    degrade t tag;
    t.unknown_reason <- tag;
    Unknown
  | exception ((Out_of_memory | Stack_overflow) as e) ->
    (* Resource-exhaustion / async control-flow: process state untrustworthy — re-raise. *)
    raise e
  | exception e ->
    (* CONTRACT-POISON firewall (I8), catch-all over the untrusted theory callbacks driven
       by [Sat.solve]: any escaping poison / unforeseen exception bricks this query to
       [Unknown]. Sticky. The census tags the sub-cause with the exception name so the
       poison bucket is not opaque (diagnostic string only; verdict unchanged). *)
    let tag = "poison-solve:" ^ exn_tag e in
    degrade t tag;
    t.unknown_reason <- tag;
    Unknown
;;

(* Commit a client-reported [Sat] (ADR-UF-models §3 / THE SOUNDNESS RULE M4): report [Sat]
   only when a self-checkable model is reconstructable AND it passes the R1 in-process
   checker — function tables AND table-free (const / Bool / LIA) models alike, no
   [has_table] short-circuit ("no [sat] without the checker"). A model it cannot soundly
   build is [None] -> [Unknown]; a checker rejection fail-closes to [Unknown]. Runs
   OUTSIDE the [raw_solve] firewall, so a bug here surfaces as a crash, not a silent
   [Unknown]. *)
(* TEST-ONLY fault-injection seam (F1 obligation, logs/dt-models-review-fable.md): the DT
   commit consults its model self-checker through this indirection so a test can
   substitute a stub and PIN that a DT [Sat] is GATED on the checker verdict. A regression
   that bypassed the checker (e.g. rewriting the arm to [| Some _ -> Sat]) would ignore an
   injected reject-all stub and report [Sat] where the test demands [Unknown] — the
   missing coverage the reviewer flagged. [None] in every production path => the real
   {!Dt_model_check.check}, so soundness is NEVER routed through a stub outside a test.
   Set only via {!For_test.set_dt_checker}. *)
let dt_checker_override
  : (Oxsmt_core.Datatype_defs.t
     -> (Term.t * Oxsmt_dt.Dt.ctor_tree) list
     -> Term.t list
     -> bool)
      option
      ref
  =
  ref None
;;

(* TEST-ONLY fault-injection seam for the arrays commit, mirroring {!dt_checker_override}:
   pins that an array [Sat] is GATED on the checker verdict. [None] in production => the
   real {!Array_model_check.check}. Set only via {!For_test.set_array_checker}. *)
let array_checker_override
  : (Oxsmt_core.Array_defs.t
     -> (Term.t * Oxsmt_arr.Arr.value) list
     -> Term.t list
     -> bool)
      option
      ref
  =
  ref None
;;

let commit_sat t =
  (* ARRAYS (QF_AX model construction, task #14): the standalone arrays theory is
     installed, so soundness rests on the array self-check, not the UF [Model_check]
     (which treats [select]/[store] as opaque functions with no array semantics). Validate
     the array model extracted at Final ([Cdclt.array_model]) against the ORIGINAL
     assertions with the independent [Array_model_check] (which computes
     [select]/[store]/extensional-equality itself); report [Sat] only if it passes, else
     [Unknown]. The scalar [model] type cannot carry array values, so [get_model] stays
     [None] for an array [Sat] (surfacing the map model is a follow-up); the verdict flips
     unknown -> checked-[Sat]. *)
  if t.has_arrays
  then (
    match Cdclt.array_model t.cdclt with
    | Some model ->
      let check =
        match !array_checker_override with
        | Some f -> f
        | None -> Array_model_check.check
      in
      if check !(t.array_registry) model t.asserted
      then (
        t.last_model <- None;
        Sat)
      else (
        t.unknown_reason <- "array-model-check-failed";
        Unknown)
    | None ->
      t.unknown_reason <- "array-no-model";
      Unknown)
  else if not (Oxsmt_core.Datatype_defs.is_empty !(t.registry))
  then (
    (* DATATYPES (GOALS Datatypes model construction): the standalone DT theory is
       installed, so soundness rests on the DT constructor-tree self-check, not the UF
       [Model_check]/[Cdclt.model] reconstruction (which fails closed on a
       [Sort.Datatype]). Validate the tree model extracted at Final ([Cdclt.dt_model])
       against the ORIGINAL assertions with the independent [Dt_model_check]; report [Sat]
       only if it passes. The scalar [model] binding-list type cannot carry constructor
       trees, so [get_model] stays [None] for a DT [Sat] in v1 (surfacing the tree model
       to the CLI / external eval is a follow-up); the verdict itself flips unknown ->
       checked-[Sat]. *)
    match Cdclt.dt_model t.cdclt with
    | Some model ->
      let check =
        match !dt_checker_override with
        | Some f -> f
        | None -> Dt_model_check.check
      in
      if check !(t.registry) model t.asserted
      then (
        t.last_model <- None;
        Sat)
      else (
        t.unknown_reason <- "dt-model-check-failed";
        Unknown)
    | None ->
      t.unknown_reason <- "dt-no-model";
      Unknown)
  else (
    match build_model t with
    | Some m ->
      if Model_check.check m t.asserted
      then (
        t.last_model <- Some m;
        Sat)
      else (
        t.unknown_reason <- "r1-model-check-failed";
        Unknown)
    | None ->
      t.unknown_reason <- "no-model";
      Unknown)
;;

let check_sat t =
  t.last_verdict <- Unknown;
  t.last_model <- None;
  t.budget_exhausted <- false;
  t.effort_exhausted <- false;
  (* census (task #78): fresh per-check reason slot; set at each giveup site below. *)
  t.unknown_reason <- "";
  (* R3 minor: capture the activation selector this solve will assume, so a post-solve
     [failed_assumptions] strips it from the core even after a later assertion clears
     [sym_sel]. *)
  t.sym_sel_in_core <- t.sym_sel;
  (* OXSMT_BASE_L0: add the permanent [base] forcing unit on the first solve (not at
     [create]), so a certificate trace installed on the pristine session records it as a
     genuine Input and the checker derives [base] by BCP over inputs. Added exactly once,
     before any solve, with the default [Query] origin (routed to an [Input] intro). *)
  if t.base_at_level0 && not t.base_unit_emitted
  then (
    Sat.add_clause t.sat [ Sat.pos t.base_var ];
    t.base_unit_emitted <- true);
  if t.degraded
  then (
    (* Degradation recorded at assert time (I8); surface its first cause. *)
    t.unknown_reason
    <- (if String.length t.degraded_reason = 0 then "degraded" else t.degraded_reason);
    Unknown)
  else if Bv_dispatch.is_pure_bv t.asserted && not (Manager.has_live_lemma t.mgr)
  then (
    (* Pure QF_BV with NO live quantified lemma: resolve by eager bit-blasting BEFORE the
       combinator (which fail-closed degrades any live bit-vector term to unknown,
       combine.ml). Bv_solve re-checks every sat model with the independent evaluator, so
       a Sat here is already self-certified — we surface its bindings directly rather than
       through the BV-unaware R1 combinator checker. Unsat is the pure-propositional
       SAT-core refutation; Unknown is the fail-closed door on any construct the blaster
       does not encode.

       The [not (has_live_lemma)] guard is a SOUNDNESS gate (F1): [is_pure_bv] inspects
       only [t.asserted] (the ground set), so a live [forall] lemma (in [t.mgr]) is
       invisible to it — bit-blasting the ground set alone would ignore the quantifier and
       could report [Sat] for a model the lemma forbids (a wrong-[Sat]). A lemma'd session
       therefore takes the combinator path below, where THE SOUNDNESS RULE degrades a
       lemma-live ground [Sat] to [Unknown] (never a model that ignores a quantifier). *)
    match Bv_dispatch.solve t.ctx (Internal_minter.mint (parse_minter t)) t.asserted with
    | Bv_dispatch.Unsat ->
      t.last_verdict <- Unsat;
      Unsat
    | Bv_dispatch.Unknown msg ->
      t.unknown_reason <- "bv-blast-unknown:" ^ san_token msg;
      Unknown
    | Bv_dispatch.Sat { bv_vars; bool_vars } ->
      t.last_verdict <- Sat;
      t.last_model
      <- Some
           ( []
           , List.map (fun (n, v, _w) -> Const (n, VInt v)) bv_vars
             @ List.map (fun (n, b) -> Const (n, VBool b)) bool_vars );
      Sat)
  else (
    Cdclt.begin_check t.cdclt;
    Manager.begin_check t.mgr (* fresh generation budget for this check_sat (§1.4) *);
    (* F1: while a symmetry-breaking emission is active, assume its activation selector
       POSITIVE so the (selector-guarded) lex clauses constrain this solve. Once a later
       assertion cleared [sym_sel] to [None], the selector is no longer assumed and —
       since it occurs only negatively — the clauses are trivially satisfiable
       (retracted). Under assumptions this stays sound: an [Unsat] means unsat given the
       frame + activation assumptions, and the activation clauses are equisatisfiable, so
       the query is unsat. *)
    let assumptions =
      (* When [base_at_level0], [base] (the outermost / last frame) is forced true by a
         permanent unit, so it is NOT assumed here; pushed frame selectors are kept. *)
      let frame_asms = List.map Sat.pos (assumed_frames t) in
      match t.sym_sel with
      | Some sel -> Sat.pos sel :: frame_asms
      | None -> frame_asms
    in
    (* THE outer instantiation loop (ADR-0012 §1.4). There is exactly ONE [Sat] exit to
       the client — the [not (has_live_lemma)] line — so THE SOUNDNESS RULE (§2) is an
       unconditional wrapper over EVERY ground [Sat] (H1+H2), never a per-arm edit: while
       a lemma is live a ground [Sat] either refutes on a later round (→ [Unsat]) or
       saturates / exhausts the generation budget (→ [Unknown]). Instances are asserted
       only BETWEEN complete ground checks, at decision level 0 (ADR-0010 invariant (i),
       §1.4). *)
    let rec loop () =
      match raw_solve t assumptions with
      | Unsat -> Unsat (* SOUND: instances are valid consequences of their lemmas *)
      | Unknown -> Unknown (* poison / Incomplete / budget, sticky-or-not per the arm *)
      | Sat ->
        if not (Manager.has_live_lemma t.mgr)
        then
          commit_sat t (* the ONLY [Sat] exit to the client goes through the R1 checker *)
        else (
          (* Tranche 2: the matcher reads the live e-graph through a read-only view
             rebuilt each round (the e-graph grows as instances are asserted).
             Non-registering (R6): matching never perturbs the congruence closure. *)
          let insts = Manager.round t.mgr (Cdclt.egraph_view t.cdclt) in
          if Manager.budget_exhausted t.mgr
          then (
            t.unknown_reason <- "lemma-gen-budget";
            Unknown (* generation budget spent with a live lemma (§3) *))
          else (
            match insts with
            | [] ->
              t.unknown_reason <- "lemma-saturated";
              Unknown (* saturated but a quantifier is live: THE SOUNDNESS RULE (§2) *)
            | _ :: _ ->
              List.iter
                (fun (frame, inst) -> assert_instance_at_frame t ~frame inst)
                insts;
              (* An instance that overflowed / was rejected during assertion degraded the
                 session (I8); stop rather than loop on a bricked state. *)
              if t.degraded
              then (
                t.unknown_reason
                <- (if String.length t.degraded_reason = 0
                    then "lemma-instance-degraded"
                    else t.degraded_reason);
                Unknown)
              else loop ()))
    in
    let v = loop () in
    t.last_splits <- Cdclt.splits_used t.cdclt;
    t.last_effort <- Cdclt.effort_used t.cdclt;
    t.last_verdict <- v;
    v)
;;

let get_model t =
  match t.last_verdict with
  | Unsat | Unknown -> None
  | Sat -> t.last_model
;;

(* census (task #78): the tag identifying WHY the most recent [check_sat] returned
   [Unknown] (empty when the verdict was not [Unknown]). Diagnostic introspection only —
   never consulted by the solver, so it cannot affect a verdict. Surfaced unconditionally
   by the dev CLI on stderr to bucket structural unknowns by cause. *)
let last_unknown_reason t = t.unknown_reason
let eliminated_vars t = List.map (fun (d : Presolve.def) -> d.Presolve.name) t.elim_defs

(* ADR-0013 certificate-emission hooks (additive; the trace is a compile-out-able side
   channel that never feeds back into search). [install_cert_trace] must run on a PRISTINE
   session — before the first {!assert_term} — because the recorder relies on observing
   every input from the start (the {!Sat.set_trace} lifecycle contract).
   [cert_assumptions] is the active selector-assumption set the terminal E3 step is
   conditioned on (the certificate's selector strip is checked by seeding these true);
   [failed_assumptions] is the failed-selector core of the most recent [Unsat]. *)
let install_cert_trace t tr =
  (* Set-once / pristine hardening (task #7 rider, defense-in-depth): a cert trace must be
     installed on a PRISTINE session (before any assert) and only once. Enforcing it makes
     the cert-OFF Pass-A gate ([cert_active] set here, read by [assert_presolved]'s
     [pass_a_enabled]) robust to a caller that would otherwise install a trace AFTER Pass
     A already fired — the interleave that could launder a derived unit into the cert. The
     interleave was verified unreachable in shipped callers; this fails it closed anyway.
     Uninstall ([None]) is always allowed. *)
  (match tr with
   | Some _ ->
     if t.cert_active
     then
       invalid_arg "Session.install_cert_trace: a trace is already installed (set-once)";
     if t.asserted <> []
     then
       invalid_arg
         "Session.install_cert_trace: must be installed on a pristine session, before \
          any assert";
     (* OXSMT_BASE_L0 rider: the [base] forcing-unit is emitted lazily on the first
        [check_sat] (see [base_unit_emitted]). A trace installed AFTER that first solve
        would not record the unit as an Input, so a later refutation over it replays as a
        (fail-safe) INVALID cert. Reject fail-loud. Inert when the flag is off:
        [base_unit_emitted] is only ever set under [base_at_level0]. *)
     if t.base_unit_emitted
     then
       invalid_arg
         "Session.install_cert_trace: must be installed before the first check_sat (the \
          OXSMT_BASE_L0 base-forcing unit is emitted there and would otherwise be \
          untraced)"
   | None -> ());
  (* Gate Pass A OFF while a cert trace is live (task #7 cert-OFF ruling): a derived
     entailed-equality unit must not enter the cert as a trusted [Input]. *)
  t.cert_active <- Option.is_some tr;
  Sat.set_trace t.sat tr
;;

let cert_assumptions t = List.map Sat.pos (assumed_frames t)

(* The failed-selector core, with the internal symmetry-breaking activation selector
   filtered out (Rider 1): [sym_sel] is assumed positive during a solve, so it can appear
   in the SAT core, but it is a private aux var and must never surface to a caller's
   assumption core. *)
let failed_assumptions t =
  let failed = Sat.failed_assumptions t.sat in
  match t.sym_sel_in_core with
  | None -> failed
  | Some sel -> List.filter (fun lit -> Sat.var_of_lit lit <> sel) failed
;;

(* Test-only introspection (task #25): is a symmetry-breaking emission currently active
   (its activation selector still assumed)? Lets [symbreak_test] assert the R2 emission
   restriction directly (no emission under a frame / with lemmas registered). *)
let symbreak_active_for_test t = Option.is_some t.sym_sel
let stats t = Sat.stats t.sat
let splits t = t.last_splits
let budget_exhausted t = t.budget_exhausted
let effort t = t.last_effort
let effort_exhausted t = t.effort_exhausted

type lemma_stats = Manager.stats =
  { live_lemmas : int
  ; instances : int
  ; rounds : int
  ; seeds : int
  }

(* ADR-0012 §O4: lemma-tier instantiation stats, distinct from {!splits}. *)
let lemma_stats t = Manager.stats t.mgr

type instantiation = Manager.instantiation =
  { lemma_id : int
  ; subst : Oxsmt_core.Term.t array
  ; instance : Oxsmt_core.Term.t
  }

let lemma_instantiations t = Manager.instantiations t.mgr

module For_test = struct
  let default_value = default_value
  let set_dt_checker f = dt_checker_override := f
  let set_array_checker f = array_checker_override := f
end
