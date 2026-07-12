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
  | VInt of int
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
  ; ctx : Context.t
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
    (* selector stack, innermost first; base always present *)
  ; mutable has_theory : bool
    (* any theory atom (Le / non-Bool Eq / applied predicate) has been asserted: the
         verdict's model comes from the theory, and a Sat is theory-validated *)
  ; mutable degraded : bool
    (* Overflow/Unsupported/poison/budget seen: verdict must be Unknown (I8,
         CONTRACT-POISON) *)
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
    (* the most recent check_sat hit the effort budget (BUDGET tag). Per-check, poison-free:
     distinct from [degraded]/[budget_exhausted], NOT sticky. *)
  }

let create ?(split_budget = default_split_budget) ?max_effort () =
  let env = Env.create () in
  let ctx = Context.create env in
  let sat = Sat.create () in
  (* One shared effort budget for the session (board #60). [max_effort = None] is
     unbounded — it still COUNTS (for instrumentation) but never cuts off, so the default
     / interactive / [make test] path is byte-identical (the count is never printed). *)
  let budget = Budget.create ?max:max_effort () in
  (* Install the theory on the pristine core BEFORE any clause (pristine-attach). *)
  let cdclt = Cdclt.create ctx env sat ~split_budget ~budget in
  let base = Sat.new_var sat in
  { env
  ; ctx
  ; pp = Preprocess.create env ctx
  ; sat
  ; cdclt
  ; mgr = Manager.create ctx env
  ; prop_to_var = Term.Table.create 256
  ; bool_consts = []
  ; frames = [ base ]
  ; has_theory = false
  ; degraded = false
  ; last_verdict = Unknown
  ; last_model = None
  ; asserted = []
  ; asserted_saved = []
  ; last_splits = 0
  ; budget_exhausted = false
  ; last_effort = 0
  ; effort_exhausted = false
  }
;;

let env t = t.env
let context t = t.ctx

(* Declarations reject the reserved fresh-symbol namespace (board #48), so a user symbol
   can never collide with one preprocessing invents. *)
let guard_name name =
  if Preprocess.is_reserved_name name
  then
    invalid_arg
      (Printf.sprintf "Session: cannot declare reserved internal symbol %s" name)
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

(* Map a clausified formula's local variable to a persistent SAT variable. Theory atoms go
   through {!Cdclt} (one SAT var 1:1 with a theory atom, registered with the combined
   theory); a propositional variable (nullary Bool [App]) shares one SAT var per distinct
   term; auxiliary Tseitin variables are fresh per formula (kept in [local]). *)
let assert_clausified ?sel t cnf =
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
      else (
        match Term.Table.find_opt t.prop_to_var atom with
        | Some sv -> sv
        | None ->
          let sv = Sat.new_var t.sat in
          Term.Table.add t.prop_to_var atom sv;
          (match atom.node with
           | App (sym, args) when Iarr.length args = 0 && Sort.equal atom.sort Sort.bool
             -> t.bool_consts <- (Symbol.name sym, sv) :: t.bool_consts
           | _ -> ());
          sv))
    else (
      match local.(v) with
      | Some sv -> sv
      | None ->
        let sv = Sat.new_var t.sat in
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
    cnf
;;

(* Preprocess -> clausify -> register a Bool term into the frame guarded by [sel]
   (default: the current innermost frame). Shared by [assert_term] and
   [assert_instance_at_frame]; the exception handling is the I8/CONTRACT-POISON
   assert-time discipline. *)
let assert_bool_at ?sel t pterm =
  match Cnf.clausify pterm with
  | exception _ -> t.degraded <- true
  | cnf ->
    (* Atom registration walks the theory engines; a rejected / out-of-fragment atom or an
       overflow escaping here degrades the whole session to unknown (I8). The
       internalization combinator raises [Combine.Incomplete] from [register_atom] (e.g. a
       structured Bool compound under a UF argument, ADR-0010 §3.6 case (ii)) — a
       DELIBERATE completeness degrade, distinct from a [Combination_unsound] fault, and
       it surfaces HERE at assert-time registration, so it must be caught on this ingress
       path too. *)
    (try assert_clausified ?sel t cnf with
     | Combine.Incomplete _ -> t.degraded <- true
     | Term.Overflow
     | Term.Unsupported _
     | Rational.Overflow
     | Lia.Poisoned
     | Lia.Unsupported _
     | Invalid_argument _ -> t.degraded <- true)
;;

let assert_term t term =
  (* ADR-0012 §1.1 (R1 POINT 4): the load-bearing assert-side gate. A user term carrying a
     coerced / interned [.oxsmt.qvar.*] placeholder degrades to a clean [Unknown] via the
     I8 Unsupported discipline (NOT a raw [Failure]) — never registered, never in a model. *)
  if Qvar.term_contains_qvar term
  then t.degraded <- true
  else (
    t.asserted <- term :: t.asserted;
    match Preprocess.run t.pp term with
    | exception Term.Overflow -> t.degraded <- true
    | exception Term.Unsupported _ -> t.degraded <- true
    | pterm -> assert_bool_at t pterm)
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
  match Preprocess.run t.pp (Instance.to_term inst) with
  | exception Term.Overflow -> t.degraded <- true
  | exception Term.Unsupported _ -> t.degraded <- true
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
  let id = Manager.fresh_id t.mgr in
  let qv =
    Array.of_list
      (List.mapi
         (fun k (_name, sort) -> Qvar.mint t.env t.ctx ~lemma_id:id ~index:k sort)
         qvars)
  in
  let { body; triggers } = build qv in
  if not (Sort.equal (body : Term.t).sort Sort.bool)
  then invalid_arg "Session.assert_lemma: lemma body must be Bool-sorted";
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

let build_model t =
  let keep name = not (Preprocess.is_reserved_name name) in
  let by_name a b = String.compare (name_of a) (name_of b) in
  let bool_bindings =
    List.filter_map
      (fun (name, sv) ->
         if keep name then Some (Const (name, VBool (Sat.value t.sat sv))) else None)
      t.bool_consts
  in
  let bool_names = List.map name_of bool_bindings in
  let assemble sort_cards theory_bindings =
    let theory_bindings =
      List.filter
        (fun b -> keep (name_of b) && not (List.mem (name_of b) bool_names))
        theory_bindings
    in
    sort_cards, List.sort by_name (theory_bindings @ bool_bindings)
  in
  if t.has_theory
  then (
    match Cdclt.model t.cdclt with
    | None -> None
    | Some (sort_cards, theory_bindings) -> Some (assemble sort_cards theory_bindings)
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
  else Some (assemble [] [])
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
    t.degraded <- true;
    t.budget_exhausted <- true;
    Unknown
  | exception Budget.Exceeded ->
    (* Board #60: the deterministic effort cap fired. NOT sticky, does NOT set [degraded];
       the same query is re-runnable at a larger [max_effort]. A distinct BUDGET tag. *)
    t.effort_exhausted <- true;
    Unknown
  | exception Combine.Incomplete _ ->
    (* DELIBERATE completeness degrade (ADR-0010 §3.6 case (ii)); a NAMED arm, not the
       catch-all. register_atom can raise it mid-solve. Sticky → Unknown. *)
    t.degraded <- true;
    Unknown
  | exception ((Out_of_memory | Stack_overflow) as e) ->
    (* Resource-exhaustion / async control-flow: process state untrustworthy — re-raise. *)
    raise e
  | exception _ ->
    (* CONTRACT-POISON firewall (I8), catch-all over the untrusted theory callbacks driven
       by [Sat.solve]: any escaping poison / unforeseen exception bricks this query to
       [Unknown]. Sticky. *)
    t.degraded <- true;
    Unknown
;;

(* Commit a client-reported [Sat] (ADR-UF-models §3 / THE SOUNDNESS RULE M4): report [Sat]
   only when a self-checkable model is reconstructable AND it passes the R1 in-process
   checker — function tables AND table-free (const / Bool / LIA) models alike, no
   [has_table] short-circuit ("no [sat] without the checker"). A model it cannot soundly
   build is [None] -> [Unknown]; a checker rejection fail-closes to [Unknown]. Runs
   OUTSIDE the [raw_solve] firewall, so a bug here surfaces as a crash, not a silent
   [Unknown]. *)
let commit_sat t =
  match build_model t with
  | Some m ->
    if Model_check.check m t.asserted
    then (
      t.last_model <- Some m;
      Sat)
    else Unknown
  | None -> Unknown
;;

let check_sat t =
  t.last_verdict <- Unknown;
  t.last_model <- None;
  t.budget_exhausted <- false;
  t.effort_exhausted <- false;
  if t.degraded
  then Unknown
  else (
    Cdclt.begin_check t.cdclt;
    Manager.begin_check t.mgr (* fresh generation budget for this check_sat (§1.4) *);
    let assumptions = List.map Sat.pos t.frames in
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
          let insts = Manager.round t.mgr in
          if Manager.budget_exhausted t.mgr
          then Unknown (* generation budget spent with a live lemma (§3) *)
          else (
            match insts with
            | [] ->
              Unknown (* saturated but a quantifier is live: THE SOUNDNESS RULE (§2) *)
            | _ :: _ ->
              List.iter
                (fun (frame, inst) -> assert_instance_at_frame t ~frame inst)
                insts;
              (* An instance that overflowed / was rejected during assertion degraded the
                 session (I8); stop rather than loop on a bricked state. *)
              if t.degraded then Unknown else loop ()))
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

let stats t = Sat.stats t.sat
let splits t = t.last_splits
let budget_exhausted t = t.budget_exhausted
let effort t = t.last_effort
let effort_exhausted t = t.effort_exhausted

type lemma_stats = Manager.stats =
  { live_lemmas : int
  ; instances : int
  ; rounds : int
  }

(* ADR-0012 §O4: lemma-tier instantiation stats, distinct from {!splits}. *)
let lemma_stats t = Manager.stats t.mgr
