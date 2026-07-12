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
let assert_clausified t cnf =
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
  let sel = current_selector t in
  Cnf.iter_clauses
    (fun clause ->
      (* frame activation: clause holds only when the frame selector is assumed true *)
      Sat.add_clause t.sat (Sat.neg sel :: List.map lit_of clause))
    cnf
;;

let assert_term t term =
  t.asserted <- term :: t.asserted;
  match Preprocess.run t.pp term with
  | exception Term.Overflow -> t.degraded <- true
  | exception Term.Unsupported _ -> t.degraded <- true
  | pterm ->
    (match Cnf.clausify pterm with
     | exception _ -> t.degraded <- true
     | cnf ->
       (* Atom registration walks the theory engines; a rejected / out-of-fragment atom or
          an overflow escaping here degrades the whole session to unknown (I8). The
          internalization combinator raises [Combine.Incomplete] from [register_atom]
          (e.g. a structured Bool compound under a UF argument, ADR-0010 §3.6 case (ii)) —
          a DELIBERATE completeness degrade, distinct from a [Combination_unsound] fault,
          and it surfaces HERE at assert-time registration, so it must be caught on this
          ingress path too. *)
       (try assert_clausified t cnf with
        | Combine.Incomplete _ -> t.degraded <- true
        | Term.Overflow
        | Term.Unsupported _
        | Rational.Overflow
        | Lia.Poisoned
        | Lia.Unsupported _
        | Invalid_argument _ -> t.degraded <- true))
;;

let push t =
  (* Snapshot the active assertion set BEFORE opening the frame, so the matching [pop]
     restores exactly the pre-frame set (F3: keeps [asserted] = the active set). *)
  t.asserted_saved <- t.asserted :: t.asserted_saved;
  t.frames <- Sat.new_var t.sat :: t.frames
;;

let pop t =
  match t.frames with
  | [ _ ] | [] -> invalid_arg "Session.pop: no matching push"
  | _ :: rest ->
    t.frames <- rest;
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
         truncating. [build_model] runs OUTSIDE the CONTRACT-POISON firewall (below), so
         catch the projection [Overflow] HERE and degrade to no-model -> [Unknown] (sound;
         never a truncated model, and no [Model.t] unfreeze). *)
      None)
  else Some (assemble [] [])
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
    let assumptions = List.map Sat.pos t.frames in
    let v =
      match Sat.solve ~assumptions t.sat with
      | Sat.Unsat -> Unsat (* theory conflicts only strengthen unsat; still sound *)
      | Sat.Sat ->
        (* THE SOUNDNESS RULE (M4): report [Sat] only when a self-checkable model is
           reconstructable AND it passes the R1 in-process checker. This also firewalls
           the combination's known incompleteness/soundness gap on function applications
           appearing only inside arithmetic atoms (no purification pass yet; see the M4
           report): a model it cannot soundly build is [None] -> [Unknown]. *)
        (match build_model t with
         | Some m ->
           (* R1 (ADR-UF-models §3, codex TCB ruling): EVERY promoted [sat] passes the
              obligatory in-process self-check over every ACTIVE original assertion
              ([t.asserted], frame-scoped per F3) — function tables AND table-free (const
              / Bool / LIA) models alike. No [has_table] short-circuit exempts the
              sort-bearing const-only slice, so the trust story is uniform: "no [sat]
              without the checker." Fail-closed to [unknown]. The checker is cheap for
              const models; QF_UF tables carry no arithmetic, so its construct coverage is
              complete for the first cut. *)
           if Model_check.check m t.asserted
           then (
             t.last_model <- Some m;
             Sat)
           else Unknown
         | None -> Unknown)
      | exception Cdclt.Split_budget_exceeded ->
        (* Not a fault: the deterministic split cap fired. Distinct stat, sticky. *)
        t.degraded <- true;
        t.budget_exhausted <- true;
        Unknown
      | exception Budget.Exceeded ->
        (* Board #60: the deterministic effort cap fired (SAT conflicts/decisions + seam
           Final-rounds). NOT a fault and — unlike the split cap above — NOT sticky and
           does NOT set [degraded]: the search was merely cut off, the theory instance is
           not bricked, so the very same query is re-runnable at a larger [max_effort]
           (poison-free, per DESIGN §6). A distinct BUDGET tag ([effort_exhausted]), never
           a verdict from an unfinished search. *)
        t.effort_exhausted <- true;
        Unknown
      | exception Combine.Incomplete _ ->
        (* DELIBERATE completeness degrade (ADR-0010 §3.6 case (ii): a structured Bool
           compound under a UF argument the combinator chooses not to decide). A NAMED
           arm, not the CONTRACT-POISON catch-all below: this is a "known unknown", not a
           bricked theory instance. register_atom can raise it mid-solve (split-atom
           re-registration in [on_assign], [intern ~split:true] in [check]), so it is
           caught here as well as at the [assert_term] ingress. Sticky → Unknown. *)
        t.degraded <- true;
        Unknown
      | exception ((Out_of_memory | Stack_overflow) as e) ->
        (* Resource-exhaustion / control-flow asynchronous exceptions: the process state
           is not trustworthy, so we do NOT swallow them into a verdict — re-raise. *)
        raise e
      | exception _ ->
        (* CONTRACT-POISON firewall (I8), catch-all. [Sat.solve] drives the untrusted
           theory callbacks ([on_assign]/[check]/[explain]/[on_backtrack], which run
           [Combined.check]/[model]/[explain]/[register_atom]); ANY exception they let
           escape — a declared poison ([Lia.Poisoned], [Rational.Overflow],
           [Lia.Unsupported], [Combine.Combination_unsound],
           [Sat.Theory_contract_violation]) OR an unforeseen
           [Failure]/[Invalid_argument]/[Not_found]/[Term.Overflow] from a bug in theory
           code — bricks this query to [Unknown] rather than crashing the session or
           leaking a verdict from a bricked theory. This handler wraps ONLY the
           theory-driving [Sat.solve] call; model reconstruction ([build_model], in the
           [Sat.Sat] arm) and the session's own bookkeeping run OUTSIDE it, so a
           programming error there still surfaces as a crash rather than a silent
           [Unknown]. *)
        t.degraded <- true;
        Unknown
    in
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
