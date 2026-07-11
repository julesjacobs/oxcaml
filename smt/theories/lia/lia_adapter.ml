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

type t =
  { lia : Lit.t Lia.t
  ; term_of_atom : Term.t Atom.Table.t (* engine atom id -> its registered [Term.t] *)
  ; atom_of_term : Atom.t Term.Table.t
    (* reverse map, for turning a propagated term back into its literal *)
  ; mutable explain_cache :
      Explanation.t Lit.Map.t (* propagated lit -> its lazy reason *)
  ; mutable frames : Lit.t list list
    (* per-frame lits cached, head = current frame; used to drop stale reasons on [pop] *)
  ; mutable overflows : int (* overflow-induced degradations to unknown (adapter side) *)
  }

let create ctx _env =
  { lia = Lia.create ctx
  ; term_of_atom = Atom.Table.create 64
  ; atom_of_term = Term.Table.create 64
  ; explain_cache = Lit.Map.empty
  ; frames = [ [] ]
  ; overflows = 0
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
    | Some term -> Lia.assert_atom t.lia term ~polarity:(Lit.sign lit) ~premise:lit
    | None ->
      (* CONTRACT: [assert_lit]'s atom was registered first. A miss is a driver bug; fail
         loud -> engine degrades to unknown rather than reasoning on an unmapped atom. *)
      failwith "Lia_adapter.assert_lit: literal's atom was not registered")
;;

let conflict_explanation (c : Lit.t Lia.conflict) : Explanation.t =
  (* Premises are the [Lit.t] tokens of the infeasible bound set; the Farkas multipliers
     stay engine-internal (self-checked at production, DESIGN §7) and route to the
     off-core M5 certificate module, never onto the frozen [Explanation] (ADR-0005 D7 /
     ADR-0006). *)
  { premises = c.premises; rule = Explanation.Rule_tag.Lia_farkas }
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
      cache_reason t lit { Explanation.premises; rule = Explanation.Rule_tag.Lia_bound };
      Some lit)
;;

let check t (effort : Theory.effort) : Theory.check_result =
  guard t (fun () ->
    match Lia.check t.lia with
    | Conflict c -> Theory.Conflict (conflict_explanation c)
    | Sat_candidate ->
      (match effort with
       | Theory.Propagate -> Theory.Propagations (propagations t)
       | Theory.Final ->
         (* Rational-feasible: integral -> genuine ℤ model -> Sat; else ask CDCL(T) to
            branch on the two distinct, currently-false atoms [x<=floor v] /
            [x>=floor v+1] (CONTRACT-SPLIT: >=2 distinct atoms, genuinely constraining —
            not the discarded [Eq v ¬Eq] tautology). *)
         (match Lia.suggest_branch t.lia with
          | None -> Theory.Sat
          | Some (le_atom, ge_atom) -> Theory.Split [ le_atom; ge_atom ])))
;;

let explain t lit =
  match Lit.Map.find_opt lit t.explain_cache with
  | Some expl -> expl
  | None ->
    (* [explain] is only defined for a literal THIS theory propagated and still on the
       trail; its reason was cached at propagation time. A miss is a driver/contract
       violation — fail loud rather than fabricate an unsound premise set. *)
    failwith "Lia_adapter.explain: no cached reason for literal (not theory-propagated?)"
;;

let model t =
  (* Valid only after [check Final] returned [Sat] (all problem vars integral);
     [Lia.model] raises otherwise. LIA emits only [Int] values. *)
  Lia.model t.lia |> List.map (fun (term, v) -> term, Model.Int v) |> Model.of_alist
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
