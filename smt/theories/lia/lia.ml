(* LIA decision procedure. See lia.mli.

   Layered on {!Simplex}: this module's job is the atom <-> bound translation (reading the
   ADR-0003 [Le]-normal form, taking the exact ℤ complement of a negated atom), variable
   bookkeeping (problem vars for term leaves, deduplicated slacks for compound linear
   forms), and the branch-and-bound driver over the rational simplex. *)

open Oxsmt_core

exception Unsupported of string
exception Poisoned

(* Simplex premise tokens: user atoms carry the caller's ['tok]; B&B branch bounds carry
   an internal marker so they never masquerade as an input-core premise. *)
type 'tok reason =
  | User of 'tok
  | Branch of int

type 'tok conflict =
  { premises : 'tok list
  ; farkas : Rational.t list
  }

type 'tok result =
  | Sat_candidate
  | Conflict of 'tok conflict

type 'tok integer_result =
  | Int_sat of (Term.t * int) list
  | Int_unsat of 'tok conflict option
  | Int_unknown

(* A registered atom's positive-polarity reading: a bound [var <sense> rhs]. *)
type reg =
  { atom : Term.t
  ; var : int
  ; is_upper : bool
  ; rhs : Delta.t
  }

type 'tok t =
  { ctx : Context.t
  ; simplex : 'tok reason Simplex.t
  ; var_of_term : int Term.Table.t (* problem-var term -> simplex id *)
  ; problem_vars : (int * Term.t) Dynarray.t (* (simplex id, term), creation order *)
  ; slacks : ((int * int) list, int) Hashtbl.t (* sorted (varid,coeff) key -> slack id *)
  ; registered : reg Dynarray.t
  ; reg_index :
      int Term.Table.t (* registered atom term -> its index in [registered]; O(1) dedup *)
  ; reg_by_var : (int, int list) Hashtbl.t
    (* simplex var id -> indices into [registered] of atoms whose bound is on that var.
         [propagate] visits only atoms on a [dirty] var, so this is the reverse lookup. *)
  ; dirty : (int, unit) Hashtbl.t
    (* vars whose simplex bound MAY have changed since the last [propagate] (set on
         every [assert_atom]/[register_atom] and on [pop] for un-reported atoms).
         Bound-to-bound entailment reads a var's own bounds only, and those bounds change
         only via those ops, so a var absent here cannot have any newly-entailed
         registered atom — the [propagate] delta scans exactly [dirty]. Cleared each
         [propagate]. *)
  ; reported : bool Dynarray.t
    (* parallel to [registered]: [true] once the atom has been emitted by [propagate] at
         a still-live frame (INVARIANT P: reported ⟺ currently bound-entailed). Skipped by
         the delta scan so an already-propagated atom is not re-emitted; reset on [pop]. *)
  ; mutable report_frames : int list list
    (* push/pop stack (head = current frame), each holding the reg indices first
         reported in that frame — mirrors the adapter's explain-cache framing so a [pop]
         un-reports exactly the atoms whose entailing bound it unwinds. *)
  ; mutable check_dirty : bool
    (* a simplex bound MAY have changed since the last [check] that returned
         [Sat_candidate]. Set by [assert_atom]/[pop]/[solve_integer] (the ops that mutate
         bounds); cleared only when [check] re-establishes feasibility. When clear, the
         tableau is still the feasible one the last [check] certified — so [check] can
         skip the simplex feasibility scan and return [Sat_candidate] without re-pivoting
         (FIX #3a). A [Conflict] leaves it set, so the next [check] re-runs. *)
  ; mutable overflows : int (* number of overflow-degradations to unknown *)
  }

let default_budget = 2000

let create ctx =
  { ctx
  ; simplex = Simplex.create ()
  ; var_of_term = Term.Table.create 64
  ; problem_vars = Dynarray.create ()
  ; slacks = Hashtbl.create 64
  ; registered = Dynarray.create ()
  ; reg_index = Term.Table.create 64
  ; reg_by_var = Hashtbl.create 64
  ; dirty = Hashtbl.create 64
  ; reported = Dynarray.create ()
  ; report_frames = [ [] ]
  ; check_dirty = true
  ; overflows = 0
  }
;;

(* Refuse to reason on a bricked instance (an overflow left the tableau mid-pivot, so any
   verdict would be unsound — CONTRACT: discard, don't reuse). Every public entry that
   could read or extend solver state calls this first. *)
let ensure_live t = if Simplex.is_poisoned t.simplex then raise Poisoned

(* Run [f]; if its arithmetic overflows, brick the instance before re-raising, so a caught
   [Rational.Overflow] can't be followed by reuse of a translation-corrupted instance.
   This is the [Lia]-side analogue of [Simplex.guarded] for arithmetic done OUTSIDE a
   simplex op (atom/equality translation, B&B branch bounds — codex L2/L4/L5). *)
let guard_overflow t f =
  try f () with
  | Rational.Overflow ->
    Simplex.poison t.simplex;
    raise Rational.Overflow
;;

(* Overflow-guarded native-int arithmetic on user-derived constants/coefficients: routed
   through the guarded {!Rational} ops (which raise {!Rational.Overflow} rather than
   wrap), so an atom constant or coefficient at the int boundary degrades to [unknown]
   instead of silently producing a WRONG bound (codex L2/L4/L5). All are integer-valued
   in, out. *)
let ineg n = Rational.num (Rational.neg (Rational.of_int n))
let isub a b = Rational.num (Rational.sub (Rational.of_int a) (Rational.of_int b))
let iadd a b = Rational.num (Rational.add (Rational.of_int a) (Rational.of_int b))

(* Get or create the problem variable for a term leaf. *)
let problem_var t (term : Term.t) =
  match Term.Table.find_opt t.var_of_term term with
  | Some id -> id
  | None ->
    let id = Simplex.new_problem_var t.simplex in
    Term.Table.replace t.var_of_term term id;
    Dynarray.add_last t.problem_vars (id, term);
    id
;;

(* Linear combination of an Int-sorted term: (problem-var id, coeff) pairs + integer
   const. [Arith] nodes carry the normalized form; a bare App/leaf is [1·leaf];
   [Int_const] is a pure constant. Int-[Ite] must have been removed by preprocessing. *)
let combo_of_term t (term : Term.t) : (int * int) list * int =
  match term.node with
  | Arith { coeffs; const } ->
    let pairs =
      Iarr.fold (fun acc (tm, c) -> (problem_var t tm, c) :: acc) [] coeffs |> List.rev
    in
    pairs, const
  | Int_const k -> [], k
  | Ite _ -> raise (Unsupported "LIA: Int-Ite must be removed by preprocessing")
  | _ -> [ problem_var t term, 1 ], 0
;;

let sort_key (pairs : (int * int) list) =
  List.sort (fun (a, _) (b, _) -> Int.compare a b) pairs
;;

(* The simplex variable carrying a linear combination, and whether the reported bound is a
   direct problem-var bound. Coeff-1 singletons bound their variable directly (DdM);
   anything else uses a deduplicated slack [s = Σ coeff·x]. *)
let var_for_combo t (pairs : (int * int) list) =
  match pairs with
  | [ (x, 1) ] -> x
  | _ ->
    let key = sort_key pairs in
    (match Hashtbl.find_opt t.slacks key with
     | Some s -> s
     | None ->
       let s =
         Simplex.new_slack t.simplex (List.map (fun (x, c) -> x, Rational.of_int c) pairs)
       in
       Hashtbl.replace t.slacks key s;
       s)
;;

(* Read [atom] (an [Le] or Int [Eq]) at [polarity] into simplex assertions [f]. [f] is
   [`Upper]/[`Lower] applied to (var, δ-rhs). Returns the list of (var, sense, rhs) so
   callers can either assert (setting bounds) or register (recording for propagation). *)
let constraints_of_atom t (atom : Term.t) ~polarity
  : (int * [ `Upper | `Lower ] * Delta.t) list
  =
  match atom.node with
  | Le inner ->
    let pairs, const = combo_of_term t inner in
    if pairs = [] then raise (Unsupported "LIA: constant Le atom (should be folded)");
    let var = var_for_combo t pairs in
    if polarity
    then
      (* Σ coeff·x + const <= 0 ==> var <= -const (guarded: const=min_int must not wrap) *)
      [ var, `Upper, Delta.of_rat (Rational.of_int (ineg const)) ]
    else
      (* ¬(inner <= 0) ≡ inner >= 1 (exact ℤ complement) ==> var >= 1 - const (guarded) *)
      [ var, `Lower, Delta.of_rat (Rational.of_int (isub 1 const)) ]
  | Eq (a, b) when not (Sort.equal a.sort Sort.bool) ->
    if not polarity then raise (Unsupported "LIA: disequality needs a trichotomy split");
    (* a = b ==> combo(a) - combo(b) = 0 ==> Σ coeff·x = -(const_a - const_b) *)
    let pa, ca = combo_of_term t a in
    let pb, cb = combo_of_term t b in
    (* coeff merges guarded (iadd/isub): a coefficient sum at the int boundary must raise,
       not wrap to a wrong merged constraint (codex L5). *)
    let merged =
      let tbl = Hashtbl.create 16 in
      let cur x =
        try Hashtbl.find tbl x with
        | Not_found -> 0
      in
      List.iter (fun (x, c) -> Hashtbl.replace tbl x (iadd (cur x) c)) pa;
      List.iter (fun (x, c) -> Hashtbl.replace tbl x (isub (cur x) c)) pb;
      Hashtbl.fold (fun x c acc -> if c = 0 then acc else (x, c) :: acc) tbl []
    in
    if merged = [] then raise (Unsupported "LIA: trivial equality (should be folded)");
    let var = var_for_combo t merged in
    (* rhs = -(ca - cb) = cb - ca, guarded *)
    let rhs = Delta.of_rat (Rational.of_int (isub cb ca)) in
    [ var, `Upper, rhs; var, `Lower, rhs ]
  | _ -> raise (Unsupported "LIA: atom is neither Le nor an Int equality")
;;

let assert_atom t atom ~polarity ~premise =
  ensure_live t;
  (* A new/tightened bound can make the tableau infeasible -> the next [check] must run. *)
  t.check_dirty <- true;
  guard_overflow t (fun () ->
    List.iter
      (fun (var, sense, rhs) ->
         (* [var]'s bound may tighten -> registered atoms on it may become newly entailed;
           mark it for the next [propagate] delta. (Marking on a no-op re-assertion of an
           already-entailed bound is harmless: the delta skips its already-reported
           atoms.) *)
         Hashtbl.replace t.dirty var ();
         let _ : _ Simplex.conflict option =
           match sense with
           | `Upper -> Simplex.assert_upper t.simplex var rhs (User premise)
           | `Lower -> Simplex.assert_lower t.simplex var rhs (User premise)
         in
         ())
      (constraints_of_atom t atom ~polarity))
;;

let register_atom t (atom : Term.t) =
  ensure_live t;
  (* Record the positive reading of a [Le] atom for bound-propagation. Equality atoms are
     not propagation targets in v1. *)
  match atom.node with
  | Le _ ->
    if not (Term.Table.mem t.reg_index atom)
    then
      guard_overflow t (fun () ->
        let add var is_upper rhs =
          let i = Dynarray.length t.registered in
          Dynarray.add_last t.registered { atom; var; is_upper; rhs };
          Dynarray.add_last t.reported false;
          Term.Table.replace t.reg_index atom i;
          Hashtbl.replace
            t.reg_by_var
            var
            (i
             ::
             (match Hashtbl.find_opt t.reg_by_var var with
              | Some is -> is
              | None -> []));
          (* A freshly registered atom may already be entailed by an existing bound on
             [var] (registration can follow the assert that set it). Mark [var] dirty so
             the next [propagate] reports it — matching the pre-delta scan, which
             re-evaluated every registered atom on every call. *)
          Hashtbl.replace t.dirty var ()
        in
        match constraints_of_atom t atom ~polarity:true with
        | [ (var, `Upper, rhs) ] -> add var true rhs
        | [ (var, `Lower, rhs) ] -> add var false rhs
        | _ -> ())
  | _ -> ()
;;

(* Map an internal simplex conflict to the public one, dropping any [Branch] premise
   (which only appears inside {!solve_integer}'s branches). *)
let externalize (c : _ Simplex.conflict) : 'tok conflict =
  let premises, farkas =
    List.fold_right2
      (fun p f (ps, fs) ->
         match p with
         | User tok -> tok :: ps, f :: fs
         | Branch _ -> ps, fs)
      c.premises
      c.farkas
      ([], [])
  in
  { premises; farkas }
;;

let check t =
  ensure_live t;
  (* FIX #3a: skip the simplex feasibility scan when no bound changed since the last
     feasible check. The tableau/assignment the previous [check] certified feasible is
     still current (no assert/pop happened), so returning [Sat_candidate] re-certifies the
     SAME feasible state — never an unrepaired one (the DdM invariants held then and
     nothing has touched them since). A [Conflict] leaves [check_dirty] set so the
     engine's backtrack (which [pop]s -> re-dirties) forces a real re-check. *)
  if not t.check_dirty
  then Sat_candidate
  else (
    match Simplex.check t.simplex with
    | None ->
      t.check_dirty <- false;
      Sat_candidate
    | Some c -> Conflict (externalize c))
;;

let rational_value t (term : Term.t) =
  ensure_live t;
  match Term.Table.find_opt t.var_of_term term with
  | Some id -> Delta.c_part (Simplex.value t.simplex id)
  | None -> Rational.zero
;;

(* Lookup-only [combo_of_term]: [term]'s (varid, coeff) pairs + integer const, WITHOUT
   allocating a problem var or slack (a fabric scan must not mutate the tableau merely by
   asking whether a shared term is fixed). [None] if any leaf has no simplex var yet. *)
let existing_combo t (term : Term.t) : ((int * int) list * int) option =
  let existing_problem tm = Term.Table.find_opt t.var_of_term tm in
  match term.node with
  | App _ ->
    (match existing_problem term with
     | Some id -> Some ([ id, 1 ], 0)
     | None -> None)
  | Arith { coeffs; const } ->
    let rec gather acc = function
      | [] -> Some (List.rev acc)
      | (tm, c) :: rest ->
        (match existing_problem tm with
         | None -> None
         | Some id -> gather ((id, c) :: acc) rest)
    in
    (match gather [] (Iarr.to_list coeffs) with
     | None -> None
     | Some pairs -> Some (pairs, const))
  | Int_const _ -> None
  | Bool_const _ | Le _ | Eq _ | Not _ | And _ | Or _ | Ite _ -> None
;;

(* Lookup-only [var_for_combo]: the existing simplex variable carrying [pairs], if any. A
   coeff-1 singleton is its own problem var; anything else is a deduplicated slack. *)
let existing_combo_var t pairs =
  match pairs with
  | [ (x, 1) ] -> Some x
  | _ -> Hashtbl.find_opt t.slacks (sort_key pairs)
;;

let negate_pairs pairs = List.map (fun (v, c) -> v, ineg c) pairs

(* The tightest ACTIVE ASSERTED (User) oriented bound of [term] on [which] side, in TERM
   space (const folded in), with its premise token — or [None].

   The subtlety (DdM): [var_for_combo] bounds only the coeff-+1 form directly, so [x <= c]
   lands on [x]'s variable but [x >= c] lands on the slack [s = -x] (as [-x <= -c]). A
   side's tightest bound may therefore live on the variable OR on its negated-combo slack
   (whose bound is on [-combo], so it flips). Only User bounds are fabric-citable — a
   [Branch] B&B bound carries no trail literal (F1-SEM ACTIVE EXACT). Shared by
   [fixed_bounds] (the fix-trigger, which bundles both sides + an equality test) and
   [oriented_bound_value] (the independent F1-SEM re-verifier's per-side read); the two
   are distinct consumers so a bug in the trigger's bundling/tuple/equality logic is
   caught by the verifier rather than trusted. *)
let tightest_oriented t (term : Term.t) (which : [ `Lower | `Upper ]) =
  match existing_combo t term with
  | None -> None
  | Some (pairs, const) ->
    let take_user b =
      match b with
      | Some (User tok, (d : Delta.t)) -> Some (d, tok)
      | Some (Branch _, _) | None -> None
    in
    let flip = function
      | Some (d, tok) -> Some (Delta.neg d, tok)
      | None -> None
    in
    let pos = existing_combo_var t pairs in
    let neg = existing_combo_var t (negate_pairs pairs) in
    let cands =
      match which with
      | `Upper ->
        (* [term <= d]: variable's own upper; and [-combo >= d'] ⇒ [combo <= -d']. *)
        [ (match pos with
           | Some v -> take_user (Simplex.get_upper t.simplex v)
           | None -> None)
        ; (match neg with
           | Some v -> flip (take_user (Simplex.get_lower t.simplex v))
           | None -> None)
        ]
      | `Lower ->
        [ (match pos with
           | Some v -> take_user (Simplex.get_lower t.simplex v)
           | None -> None)
        ; (match neg with
           | Some v -> flip (take_user (Simplex.get_upper t.simplex v))
           | None -> None)
        ]
    in
    let better a b =
      match which with
      | `Upper -> Delta.lt a b (* tightest upper = min *)
      | `Lower -> Delta.lt b a (* tightest lower = max *)
    in
    let tightest =
      match List.filter_map Fun.id cands with
      | [] -> None
      | x :: rest ->
        Some
          (List.fold_left
             (fun best c -> if better (fst c) (fst best) then c else best)
             x
             rest)
    in
    (match tightest with
     | Some (d, tok) when Delta.is_rational d && Rational.is_int (Delta.c_part d) ->
       Some (tok, Rational.add (Delta.c_part d) (Rational.of_int const))
     | _ -> None)
;;

(* [fixed_bounds t term] — is [term] pinned to one integer by ACTIVE ASSERTED bounds? Its
   tightest lower and upper coincide on an integer [v]; returns
   [(v, lower_tok, upper_tok)] where [lower_tok] proves [term >= v] and [upper_tok] proves
   [term <= v] (the two oriented Farkas premises the fabric injection needs). This is the
   fix-TRIGGER. *)
let fixed_bounds t term =
  ensure_live t;
  match tightest_oriented t term `Lower, tightest_oriented t term `Upper with
  | Some (lo_tok, lv), Some (up_tok, uv) when Rational.equal lv uv ->
    Some (lv, lo_tok, up_tok)
  | _ -> None
;;

(* ADR-0014 Stage 1b F1-SEM independent oriented-bound accessor (§B.1 C1/Rev5-B3). Returns
   ONE oriented bound of [term] as [(token, value)] with NO cross-side equality bundling.
   The fabric's semantic re-verifier consumes it to re-derive, by a path independent of
   the [fixed_bounds] tuple, that a fixed-value pair's cited premises really are that
   term's oriented bounds at the group value — so a [fixed_bounds] bug (wrong value,
   swapped or foreign token, dropped/non-exact bound) is REJECTED rather than injected as
   an unsound merge, and the ADR's weak-Γ acceptance mutant is non-vacuous. *)
let oriented_bound_value t (term : Term.t) (which : [ `Lower | `Upper ]) =
  ensure_live t;
  tightest_oriented t term which
;;

let value_is_integer d = Delta.is_rational d && Rational.is_int (Delta.c_part d)

(* Lowest-tag problem variable whose current value is non-integer (needs branching).
   Branching (below) floors the c_part only. That is exact here because the LIA atom
   translation never emits a δ bound (every assert_atom bound has k=0), so problem-var
   values are always δ=0; a hypothetical strict δ bound wired through this layer would
   still branch soundly (x<=floor ∨ x>=floor+1 partitions ℤ) but could spin to the budget. *)
let first_non_integer t =
  let best = ref None in
  Dynarray.iter
    (fun (id, term) ->
       let d = Simplex.value t.simplex id in
       if not (value_is_integer d)
       then (
         match !best with
         | Some (bt, _, _) when bt.Term.tag <= term.Term.tag -> ()
         | _ -> best := Some (term, id, d)))
    t.problem_vars;
  !best
;;

let suggest_branch t =
  ensure_live t;
  match first_non_integer t with
  | None -> None
  | Some (term, _, d) ->
    let f = Rational.floor (Delta.c_part d) in
    let fp1 = guard_overflow t (fun () -> iadd f 1) in
    let le_atom = Context.le t.ctx term (Context.int_const t.ctx f) in
    let ge_atom = Context.ge t.ctx term (Context.int_const t.ctx fp1) in
    Some (le_atom, ge_atom)
;;

let extract_model t =
  Dynarray.fold_left
    (fun acc (id, term) ->
       let d = Simplex.value t.simplex id in
       if not (value_is_integer d)
       then failwith "Lia.model: variable is not integral (call after Int_sat)";
       (term, Rational.num (Delta.c_part d)) :: acc)
    []
    t.problem_vars
  |> List.sort (fun (a, _) (b, _) -> Int.compare a.Term.tag b.Term.tag)
;;

let model t =
  ensure_live t;
  extract_model t
;;

let solve_integer ?(budget = default_budget) t =
  ensure_live t;
  (* B&B pushes/asserts/pops simplex bounds directly (bypassing [assert_atom]); DdM does
     not restore VALUES on [pop], so the assignment left behind need not be feasible for
     the restored bounds. Force any later [check] to re-run rather than trust the gate. *)
  t.check_dirty <- true;
  let splits = ref 0 in
  (* [root_conflict] captures a ℚ-level conflict found with no branch in scope, so a
     genuine single-Farkas certificate can be surfaced. *)
  let root_conflict = ref None in
  let rec dfs ~depth0 =
    match Simplex.check t.simplex with
    | Some c ->
      if depth0 then root_conflict := Some (externalize c);
      `Unsat
    | None ->
      (match first_non_integer t with
       | None -> `Sat (extract_model t)
       | Some (_, id, d) ->
         if !splits >= budget
         then `Unknown
         else (
           incr splits;
           let f = Rational.floor (Delta.c_part d) in
           let lo = Delta.of_rat (Rational.of_int f) in
           (* f+1 guarded: a branch point at the int boundary must raise (→ Int_unknown
              below), not wrap to a bogus bound (codex L2/L4/L5 class). *)
           let hi = Delta.of_rat (Rational.of_int (iadd f 1)) in
           Simplex.push t.simplex;
           let _ = Simplex.assert_upper t.simplex id lo (Branch id) in
           let r1 = dfs ~depth0:false in
           Simplex.pop t.simplex 1;
           match r1 with
           | (`Sat _ | `Unknown) as r -> r
           | `Unsat ->
             Simplex.push t.simplex;
             let _ = Simplex.assert_lower t.simplex id hi (Branch id) in
             let r2 = dfs ~depth0:false in
             Simplex.pop t.simplex 1;
             r2))
  in
  (* Native-int incompleteness ceiling (I8): DdM pivoting and Farkas combinations grow
     coefficients internally, so guarded rational arithmetic can overflow mid-solve on
     small, non-adversarial inputs. Raising is sound; here — the complete decision driver
     — we degrade that to [Int_unknown] and count it as a distinct stat so a benchmark
     pass-rate gap is attributable, not a mystery. The fix is arbitrary-precision
     rationals (tracked as the core-bignum row, post-M4). *)
  match dfs ~depth0:true with
  | `Sat m -> Int_sat m
  | `Unknown -> Int_unknown
  | `Unsat -> Int_unsat !root_conflict
  | exception Rational.Overflow ->
    (* Poison regardless of where the overflow arose (mid-pivot via Simplex.guarded, or a
       branch-point iadd here): the instance is not safe to reuse. *)
    Simplex.poison t.simplex;
    t.overflows <- t.overflows + 1;
    Int_unknown
;;

(* Incremental bound-to-bound propagation (delta). Report only the atoms whose entailment
   became newly TRUE/FALSE since the last call, not the whole entailed set every check.

   Since an atom's entailment reads only its own var's bounds, and those bounds change
   only through [assert_atom]/[register_atom] (recorded in [dirty]) and [pop] (which
   re-dirties the un-reported), it suffices to visit atoms on [dirty] vars and skip those
   already [reported]. Candidates are sorted by registration index so the emitted order is
   deterministic (I6) and identical to the pre-delta full scan.

   Un-reported reporting is monotone within a frame (a bound that entails an atom only
   tightens until a [pop]), so a [reported] atom is still entailed and needs no re-check.
   [pop] restores that invariant by un-reporting the atoms of the frames it unwinds. *)
let propagate t =
  ensure_live t;
  let out = ref [] in
  let cands =
    Hashtbl.fold
      (fun var () acc ->
         match Hashtbl.find_opt t.reg_by_var var with
         | Some is -> List.rev_append is acc
         | None -> acc)
      t.dirty
      []
    |> List.sort_uniq Int.compare
  in
  Hashtbl.clear t.dirty;
  List.iter
    (fun i ->
       if not (Dynarray.get t.reported i)
       then (
         let r = Dynarray.get t.registered i in
         (* atom's positive reading is [var <sense> rhs]. TRUE if the current bound already
           entails it; FALSE if the current opposite bound refutes it. Explanation = the
           single entailing bound (Lia_bound). *)
         let up = Simplex.get_upper t.simplex r.var in
         let lo = Simplex.get_lower t.simplex r.var in
         let emit polarity prem =
           out := (r.atom, polarity, [ prem ]) :: !out;
           Dynarray.set t.reported i true;
           match t.report_frames with
           | fr :: rest -> t.report_frames <- (i :: fr) :: rest
           | [] -> t.report_frames <- [ [ i ] ]
         in
         if r.is_upper
         then (
           (* atom: var <= rhs *)
           match up with
           | Some (User tok, u) when Delta.le u r.rhs -> emit true tok
           | _ ->
             (match lo with
              | Some (User tok, l) when Delta.lt r.rhs l -> emit false tok
              | _ -> ()))
         else (
           (* atom: var >= rhs *)
           match lo with
           | Some (User tok, l) when Delta.le r.rhs l -> emit true tok
           | _ ->
             (match up with
              | Some (User tok, u) when Delta.lt u r.rhs -> emit false tok
              | _ -> ()))))
    cands;
  List.rev !out
;;

let push t =
  ensure_live t;
  Simplex.push t.simplex;
  t.report_frames <- [] :: t.report_frames
;;

let pop t n =
  ensure_live t;
  Simplex.pop t.simplex n;
  (* Restoring (loosening) bounds can leave the assignment infeasible for the restored
     frame; the next [check] must re-run rather than trust the gate. *)
  t.check_dirty <- true;
  (* Un-report every atom first reported in a popped frame: its entailing bound is being
     unwound. Re-dirty its var so the next [propagate] re-checks it and re-emits if a
     surviving (shallower) bound still entails it — this is what lets an atom entailed at
     level N and backtracked past be re-reported when re-entailed (CONTRACT-EX). *)
  let rec drop k frames =
    if k = 0
    then frames
    else (
      match frames with
      | fr :: rest ->
        List.iter
          (fun i ->
             Dynarray.set t.reported i false;
             Hashtbl.replace t.dirty (Dynarray.get t.registered i).var ())
          fr;
        drop (k - 1) rest
      | [] -> [])
  in
  t.report_frames
  <- (match drop n t.report_frames with
      | [] -> [ [] ]
      | fs -> fs)
;;

(* Diagnostics stay readable after poisoning (you need [overflow_count] precisely to
   attribute the brick). *)
let pivot_count t = Simplex.pivot_count t.simplex
let overflow_count t = t.overflows
let is_poisoned t = Simplex.is_poisoned t.simplex
