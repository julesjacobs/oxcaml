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
  ; slacks : ((int * string) list, int) Hashtbl.t
      (* sorted (varid, canonical-coeff-string) key -> slack id; the coeff is stringified
         via [Rational.to_string] because coefficients are now arbitrary-precision
         [Rational.t] (core-bignum W2) and must not be keyed by a polymorphic hash of the
         two-tier value. *)
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
  ; mutable last_cube_model : (Term.t * int) list option
      (* set by [cube_model] when the Bromberger-Fleury cube test found an integer model
         at the current Final; read once by [model] (the adapter reads it immediately
         after the Final->Sat). Cleared at the start of every [check] so it can never
         satisfy a later, non-cube Sat with a stale point. *)
  ; mutable eq_frames : (Term.t * Term.t * 'tok) list list
      (* push/pop stack (head = current frame) of asserted positive Int equalities as
         [(lhs, rhs, premise)], mirroring [report_frames]' framing so a [pop] drops
         exactly the equalities asserted in the unwound frames. Read by
         {!diophantine_conflict}. *)
  ; mutable cube_tried : bool
  (* the cube test runs at most ONCE per instance — the first non-integral Final, which
     for a batch query is the b&b root (fat feasible regions are cracked there). This
     bounds its extra LP solve to one per query, so it cannot accumulate overhead on a
     file that b&b would otherwise close within the wall (the [cut_lemma] unsat
     regression). *)
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
  ; last_cube_model = None
  ; eq_frames = [ [] ]
  ; cube_tried = false
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

(* Linear combination of an Int-sorted term: (problem-var id, coeff) pairs + const, with
   coefficients/const as arbitrary-precision {!Rational.t} (core-bignum W2 — a term
   coefficient can exceed int63). [Arith] nodes carry the normalized form; a bare App/leaf
   is [1·leaf]; [Int_const] is a pure constant. Int-[Ite] must have been removed by
   preprocessing. The [Bigint -> Rational] widen never loses precision and never raises;
   the residual int boundary is only at model extraction. *)
let combo_of_term t (term : Term.t) : (int * Rational.t) list * Rational.t =
  match term.node with
  | Arith { coeffs; const } ->
    let pairs =
      Iarr.fold
        (fun acc (tm, c) -> (problem_var t tm, Rational.of_bigint c) :: acc)
        []
        coeffs
      |> List.rev
    in
    pairs, Rational.of_bigint const
  | Int_const k -> [], Rational.of_bigint k
  | Ite _ -> raise (Unsupported "LIA: Int-Ite must be removed by preprocessing")
  | _ -> [ problem_var t term, Rational.one ], Rational.zero
;;

(* Canonical dedup key for a slack definition: sort by varid, stringify the coefficient
   (value-canonical, so it does not depend on the [Rational] tier). *)
let sort_key (pairs : (int * Rational.t) list) : (int * string) list =
  List.sort (fun (a, _) (b, _) -> Int.compare a b) pairs
  |> List.map (fun (x, c) -> x, Rational.to_string c)
;;

(* The simplex variable carrying a linear combination, and whether the reported bound is a
   direct problem-var bound. Coeff-1 singletons bound their variable directly (DdM);
   anything else uses a deduplicated slack [s = Σ coeff·x]. *)
let var_for_combo t (pairs : (int * Rational.t) list) =
  match pairs with
  | [ (x, c) ] when Rational.equal c Rational.one -> x
  | _ ->
    let key = sort_key pairs in
    (match Hashtbl.find_opt t.slacks key with
     | Some s -> s
     | None ->
       let s = Simplex.new_slack t.simplex pairs in
       Hashtbl.replace t.slacks key s;
       s)
;;

(* The simplex reading of a positive Int equality [a = b]. When the variable combination
   of [combo(a) - combo(b)] cancels, the equality is a CONSTANT RELATION with no simplex
   variable to bound, and the two sub-cases are DISTINCT and must not be conflated:
   - [Trivially_true] ([0 = 0], the constants also match): a tautology — no constraint.
   - [Trivially_false] ([0 = k], k <> 0): an UNSATISFIABLE relation — a live constraint.
     Otherwise [Bounds] carries the pair of bounds [Σ coeff·x = cb - ca]. Callers pick the
     policy per sub-case (see {!constraints_of_atom} and {!notify_equality}); collapsing
     the two constant cases into one is a soundness hazard (silently dropping a [0 = k] is
     a wrong-verdict hole). Uses the same var-creating {!combo_of_term} as the assert
     path, so the classification matches assertion exactly — a lookup-only test could
     disagree when a leaf's variable is not registered yet. *)
type equality_reading =
  | Bounds of (int * [ `Upper | `Lower ] * Delta.t) list
  | Trivially_true
  | Trivially_false

(* The merged linear form of [a = b]: [Σ coeff·x = rhs] with [rhs = cb - ca], over
   problem-var ids, exact [Rational] coefficients (never wraps). Shared by
   {!equality_reading} (the simplex bound translation) and {!diophantine_conflict} (the
   integer-feasibility test), so both read the SAME normalized combo. *)
let equality_merged t (a : Term.t) (b : Term.t) : (int * Rational.t) list * Rational.t =
  let pa, ca = combo_of_term t a in
  let pb, cb = combo_of_term t b in
  let merged =
    let tbl = Hashtbl.create 16 in
    let cur x =
      try Hashtbl.find tbl x with
      | Not_found -> Rational.zero
    in
    List.iter (fun (x, c) -> Hashtbl.replace tbl x (Rational.add (cur x) c)) pa;
    List.iter (fun (x, c) -> Hashtbl.replace tbl x (Rational.sub (cur x) c)) pb;
    Hashtbl.fold (fun x c acc -> if Rational.is_zero c then acc else (x, c) :: acc) tbl []
  in
  merged, Rational.sub cb ca
;;

let equality_reading t (a : Term.t) (b : Term.t) : equality_reading =
  (* a = b ==> combo(a) - combo(b) = 0 ==> Σ coeff·x = -(const_a - const_b) *)
  let merged, rhs_rat = equality_merged t a b in
  match merged with
  | [] ->
    (* no variable term: the equality is the constant relation [0 = rhs] *)
    if Rational.is_zero rhs_rat then Trivially_true else Trivially_false
  | _ :: _ ->
    let var = var_for_combo t merged in
    let rhs = Delta.of_rat rhs_rat in
    Bounds [ var, `Upper, rhs; var, `Lower, rhs ]
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
      (* Σ coeff·x + const <= 0 ==> var <= -const *)
      [ var, `Upper, Delta.of_rat (Rational.neg const) ]
    else
      (* ¬(inner <= 0) ≡ inner >= 1 (exact ℤ complement) ==> var >= 1 - const *)
      [ var, `Lower, Delta.of_rat (Rational.sub Rational.one const) ]
  | Eq (a, b) when not (Sort.equal a.sort Sort.bool) ->
    if not polarity then raise (Unsupported "LIA: disequality needs a trichotomy split");
    (* The user assert/register path raises on BOTH constant sub-cases, exactly as before:
       a [0 = k] is a live constraint that must not be dropped, and a [0 = 0] should have
       been folded by preprocessing — hitting either here is a contract violation. *)
    (match equality_reading t a b with
     | Bounds cs -> cs
     | Trivially_true | Trivially_false ->
       raise (Unsupported "LIA: trivial equality (should be folded)"))
  | _ -> raise (Unsupported "LIA: atom is neither Le nor an Int equality")
;;

(* Apply already-computed (var, sense, rhs) bounds to the simplex, attributing each to
   [premise]. Marks each touched var dirty for the next propagate delta. Callers wrap this
   in {!guard_overflow} so a coefficient overflow during the preceding combo computation
   poisons cleanly. *)
let apply_bounds t cs ~premise =
  List.iter
    (fun (var, sense, rhs) ->
      (* [var]'s bound may tighten -> registered atoms on it may become newly entailed;
         mark it for the next [propagate] delta. (Marking on a no-op re-assertion of an
         already-entailed bound is harmless: the delta skips its already-reported atoms.) *)
      Hashtbl.replace t.dirty var ();
      let _ : _ Simplex.conflict option =
        match sense with
        | `Upper -> Simplex.assert_upper t.simplex var rhs (User premise)
        | `Lower -> Simplex.assert_lower t.simplex var rhs (User premise)
      in
      ())
    cs
;;

let assert_atom t atom ~polarity ~premise =
  ensure_live t;
  (* A new/tightened bound can make the tableau infeasible -> the next [check] must run. *)
  t.check_dirty <- true;
  (* Record a positive Int equality for the integer-feasibility (gcd) test. Only the
     positive polarity is a genuine equation [a = b]; a negated equality raises
     [Unsupported] below (it is resolved by a trichotomy split, never asserted here). The
     record is framed like [report_frames] so [pop] drops exactly this scope's equations. *)
  (match (atom : Term.t).node with
   | Eq (a, b) when polarity && not (Sort.equal a.sort Sort.bool) ->
     (match t.eq_frames with
      | fr :: rest -> t.eq_frames <- ((a, b, premise) :: fr) :: rest
      | [] -> t.eq_frames <- [ [ a, b, premise ] ])
   | _ -> ());
  guard_overflow t (fun () ->
    apply_bounds t (constraints_of_atom t atom ~polarity) ~premise)
;;

(* ADR-0014 Stage 2 fabric [new_eq] entry: assert an EUF-entailed positive Int equality
   [eq] into the tableau, attributed to [premise]. Differs from {!assert_atom} for a
   positive equality ONLY in the [Trivially_true] ([0 = 0]) case: the merge callback fires
   whenever the e-graph unions two Int classes, and congruence can RE-SURFACE an equality
   LIA already relates (its variable combination already cancels to [0 = 0]). That
   re-notification is a tautology — no LIA constraint — so it is a NO-OP here instead of
   the {!Unsupported} raise that would degrade the whole query to [unknown].

   The [Trivially_false] ([0 = k], k <> 0) case is deliberately NOT no-oped: that relation
   is UNSATISFIABLE, so silently dropping it would be a wrong-verdict hole. It keeps
   raising {!Unsupported} exactly like {!assert_atom} — the same fail-closed [unknown] as
   trunk, never a laundered [sat]. (A genuine [0 = k] merge means the branch is
   inconsistent; the combinator's find_disagreement/model-agreement split still surfaces
   and splits the pair, which reaches this same raise, so behaviour is unchanged and
   sound.)

   [Bounds] asserts as usual. When [Context.eq] already FOLDED the equality to a boolean
   constant (it folds two equal constants to [true] and two unequal ones to [false]), the
   [true] fold is a tautology (no-op) but the [false] fold is [Context.eq c1 c2] with
   [c1 <> c2] — an UNSATISFIABLE equality — so it fails closed exactly like
   [Trivially_false] (symmetric; a silent no-op there is the same wrong-verdict hole in
   the folded path). *)
let notify_equality t (eq : Term.t) ~premise =
  ensure_live t;
  match eq.node with
  | Eq (a, b) when not (Sort.equal a.sort Sort.bool) ->
    t.check_dirty <- true;
    guard_overflow t (fun () ->
      match equality_reading t a b with
      | Trivially_true -> () (* [0 = 0] re-notification: no LIA constraint, skip *)
      | Trivially_false ->
        (* [0 = k], k <> 0: unsatisfiable — must NOT be silently dropped (wrong-verdict
           hole). Fail closed, exactly like {!assert_atom}. *)
        raise (Unsupported "LIA: trivial equality (should be folded)")
      | Bounds cs -> apply_bounds t cs ~premise)
  | Bool_const true ->
    () (* [Context.eq] folded a true equality: tautology, no constraint *)
  | Bool_const false ->
    (* [Context.eq] folded [c1 = c2], c1 <> c2: an unsatisfiable equality. Fail closed
       (symmetric with [Trivially_false]); never silently drop it. *)
    raise (Unsupported "LIA: trivial equality (should be folded)")
  | _ ->
    (* Not an Int equality (defensive: the combinator only notifies Int-class merges);
       fall back to the strict assert so any genuinely unhandled shape still degrades
       loudly. *)
    assert_atom t eq ~polarity:true ~premise
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
  (* A cube model is valid only within the single Final->model window that produced it;
     clear it here so a later (non-cube) Sat can never read a stale point. *)
  t.last_cube_model <- None;
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

(* Lookup-only [combo_of_term]: [term]'s (varid, coeff) pairs + const, WITHOUT allocating
   a problem var or slack (a fabric scan must not mutate the tableau merely by asking
   whether a shared term is fixed). [None] if any leaf has no simplex var yet.
   Coefficients/const are arbitrary-precision [Rational.t] (core-bignum W2), mirroring
   {!combo_of_term} exactly so the [sort_key] slack lookup below hits the SAME key the
   real ingest recorded (a native-int projection here would compute a different key and
   silently miss the slack). *)
let existing_combo t (term : Term.t) : ((int * Rational.t) list * Rational.t) option =
  let existing_problem tm = Term.Table.find_opt t.var_of_term tm in
  match term.node with
  | App _ ->
    (match existing_problem term with
     | Some id -> Some ([ id, Rational.one ], Rational.zero)
     | None -> None)
  | Arith { coeffs; const } ->
    let rec gather acc = function
      | [] -> Some (List.rev acc)
      | (tm, c) :: rest ->
        (match existing_problem tm with
         | None -> None
         | Some id -> gather ((id, Rational.of_bigint c) :: acc) rest)
    in
    (match gather [] (Iarr.to_list coeffs) with
     | None -> None
     | Some pairs -> Some (pairs, Rational.of_bigint const))
  | Int_const _ -> None
  | Bool_const _ | Le _ | Eq _ | Not _ | And _ | Or _ | Ite _ -> None
;;

(* Lookup-only [var_for_combo]: the existing simplex variable carrying [pairs], if any. A
   coeff-1 singleton is its own problem var; anything else is a deduplicated slack. *)
let existing_combo_var t pairs =
  match pairs with
  | [ (x, c) ] when Rational.equal c Rational.one -> Some x
  | _ -> Hashtbl.find_opt t.slacks (sort_key pairs)
;;

let negate_pairs pairs = List.map (fun (v, c) -> v, Rational.neg c) pairs

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
       Some (tok, Rational.add (Delta.c_part d) const)
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

(* Integer-feasibility (GCD / Diophantine) test on a single asserted equality.

   The rational simplex certifies ℚ-feasibility, but an equation like [4·s + 4·x = 6] is
   ℚ-feasible ([s=1.5]) while ℤ-INFEASIBLE (its integer combinations are multiples of
   [gcd(4,4)=4], which does not divide 6). Without this test the b&b driver
   ({!suggest_branch}) wanders indefinitely on such a row. This is a standard sound
   integer-infeasibility certificate, ORTHOGONAL to the (refuted) cross-theory propagation
   levers: it only ever REPORTS A CONFLICT on a genuinely ℤ-infeasible state; it never
   merges classes, injects atoms, or otherwise perturbs the search.

   For each recorded positive Int equality [Σ cᵢ·xᵢ = rhs], substitute every variable the
   simplex has PINNED to a single integer value [vᵢ] (via {!fixed_bounds}, whose two
   oriented-bound tokens prove [xᵢ = vᵢ]); the residual equation over the still-free
   variables is [Σ_free cⱼ·xⱼ = rhs − Σ_fixed cᵢ·vᵢ]. All coefficients and the residual
   are integer-valued (combo coefficients come from the normalized [Arith] form; fixed
   values are integers). If [gcd(free cⱼ)] does not divide the residual, no integer
   assignment of the free variables satisfies the equation: emit a conflict whose premises
   are the equality literal together with the oriented-bound tokens of every substituted
   variable (that conjunction is ℤ-unsatisfiable). A fully-fixed equation is left to the ℚ
   simplex (an inconsistent constant relation is already a rational conflict). Any
   [Rational] projection overflow or a non-integer coefficient (out of the integer
   fragment) skips the row soundly (no conflict claimed).

   [farkas] carries no rational multiplier here (the state is ℚ-feasible, so no Farkas
   vector exists); the field is filled with a same-length placeholder and is never read on
   this path (the adapter forwards only the premise set + rule tag, and {!externalize} —
   the sole farkas consumer — is not on this path). The certificate/replay layer needs a
   dedicated Diophantine rule tag; that is a follow-up (see the lane log). *)
let diophantine_conflict t : 'tok conflict option =
  ensure_live t;
  if Simplex.is_poisoned t.simplex
  then None
  else (
    let gcd_int a b =
      let rec go a b = if b = 0 then a else go b (a mod b) in
      go (abs a) (abs b)
    in
    (* [fixed]: var id -> (integer value, the trail-literal premises that pin it). Seeded
       with the SIMPLEX-DIRECT fixings ([fixed_bounds], two oriented-bound tokens), then
       extended by fixpoint over the asserted equations: a var determined transitively
       through the equation system (e.g. [arg0 = fmt0 - distance] with [fmt0], [distance]
       fixed) is NOT a direct simplex bound, so this equation-level closure is what makes
       the substitution complete. Each closure entry's premises accumulate the equation's
       literal plus the premises of every fixed var it was solved from — so a conflict
       built from these premises cites only real trail literals whose conjunction is
       ℤ-unsatisfiable. *)
    let fixed : (int, Rational.t * 'tok list) Hashtbl.t = Hashtbl.create 64 in
    Dynarray.iter
      (fun (id, term) ->
        match fixed_bounds t term with
        | Some (v, lo, hi) -> Hashtbl.replace fixed id (v, [ lo; hi ])
        | None -> ())
      t.problem_vars;
    (* Rows: each asserted equality's merged linear form + rhs + its literal. Malformed /
       out-of-fragment rows are dropped soundly (no conflict claimed for them). *)
    let rows =
      List.filter_map
        (fun (a, b, tok) ->
          try
            let merged, rhs = equality_merged t a b in
            if List.for_all (fun (_, c) -> Rational.is_int c) merged
               && Rational.is_int rhs
            then Some (merged, rhs, tok)
            else None
          with
          | Exit | Rational.Overflow -> None)
        (List.concat t.eq_frames)
    in
    let conflict = ref None in
    (* Split a row by the current [fixed] map: residual (rhs minus the fixed
       contributions), the free (var,coeff) list, and the accumulated premises (row
       literal + every used fixed var's premises). *)
    let split_row (merged, rhs, tok) =
      let residual = ref rhs in
      let prems = ref [ tok ] in
      let free = ref [] in
      List.iter
        (fun (id, c) ->
          match Hashtbl.find_opt fixed id with
          | Some (v, ps) ->
            residual := Rational.sub !residual (Rational.mul c v);
            prems := List.rev_append ps !prems
          | None -> free := (id, c) :: !free)
        merged;
      !residual, !free, !prems
    in
    (* One closure sweep: for each row solve/deduce, returning whether [fixed] grew. Sets
       [conflict] on the first ℤ-infeasible row found. *)
    let sweep () =
      let changed = ref false in
      List.iter
        (fun row ->
          if !conflict = None
          then (
            try
              let residual, free, prems = split_row row in
              match free with
              | [] ->
                (* fully fixed: the equation must hold; a nonzero residual contradicts the
                   substituted values. *)
                if not (Rational.is_zero residual)
                then conflict := Some { premises = prems; farkas = [] }
              | [ (id, c) ] ->
                (* [c·x = residual] pins x. Non-integer quotient ⇒ no integer x ⇒
                   conflict; else record x as fixed (premises = this row's). *)
                let q = Rational.div residual c in
                if not (Rational.is_int q)
                then conflict := Some { premises = prems; farkas = [] }
                else if not (Hashtbl.mem fixed id)
                then (
                  Hashtbl.replace fixed id (q, prems);
                  changed := true)
              | _ ->
                (* ≥2 free vars: gcd test. Σ_free cⱼ·xⱼ = residual has an integer solution
                   only if gcd(cⱼ) | residual. *)
                let g =
                  List.fold_left (fun acc (_, c) -> gcd_int acc (Rational.num c)) 0 free
                in
                if g <> 0 && Rational.num residual mod g <> 0
                then conflict := Some { premises = prems; farkas = [] }
            with
            | Exit | Rational.Overflow -> ()))
        rows;
      !changed
    in
    (* Iterate to a fixpoint (bounded: [fixed] only grows, ≤ #vars) or first conflict. *)
    let rec loop () = if !conflict = None && sweep () then loop () in
    loop ();
    !conflict)
;;

(* ADR-0014 Stage 1b F1-SEM independent oriented-bound accessor (§B.1 C1/Rev5-B3). Returns
   ONE oriented bound of [term] as [(token, value)] with NO cross-side equality bundling.
   The fabric's semantic re-verifier consumes it to re-derive, by a path independent of
   the [fixed_bounds] tuple, that a fixed-value pair's cited premises really are that
   term's oriented bounds at the group value — so a [fixed_bounds] bug (wrong value,
   swapped or foreign token, dropped/non-exact bound) is REJECTED rather than injected as
   an unsound merge, and the ADR's weak-Γ acceptance mutant is non-vacuous.

   Independence (codex F1-SEM residual): this DELIBERATELY does not call
   [tightest_oriented] — it re-reads the raw simplex bounds by a separate code path with
   its own combining logic, so the verifier is independent even of that helper's
   arithmetic (a bug in [tightest_oriented]'s candidate/tightest fold does not mirror
   here). Both read the same [Simplex.get_lower]/[get_upper] ground truth; only the
   combinator logic is duplicated, and that is the point. Coverage matches [fixed_bounds]:
   a coeff-1 singleton bounded on its own variable and/or the negated-[-x] slack, plus the
   term const; a non-unit coefficient (e.g. [2x]) with no own slack is out of coverage
   here as in the trigger — see the report's finding-3 follow-up. *)
let oriented_bound_value t (term : Term.t) (which : [ `Lower | `Upper ]) =
  ensure_live t;
  match existing_combo t term with
  | None -> None
  | Some (pairs, const) ->
    let user_lower v =
      match Simplex.get_lower t.simplex v with
      | Some (User tok, d) -> Some (tok, d)
      | Some (Branch _, _) | None -> None
    in
    let user_upper v =
      match Simplex.get_upper t.simplex v with
      | Some (User tok, d) -> Some (tok, d)
      | Some (Branch _, _) | None -> None
    in
    let pos = existing_combo_var t pairs in
    let neg = existing_combo_var t (negate_pairs pairs) in
    let on v f =
      match v with
      | Some id -> f id
      | None -> None
    in
    let flip = function
      | Some (tok, d) -> Some (tok, Delta.neg d)
      | None -> None
    in
    (* [`Upper]: [term <= d] from the combo's own upper OR [-combo >= d'] ⇒ [term <= -d'];
       [`Lower]: symmetric. Take the tighter of the two sources inline (min upper / max
       lower) — no shared [tightest_oriented] fold. *)
    let a, b =
      match which with
      | `Upper -> on pos user_upper, flip (on neg user_lower)
      | `Lower -> on pos user_lower, flip (on neg user_upper)
    in
    let tighter =
      (* On a TIE (equal bound value from the own-variable and the negated-combo slack)
         keep the OWN-VARIABLE token [a] — the neg source [b] is chosen only when strictly
         tighter. This matches the fix-trigger's own tie-break ([tightest_oriented] folds
         with the pos candidate first and a strict [Delta.lt], so a tie keeps pos), so the
         verifier re-derives the SAME token the producer recorded in Γ and [Lit.equal]
         succeeds — otherwise a valid injection is spuriously refused to a fallback split
         (codex-verified tie-break misalignment). *)
      match a, b, which with
      | None, None, _ -> None
      | Some x, None, _ | None, Some x, _ -> Some x
      | Some (ta, da), Some (tb, db), `Upper ->
        if Delta.lt db da then Some (tb, db) else Some (ta, da)
      | Some (ta, da), Some (tb, db), `Lower ->
        if Delta.lt da db then Some (tb, db) else Some (ta, da)
    in
    (match tighter with
     | Some (tok, d) when Delta.is_rational d && Rational.is_int (Delta.c_part d) ->
       Some (tok, Rational.add (Delta.c_part d) const)
     | _ -> None)
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
    (* Arbitrary-precision branch point (ADR-0018): [floor_bigint] never projects to
       int63, so a uint256-range fractional value branches instead of degrading to
       [unknown]. The branch bounds are built as [Bigint] constants
       ([Context.int_const_big]); the term layer is already arbitrary-precision. *)
    let f = Rational.floor_bigint (Delta.c_part d) in
    let fp1 = Oxsmt_core.Bigint.add f Oxsmt_core.Bigint.one in
    let le_atom = Context.le t.ctx term (Context.int_const_big t.ctx f) in
    let ge_atom = Context.ge t.ctx term (Context.int_const_big t.ctx fp1) in
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
  match t.last_cube_model with
  | Some m -> m
  | None -> extract_model t
;;

(* Arbitrary-precision model extraction (ADR-0018): identical to {!extract_model} but the
   integer value is projected via {!Rational.num_bigint}, which never raises on a >int63
   value — so a model assigning a variable a uint256-range value is representable at the
   [Bigint] model sink instead of overflowing to [unknown]. The int-tier drivers (B&B,
   cube) keep {!extract_model}/{!model}; only the model boundary consumed by the
   combinator ({!Lia_adapter.model} -> [Model.Int]) needs the Bigint form. *)
let extract_model_bigint t =
  Dynarray.fold_left
    (fun acc (id, term) ->
      let d = Simplex.value t.simplex id in
      if not (value_is_integer d)
      then failwith "Lia.model_bigint: variable is not integral (call after Int_sat)";
      (term, Rational.num_bigint (Delta.c_part d)) :: acc)
    []
    t.problem_vars
  |> List.sort (fun (a, _) (b, _) -> Int.compare a.Term.tag b.Term.tag)
;;

let model_bigint t =
  ensure_live t;
  match t.last_cube_model with
  (* A cube model's values are the Bromberger–Fleury rounded integers, already native and
     small; widen them exactly. *)
  | Some m -> List.map (fun (term, v) -> term, Oxsmt_core.Bigint.of_int v) m
  | None -> extract_model_bigint t
;;

(* Bromberger-Fleury unit cube test (see {!Simplex.cube_test}): after a {!Sat_candidate}
   whose ℚ-model is not integral, try to satisfy the asserted atoms with an integer model
   directly, before branch-and-bound. On success the model is stashed in [last_cube_model]
   (which {!model} then returns) and [Some] is returned; [None] means fall back to
   {!suggest_branch}. Sound: the point is re-verified feasible by the simplex, and the
   session's R1 model check re-validates it independently — a wrong point degrades to
   [unknown], never a wrong [sat]. The internal cube test push/pops simplex bounds, so
   [check_dirty] is set (mirroring {!solve_integer}) to force the next {!check} to re-run. *)
let cube_model t =
  ensure_live t;
  if t.cube_tried
  then None
  else (
    t.cube_tried <- true;
    t.check_dirty <- true;
    let ids = Dynarray.fold_left (fun acc (id, _term) -> id :: acc) [] t.problem_vars in
    match Simplex.cube_test t.simplex ids with
    | None -> None
    | Some assignment ->
      (try
         let vals = Hashtbl.create (List.length assignment) in
         List.iter (fun (id, r) -> Hashtbl.replace vals id (Rational.num r)) assignment;
         let m =
           Dynarray.fold_left
             (fun acc (id, term) -> (term, Hashtbl.find vals id) :: acc)
             []
             t.problem_vars
           |> List.sort (fun (a, _) (b, _) -> Int.compare a.Term.tag b.Term.tag)
         in
         t.last_cube_model <- Some m;
         Some m
       with
       | Rational.Overflow -> None))
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
           (* f+1 via [Rational]: a branch point at the int boundary degrades to
              [Int_unknown] (the projection raises), not a bogus wrapped bound (codex
              L2/L4/L5 class). *)
           let hi = Delta.of_rat (Rational.add (Rational.of_int f) Rational.one) in
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
  t.report_frames <- [] :: t.report_frames;
  t.eq_frames <- [] :: t.eq_frames
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
      | fs -> fs);
  (* Drop the equalities recorded in the unwound frames (plain frame drop; no per-entry
     undo needed — they carry no simplex state, only the recorded triple). *)
  let rec drop_eq k frames =
    if k = 0
    then frames
    else (
      match frames with
      | _ :: rest -> drop_eq (k - 1) rest
      | [] -> [])
  in
  t.eq_frames
  <- (match drop_eq n t.eq_frames with
      | [] -> [ [] ]
      | fs -> fs)
;;

(* Diagnostics stay readable after poisoning (you need [overflow_count] precisely to
   attribute the brick). *)
let pivot_count t = Simplex.pivot_count t.simplex
let overflow_count t = t.overflows
let is_poisoned t = Simplex.is_poisoned t.simplex
