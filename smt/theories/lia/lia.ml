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

(* Canonical slack definitions are sorted by variable id before lookup. Hash and compare
   that sequence directly, using the value operations for [Rational.t]; never invoke
   polymorphic hash/compare on its mixed representation. A hash collision is only a
   bucket collision: [equal] still distinguishes every variable id and coefficient, so
   distinct linear forms can never share a slack. *)
module Slack_key = struct
  type t = (int * Rational.t) list

  let rec equal a b =
    match a, b with
    | [], [] -> true
    | (va, ca) :: ra, (vb, cb) :: rb ->
      va = vb && Rational.equal ca cb && equal ra rb
    | [], _ :: _ | _ :: _, [] -> false
  ;;

  let hash pairs =
    let avalanche n =
      let n = (n lxor (n lsr 16)) * 0x45d9f3b in
      let n = (n lxor (n lsr 16)) * 0x45d9f3b in
      n lxor (n lsr 16)
    in
    let mix h n = avalanche (h lxor (n + 0x9e3779b9 + (h lsl 6) + (h lsr 2))) in
    List.fold_left
      (fun h (var, coeff) -> mix (mix h var) (Rational.hash coeff))
      0
      pairs
    land max_int
  ;;
end

module Slack_table = Hashtbl.Make (Slack_key)

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
  ; slacks : int Slack_table.t
      (* canonical sorted (varid, coefficient) sequence -> slack id; custom monomorphic
         hash/equality above is value-correct for the mixed-tier [Rational.t]. *)
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
  ; mutable false_frames : 'tok list list
      (* task #78 follow-up: push/pop stack (head = current frame) of premises of asserted
         positive Int equalities that read [Trivially_false] ([0 = k], k <> 0) — an
         UNSATISFIABLE relation preprocessing did not fold (the nec/wisa dense-disequality
         shape, where [?v_i = ?v_j] over [ZERO_ + const] terms cancels the variable to a
         nonzero constant). A non-empty frame makes {!check} report a [Conflict] on that
         premise instead of {!constraints_of_atom} raising [Unsupported] and poisoning the
         whole query to [unknown]. Mirrors [eq_frames]' framing (plain frame-drop on
         [pop]; carries no simplex state). Only ever populated when {!trivial_eq_fix_on};
         stays empty otherwise, so the OFF path is byte-for-byte trunk. *)
  ; mutable cube_tried : bool
  ; mutable gcd_cut_tried : bool
  (* task #128: the multi-row gcd cut runs at most ONCE per instance. The lattice
     infeasibility it tests depends only on the asserted EQUALITY rows (eq_frames), which
     B&B branching never changes (it adds inequality bounds), so re-running it at every
     Final is pure overhead (observed 14x on a big SAT SMPT file). Once per instance,
     exactly like [cube_tried]; the batch reader rejects push/pop so eq_frames is fixed.
     An incremental generation that pushed new equalities would not be re-tested —
     incompleteness, never unsoundness (a missed conflict, not a wrong one); acceptable
     for this dark prototype. *)
  (* the cube test runs at most ONCE per instance — the first non-integral Final, which
     for a batch query is the b&b root (fat feasible regions are cracked there). This
     bounds its extra LP solve to one per query, so it cannot accumulate overhead on a
     file that b&b would otherwise close within the wall (the [cut_lemma] unsat
     regression). *)
  }

let default_budget = 2000

(* task #78 follow-up (verdict-affecting, tri-state, default-ON): a POSITIVE Int equality
   whose two sides differ only by a constant (variables cancel) reads [Trivially_true]
   ([0 = 0]) or [Trivially_false] ([0 = k], k <> 0). Preprocessing is SUPPOSED to fold
   such atoms, but the [?v_i = ?v_j] shape over [ZERO_ + const] terms (nec/wisa) is not
   folded by [Context.eq] (it only folds literal constants) and reaches {!assert_atom},
   where the trunk raises [Unsupported] and poisons the whole query to [unknown] (census
   task #78: QF_LIA/wisa ×5). ON: [Trivially_true] -> no-op (a tautology adds no
   constraint), [Trivially_false] -> a frame-scoped [check] conflict (sound: the literal
   is globally false, so its negation is a valid lemma; R1 remains the backstop). OFF
   ([=0]): the exact trunk raise, byte-for-byte. Read once. *)
let trivial_eq_fix_on =
  match Sys.getenv_opt "OXSMT_LIA_TRIVIAL_EQ" with
  | Some ("0" | "false" | "no") -> false
  | Some _ | None -> true
;;

(* Multi-row integer-elimination gcd cut (task #128). Dark: default OFF, so the extra pass
   in {!diophantine_conflict} is skipped and behaviour is byte-identical to trunk. Set
   OXSMT_LIA_GCD_CUT=1 to eliminate a shared variable across asserted equality rows and
   gcd-test the integer combination — catching a parity/lattice infeasibility (e.g. [x=2q]
   and [x=2q'+1] give [2q-2q'=1], gcd 2 does not divide 1) that the single-row test cannot
   see. *)
let gcd_cut_on =
  match Sys.getenv_opt "OXSMT_LIA_GCD_CUT" with
  | Some ("1" | "true" | "yes" | "on") -> true
  | _ -> false
;;

(* A residual equality row over the still-free integer variables, used by the multi-row
   gcd cut: [gc] is the (var-id, integer-valued coefficient) list, [gr] the residual, [gp]
   the accumulated trail-literal premises of every asserted equality combined into it. *)
type 'tok gcd_row =
  { mutable gc : (int * Rational.t) list
  ; mutable gr : Rational.t
  ; mutable gp : 'tok list
  }

let create ctx =
  { ctx
  ; simplex = Simplex.create ()
  ; var_of_term = Term.Table.create 64
  ; problem_vars = Dynarray.create ()
  ; slacks = Slack_table.create 64
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
  ; false_frames = [ [] ]
  ; cube_tried = false
  ; gcd_cut_tried = false
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

(* Return [pairs] itself when it is already in the canonical order. [combo_of_term]
   visits a normalized arithmetic node's tag-sorted coefficients, and problem variables
   are normally allocated in that same first-use order, so this is the common ingest
   path. The fallback keeps the API's order-independence for equality merges and clients
   that construct terms before asserting them in a different order. *)
let canonical_pairs pairs =
  let rec strictly_increasing previous = function
    | [] -> true
    | (var, _) :: rest -> previous < var && strictly_increasing var rest
  in
  match pairs with
  | [] | [ _ ] -> pairs
  | (var, _) :: rest ->
    if strictly_increasing var rest
    then pairs
    else List.sort (fun (a, _) (b, _) -> Int.compare a b) pairs
;;

module For_testing = struct
  let slack_key_equal a b = Slack_key.equal (canonical_pairs a) (canonical_pairs b)
  let slack_key_hash pairs = Slack_key.hash (canonical_pairs pairs)
end

(* The simplex variable carrying a linear combination, and whether the reported bound is a
   direct problem-var bound. Coeff-1 singletons bound their variable directly (DdM);
   anything else uses a deduplicated slack [s = Σ coeff·x]. *)
let var_for_combo t (pairs : (int * Rational.t) list) =
  match pairs with
  | [ (x, c) ] when Rational.equal c Rational.one -> x
  | _ ->
    (* Sort once for both the dedup key and the simplex row. [Simplex.new_slack]'s
       sorted-input fast path then copies this canonical sequence directly instead of
       sorting it a second time. *)
    let pairs = canonical_pairs pairs in
    (match Slack_table.find_opt t.slacks pairs with
     | Some s -> s
     | None ->
       let s = Simplex.new_slack t.simplex pairs in
       Slack_table.replace t.slacks pairs s;
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
    match (atom : Term.t).node with
    | Eq (a, b) when trivial_eq_fix_on && polarity && not (Sort.equal a.sort Sort.bool) ->
      (* task #78 follow-up: handle a positive Int equality that preprocessing left
         un-folded but whose variables cancel to a constant, INSTEAD of the trunk
         [Unsupported] raise (which poisons the query to [unknown]). *)
      (match equality_reading t a b with
       | Trivially_true -> () (* [0 = 0]: a tautology contributes no LIA constraint *)
       | Trivially_false ->
         (* [0 = k], k <> 0: the asserted equality is globally UNSAT. Record its premise
            so {!check} reports a [Conflict] on it (never silently dropped — that would be
            a wrong-verdict hole). The SAT core then learns the negation and backtracks. *)
         (match t.false_frames with
          | fr :: rest -> t.false_frames <- (premise :: fr) :: rest
          | [] -> t.false_frames <- [ [ premise ] ])
       | Bounds cs -> apply_bounds t cs ~premise)
    | _ -> apply_bounds t (constraints_of_atom t atom ~polarity) ~premise)
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
  (* task #78 follow-up: record a [Trivially_false] merge-surfaced equality as a frame-
     scoped [check] conflict (like {!assert_atom}) instead of raising [Unsupported] and
     poisoning the query. The trunk comment argued the raise was "sound" because
     find_disagreement re-surfaces and splits the pair reaching the SAME raise — but that
     path still degrades to [unknown] (census task #78: nec/wisa merge exactly this
     shape). Recording the conflict is sound (the merged equality is globally false → its
     negation is valid) and complete. OFF ([=0]): the exact trunk raise. *)
  let record_false () =
    match t.false_frames with
    | fr :: rest -> t.false_frames <- (premise :: fr) :: rest
    | [] -> t.false_frames <- [ [ premise ] ]
  in
  match eq.node with
  | Eq (a, b) when not (Sort.equal a.sort Sort.bool) ->
    t.check_dirty <- true;
    guard_overflow t (fun () ->
      match equality_reading t a b with
      | Trivially_true -> () (* [0 = 0] re-notification: no LIA constraint, skip *)
      | Trivially_false ->
        (* [0 = k], k <> 0: unsatisfiable — must NOT be silently dropped (wrong-verdict
           hole). *)
        if trivial_eq_fix_on
        then record_false ()
        else raise (Unsupported "LIA: trivial equality (should be folded)")
      | Bounds cs -> apply_bounds t cs ~premise)
  | Bool_const true ->
    () (* [Context.eq] folded a true equality: tautology, no constraint *)
  | Bool_const false ->
    (* [Context.eq] folded [c1 = c2], c1 <> c2: an unsatisfiable equality. H3 (review
       census-followups): set [check_dirty] only on the ON path — the OFF [raise] poisons
       and discards the instance, so a pre-raise mutation is unobservable, but keeping it
       out of the OFF branch makes [=0] byte-identical to trunk in internal state too. *)
    if trivial_eq_fix_on
    then (
      t.check_dirty <- true;
      record_false ())
    else raise (Unsupported "LIA: trivial equality (should be folded)")
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

(* First recorded [Trivially_false] premise across the live frames, or [None]. A positive
   [Trivially_false] equality is a standalone UNSAT relation independent of the simplex
   (its negation is a valid tautology); it persists until its frame is popped. Non-
   allocating: [None] — the common ON case, and always on the OFF path where
   [false_frames] stays [[[]]] — walks the shallow frame-list spine only. Shared by
   {!check} and {!solve_integer} so both honor the frame-scoped conflict identically. *)
let rec first_false_frame = function
  | [] -> None
  | [] :: rest -> first_false_frame rest
  | (premise :: _) :: _ -> Some premise
;;

let check t =
  ensure_live t;
  (* A cube model is valid only within the single Final->model window that produced it;
     clear it here so a later (non-cube) Sat can never read a stale point. *)
  t.last_cube_model <- None;
  (* task #78 follow-up: a positive [Trivially_false] equality asserted in a still-live
     frame is a standalone UNSAT relation independent of the simplex. Report it as a
     [Conflict] before (and regardless of) the simplex scan; it persists until the frame
     is popped, exactly like a simplex infeasibility. Empty when {!trivial_eq_fix_on} is
     off, so this branch is inert on the OFF path (trunk). *)
  match first_false_frame t.false_frames with
  | Some premise -> Conflict { premises = [ premise ]; farkas = [] }
  | None ->
    (* FIX #3a: skip the simplex feasibility scan when no bound changed since the last
       feasible check. The tableau/assignment the previous [check] certified feasible is
       still current (no assert/pop happened), so returning [Sat_candidate] re-certifies
       the SAME feasible state — never an unrepaired one (the DdM invariants held then and
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
   {!combo_of_term} exactly so the canonical slack-table lookup below hits the SAME key
   the real ingest recorded (a native-int projection here would compute a different key
   and silently miss the slack). *)
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
  | _ -> Slack_table.find_opt t.slacks (canonical_pairs pairs)
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
    (* Multi-row integer-elimination gcd cut (task #128, dark OXSMT_LIA_GCD_CUT). The
       [sweep] fixpoint above is per-row; it misses a lattice infeasibility that only
       appears after ELIMINATING a shared variable between two rows. This pass
       integer-row- reduces the residual free system: for each pivot variable it cancels
       that variable from every other row by the integer combination
       [rowj := ap*rowj - aj*rowp] (ap/aj the pivot/other coefficients; premises unioned)
       and gcd-tests each combined row. All arithmetic is [Rational] so a coefficient
       blow-up raises [Rational.Overflow] and aborts the pass SOUNDLY (no conflict
       claimed) rather than wrapping a native int. Sound: each reduced row is an integer
       linear combination of asserted equalities, so a row with [gcd(coeffs) ∤ residual]
       (or all-zero coeffs and a nonzero residual) is a genuine ℤ-infeasibility of exactly
       the cited premises. Bounded: ≤ #free-vars pivots, each a single linear scan. *)
    let multi_row_gcd_cut () =
      try
        let erows =
          List.filter_map
            (fun row ->
              let residual, free, prems = split_row row in
              if Rational.is_int residual
                 && List.for_all (fun (_, c) -> Rational.is_int c) free
              then Some { gc = free; gr = residual; gp = prems }
              else None)
            rows
          |> Array.of_list
        in
        let n = Array.length erows in
        let coeff r id =
          match List.assoc_opt id r.gc with
          | Some c -> c
          | None -> Rational.zero
        in
        let scale k xs =
          List.filter_map
            (fun (id, c) ->
              let c = Rational.mul k c in
              if Rational.is_zero c then None else Some (id, c))
            xs
        in
        let merge xs ys =
          List.fold_left
            (fun acc (id, c) ->
              match List.assoc_opt id acc with
              | None -> (id, c) :: acc
              | Some c0 ->
                let s = Rational.add c0 c in
                let acc = List.remove_assoc id acc in
                if Rational.is_zero s then acc else (id, s) :: acc)
            xs
            ys
        in
        let test r =
          if !conflict = None
          then (
            match r.gc with
            | [] ->
              if not (Rational.is_zero r.gr)
              then conflict := Some { premises = r.gp; farkas = [] }
            | _ ->
              let g =
                List.fold_left (fun a (_, c) -> gcd_int a (Rational.num c)) 0 r.gc
              in
              if g <> 0 && Rational.num r.gr mod g <> 0
              then conflict := Some { premises = r.gp; farkas = [] })
        in
        let used = Array.make n false in
        let vars =
          let seen = Hashtbl.create 64 in
          let acc = ref [] in
          Array.iter
            (fun r ->
              List.iter
                (fun (id, _) ->
                  if not (Hashtbl.mem seen id)
                  then (
                    Hashtbl.add seen id ();
                    acc := id :: !acc))
                r.gc)
            erows;
          List.rev !acc
        in
        List.iter
          (fun p ->
            if !conflict = None
            then (
              let piv = ref (-1) in
              for i = 0 to n - 1 do
                if !piv < 0
                   && (not used.(i))
                   && not (Rational.is_zero (coeff erows.(i) p))
                then piv := i
              done;
              if !piv >= 0
              then (
                let pv = erows.(!piv) in
                used.(!piv) <- true;
                let ap = coeff pv p in
                for j = 0 to n - 1 do
                  if !conflict = None && j <> !piv
                  then (
                    let aj = coeff erows.(j) p in
                    if not (Rational.is_zero aj)
                    then (
                      let r = erows.(j) in
                      r.gc <- merge (scale ap r.gc) (scale (Rational.neg aj) pv.gc);
                      r.gr <- Rational.sub (Rational.mul ap r.gr) (Rational.mul aj pv.gr);
                      r.gp <- List.rev_append pv.gp r.gp;
                      test r))
                done)))
          vars
      with
      | Exit | Rational.Overflow -> ()
    in
    loop ();
    if !conflict = None && gcd_cut_on && not t.gcd_cut_tried
    then (
      t.gcd_cut_tried <- true;
      multi_row_gcd_cut ());
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

(* Stage B2 HNF tight-constraint integer cut (charter logs/lia-cuts-charter.md, spec
   logs/lia-cuts-b-log.md §5a). Over the TIGHT constraints active at the current LP vertex
   ({!Simplex.tight_rows}) — asserted equalities (fixed vars) AND active one-sided bounds
   — derive a Chvátal–Gomory lattice cut and return it for emission through the
   CONTRACT-LEMMA seam as [Some (cut_atom, antecedent_tokens)]: [cut_atom] is [f·x ≤ k]
   built through the session {!Context}; the tokens are the contributing rows' trail
   literals. This subsumes Stage B's equality-only cut (a fixed var appears as both its ±
   tight bounds) and adds the inequality rows the ring lattice needs. [None] when no cut
   is found, the system exceeds the z3-parity caps, a contributing row is not a real trail
   literal, or the self-check fails (cut-only degradation, never the verdict).

   {b Soundness (self-checked, independent of the HNF kernel).} Every tight row is
   ±-normalized to [gₖ·x ≤ cₖ] (an [Upper] bound as is; a [Lower] bound [def ≥ l] as
   [−def ≤ −l]); a fixed variable contributes BOTH directions. A candidate multiplier
   [μ = ±U[i]/g] (g = gcd(H[i])) is accepted ONLY if [μ ≥ 0 on EVERY row] — the
   Chvátal–Gomory sign condition (a fixed var's ± pair realizes an any-sign net
   multiplier, so this uniform rule is correct for equalities and inequalities alike) —
   AND [f = μ·A] is integer AND [β = μ·c ∉ ℤ]. Then for any feasible integer [x]:
   [f·x = μ·A·x ≤ μ·c = β] (each row [gₖ·x ≤ cₖ] with [μₖ ≥ 0]) and [f·x ∈ ℤ], so
   [f·x ≤ ⌊β⌋] — a T-valid cut; it separates the vertex ([gₖ·x0 = cₖ tight] ⇒
   [f·x0 = β > ⌊β⌋]). The certificate [μ] (sign, integrality, non-integer β) is
   RE-VERIFIED against the ORIGINAL rows before emission; a kernel or assembly bug fails
   the check and the cut is dropped — an unsound cut is never emitted. *)
(* ⌊a/b⌋ with b > 0; [Bigint.divmod] truncates toward zero (the remainder carries the sign
   of [a]), so a negative non-exact quotient is nudged down by one. Shared by the HNF cut
   (B2) and the CG-separation cut (B3). *)
let floor_div a b =
  let q, r = Bigint.divmod a b in
  if Bigint.sign r < 0 then Bigint.sub q Bigint.one else q
;;

(* ⌈a/b⌉ with a ≥ 0 and b > 0 (the minimal nonnegative integer shift, {!cg_cut}). *)
let ceil_div_nonneg a b =
  let q, r = Bigint.divmod a b in
  if Bigint.is_zero r then q else Bigint.add q Bigint.one
;;

(* The tight-constraint system assembled at the current LP vertex, shared by {!hnf_cut}
   (B2) and {!cg_cut} (B3). Each [rows.(i)] is one tight variable, ±-normalized to a
   [≤]-row [(signdef, rhs, restricted, tokens)]: [restricted] marks a genuine one-sided
   inequality (multiplier must be ≥ 0 — Chvátal–Gomory), a fixed variable is a two-sided
   EQUALITY ([restricted = false], any-sign multiplier). [mat_a] (m×n) / [vec_c] (m) are
   the integer matrix/rhs of [signdef]/[rhs] over the compact column index [id_of_col]
   (col -> problem var id); [term_of_id] maps ids back to terms; [hnf] is [U·A = H]. *)
type 'tok tight_system =
  { rows : ((int * Rational.t) list * Rational.t * bool * 'tok list) array
  ; mat_a : Bigint.t array array
  ; vec_c : Bigint.t array
  ; id_of_col : int array
  ; term_of_id : (int, Term.t) Hashtbl.t
  ; hnf : Hnf.t
  }

(* Select a maximal linearly-INDEPENDENT subset of the tight rows, capped at [limit], to keep
   the HNF matrix at z3-parity size and bound its coefficient blow-up. z3 does the same
   ([hnf_cutter::create_cut] shrinks the assembled terms to a rank basis). Because our tight
   set has one row per tight VARIABLE — most of them trivial single-variable bit-range bounds
   — the raw count (m ≈ 100+) dwarfs the rank (≤ #columns). Rows are visited EQUALITY-first,
   then multi-variable before single-variable (the ring lattice lives in the structural
   equality/sum rows; unit bounds only fill leftover rank), and a row is kept iff it raises the
   rank of the chosen set (Gaussian elimination over {!Rational}). Selecting a SUBSET is sound:
   a Chvátal–Gomory cut over any subset of tight rows is valid and separates the same vertex;
   the per-cut self-check re-verifies against the selected rows. [rows.(i)] carries [signdef]
   (over problem-var ids); [col_of] maps an id to its dense column [0, n). *)
let select_independent_rows rows col_of n ~limit =
  let m = Array.length rows in
  (* visitation order: equality rows first, then by descending support size (multi-var
     before unit bounds); a stable order over the original indices for determinism. *)
  let order = Array.init m (fun i -> i) in
  let key i =
    let signdef, _, restricted, _ = rows.(i) in
    (* smaller key = visited earlier: equalities (restricted=false) first, then larger
       |def| *)
    (if restricted then 1 else 0), -List.length signdef
  in
  Array.sort
    (fun a b ->
      let ka = key a
      and kb = key b in
      match compare ka kb with
      | 0 -> compare a b
      | c -> c)
    order;
  (* Pivot rows kept in REDUCED row-echelon form: each [(pivot_col, vec)] has [vec] zero
     at every OTHER pivot's column (a proper RREF, not just forward echelon). This is what
     makes the independence test correct — WITHOUT back-reduction a stale pivot [pv] can
     carry a nonzero at a later pivot's column, so forward-eliminating a candidate
     reintroduces a nonzero at an already-covered pivot column and the "first surviving
     nonzero" scan can mistake a spanned direction for a new one, ACCEPTING a dependent
     row (codex #51 H2). Two invariants maintained on each accept: the new candidate is
     forward-reduced against all existing pivots (zero at their columns), and every
     existing pivot is back-reduced by the new one (zero at its column). Together every
     stored pivot is zero at all other pivot columns, so one forward pass now fully clears
     a candidate at every pivot column and the test is exact. *)
  let pivots = ref [] in
  let selected = ref [] in
  let count = ref 0 in
  let i = ref 0 in
  while !count < limit && !i < m do
    let ri = order.(!i) in
    let signdef, _, _, _ = rows.(ri) in
    let vec = Array.make n Rational.zero in
    List.iter (fun (id, c) -> vec.(Hashtbl.find col_of id) <- c) signdef;
    (* forward-reduce the candidate against every pivot (order-independent under RREF) *)
    List.iter
      (fun (pc, pv) ->
        if not (Rational.is_zero vec.(pc))
        then (
          let factor = Rational.div vec.(pc) pv.(pc) in
          for j = 0 to n - 1 do
            vec.(j) <- Rational.sub vec.(j) (Rational.mul factor pv.(j))
          done))
      !pivots;
    let pc = ref (-1) in
    let j = ref 0 in
    while !pc < 0 && !j < n do
      if not (Rational.is_zero vec.(!j)) then pc := !j;
      incr j
    done;
    if !pc >= 0
    then (
      (* back-reduce existing pivots by the new one, so all pivots stay mutually reduced *)
      let pcv = !pc in
      List.iter
        (fun (_, pv) ->
          if not (Rational.is_zero pv.(pcv))
          then (
            let factor = Rational.div pv.(pcv) vec.(pcv) in
            for j = 0 to n - 1 do
              pv.(j) <- Rational.sub pv.(j) (Rational.mul factor vec.(j))
            done))
        !pivots;
      pivots := (pcv, vec) :: !pivots;
      selected := ri :: !selected;
      incr count);
    incr i
  done;
  let keep = List.sort compare !selected in
  Array.of_list (List.map (fun ri -> rows.(ri)) keep)
;;

(* Assemble the tight system (above) at the current vertex, GROUPED by variable. A
   variable tight on BOTH bounds is FIXED — an EQUALITY row [def = bound] with an
   UNRESTRICTED multiplier (this subsumes Stage B's asserted-equality lattice: an equality
   [a=b] is a fixed slack). A one-sided tight bound is a genuine INEQUALITY, ±-normalized
   to [g·x ≤ c] with a multiplier RESTRICTED to [≥ 0]. A [Branch] reason (Lia's own b&b,
   off the adapter path) or a non-integer coeff drops the whole row's variable soundly.
   [None] when there is no row, no column, or the z3-parity caps
   ([Hnf.max_rows]/[Hnf.max_cols]) are exceeded. *)
let assemble_tight_system ?(max_rows = Hnf.max_rows) ?(select_rank = false) t
  : 'tok tight_system option
  =
  let by_var = Hashtbl.create 64 in
  List.iter
    (fun (r : _ Simplex.tight_row) ->
      let prev =
        try Hashtbl.find by_var r.Simplex.row_var with
        | Not_found -> []
      in
      Hashtbl.replace by_var r.Simplex.row_var (r :: prev))
    (Simplex.tight_rows t.simplex);
  let user_tok (r : _ Simplex.tight_row) =
    match r.Simplex.row_reason with
    | User tok -> Some tok
    | Branch _ -> None
  in
  let rows =
    Hashtbl.fold
      (fun _ rs acc ->
        let r0 = List.hd rs in
        let def = r0.Simplex.row_def
        and bound = r0.Simplex.row_bound in
        let ints =
          List.for_all (fun (_, c) -> Rational.is_int c) def && Rational.is_int bound
        in
        let toks = List.filter_map user_tok rs in
        if (not ints) || List.length toks < List.length rs
        then acc (* non-integer, or a Branch token: drop this variable's row(s) *)
        else (
          let has_lower = List.exists (fun r -> r.Simplex.row_side = `Lower) rs in
          let has_upper = List.exists (fun r -> r.Simplex.row_side = `Upper) rs in
          if has_lower && has_upper
          then (def, bound, false (* equality: unrestricted *), toks) :: acc
          else (
            (* one-sided: ±-normalize to [≤]; multiplier restricted to ≥ 0 *)
            let signdef, rhs =
              match r0.Simplex.row_side with
              | `Upper -> def, bound
              | `Lower ->
                List.map (fun (id, c) -> id, Rational.neg c) def, Rational.neg bound
            in
            (signdef, rhs, true, toks) :: acc)))
      by_var
      []
  in
  let rowsA = Array.of_list rows in
  let m = Array.length rowsA in
  (* compact column index over the problem-var ids appearing in the rows *)
  let col_of = Hashtbl.create 64 in
  Array.iter
    (fun (sd, _, _, _) ->
      List.iter
        (fun (id, _) ->
          if not (Hashtbl.mem col_of id)
          then Hashtbl.replace col_of id (Hashtbl.length col_of))
        sd)
    rowsA;
  let n = Hashtbl.length col_of in
  (* z3-style rank shrink (B3): drop linearly-dependent tight rows so the HNF matrix stays
     at z3-parity size and its coefficients stay bounded (see {!select_independent_rows}). *)
  let rowsA, m =
    if select_rank && n > 0 && m > min n max_rows
    then (
      let r = select_independent_rows rowsA col_of n ~limit:(min n max_rows) in
      r, Array.length r)
    else rowsA, m
  in
  if m = 0 || n = 0 || m > max_rows || n > Hnf.max_cols
  then None
  else (
    let id_of_col = Array.make n 0 in
    Hashtbl.iter (fun id j -> id_of_col.(j) <- id) col_of;
    let term_of_id = Hashtbl.create 64 in
    Dynarray.iter (fun (id, tm) -> Hashtbl.replace term_of_id id tm) t.problem_vars;
    (* integer matrix A (m×n, ±-normalized ≤-rows) and rhs c (m) over Bigint *)
    let mat_a = Array.make_matrix m n Bigint.zero in
    let vec_c = Array.make m Bigint.zero in
    Array.iteri
      (fun i (sd, rhs, _, _) ->
        List.iter
          (fun (id, cf) -> mat_a.(i).(Hashtbl.find col_of id) <- Rational.num_bigint cf)
          sd;
        vec_c.(i) <- Rational.num_bigint rhs)
      rowsA;
    let hnf = Hnf.compute mat_a in
    Some { rows = rowsA; mat_a; vec_c; id_of_col; term_of_id; hnf })
;;

let hnf_cut t : (Term.t * 'tok list) option =
  ensure_live t;
  if Simplex.is_poisoned t.simplex
  then None
  else (
    match assemble_tight_system t with
    | None -> None
    | Some { rows = rowsA; mat_a; vec_c; id_of_col; term_of_id; hnf } ->
      let m = Array.length rowsA
      and n = Array.length id_of_col in
      let u = hnf.Hnf.u
      and h = hnf.Hnf.h in
      (* Try candidate multiplier [μ = sign·U[i] / g]. Accept iff (SIGN) [μₖ ≥ 0] on every
         RESTRICTED (inequality) row — equality rows are unrestricted, (INT) [f = μ·A]
         integer (= sign·H[i]/g), and (SEP) [β = μ·c ∉ ℤ] — all recomputed from the
         ORIGINAL A/c. Returns the emitted cut or [None]. *)
      let try_candidate i g sign =
        let ok = ref true in
        (* SIGN: on every INEQUALITY row, sign·U[i][k] ≥ 0 (Chvátal–Gomory) *)
        for k = 0 to m - 1 do
          let _, _, restricted, _ = rowsA.(k) in
          if restricted && Bigint.sign u.(i).(k) * sign < 0 then ok := false
        done;
        if not !ok
        then None
        else (
          let f = Array.make n Bigint.zero in
          for j = 0 to n - 1 do
            let s = ref Bigint.zero in
            for k = 0 to m - 1 do
              s := Bigint.add !s (Bigint.mul u.(i).(k) mat_a.(k).(j))
            done;
            let s = if sign < 0 then Bigint.neg !s else !s in
            (* kernel-consistency tripwire: sign·U·A must equal sign·H for this row *)
            let hij = if sign < 0 then Bigint.neg h.(i).(j) else h.(i).(j) in
            if not (Bigint.equal s hij) then ok := false;
            let qj, rj = Bigint.divmod s g in
            if Bigint.is_zero rj then f.(j) <- qj else ok := false
          done;
          (* β = (sign·U·c)/g, recomputed from the ORIGINAL c, must be NON-integer *)
          let num_beta =
            let s = ref Bigint.zero in
            for k = 0 to m - 1 do
              s := Bigint.add !s (Bigint.mul u.(i).(k) vec_c.(k))
            done;
            if sign < 0 then Bigint.neg !s else !s
          in
          let _, rb = Bigint.divmod num_beta g in
          if Bigint.is_zero rb then ok := false;
          if not !ok
          then None
          else (
            let coeffs =
              let acc = ref [] in
              for j = n - 1 downto 0 do
                if not (Bigint.is_zero f.(j))
                then acc := (f.(j), Hashtbl.find term_of_id id_of_col.(j)) :: !acc
              done;
              !acc
            in
            match coeffs with
            | [] -> None (* constant cut: never emit *)
            | _ ->
              let k_bound = floor_div num_beta g in
              let pol = Context.linear_combination_big t.ctx coeffs Bigint.zero in
              let cut_atom = Context.le t.ctx pol (Context.int_const_big t.ctx k_bound) in
              let ants =
                let acc = ref [] in
                for k = m - 1 downto 0 do
                  if not (Bigint.is_zero u.(i).(k))
                  then (
                    let _, _, _, toks = rowsA.(k) in
                    acc := List.rev_append (List.rev toks) !acc)
                done;
                !acc
              in
              Some (cut_atom, ants)))
      in
      let result = ref None in
      let i = ref 0 in
      while Option.is_none !result && !i < m do
        let g = Array.fold_left Bigint.gcd Bigint.zero h.(!i) in
        if not (Bigint.is_zero g)
        then (
          match try_candidate !i g 1 with
          | Some _ as r -> result := r
          | None ->
            (match try_candidate !i g (-1) with
             | Some _ as r -> result := r
             | None -> ()));
        incr i
      done;
      !result)
;;

(* Stage B3 Chvátal–Gomory SEPARATION cut (charter logs/lia-cuts-charter.md; the rings
   prize, logs/lia-cuts-b2-log.md §next rung). Same tight system as {!hnf_cut}, same
   emission contract ([Some (cut_atom, antecedent_tokens)] through the CONTRACT-LEMMA
   seam), but where B2 REJECTS an HNF-row multiplier that is negative on some inequality
   row, B3 SEARCHES for a sign-valid multiplier by shifting it into the tight cone —
   cracking cuts B2 could not emit.

   {b Mechanism.} For HNF row [i] and sign [±1], the base integer weight is
   [w = sign·U[i]] and [g = gcd(H[i]) > 0]; the base multiplier [μ = w/g] gives
   [f = μ·A = sign·H[i]/g] (integer) and [β = μ·c = sign·(U[i]·c)/g]. Fractionality of [β]
   (the separation source) is decided by [w·c mod g] and is invariant under sign and the
   shift below, so rows with integer [β] are skipped. B3 adds the MINIMAL NONNEGATIVE
   INTEGER shift [z] on the restricted rows ([z_k = ⌈−w_k/g⌉] where [w_k < 0], else 0),
   forming the integer weight [W = w + g·z] with [W_k ≥ 0] on every restricted row. The
   realized multiplier is [μ' = W/g].

   {b Soundness (self-checked, independent of the HNF kernel).} [z] is integer and [A],
   [c] are integer, so [f' = μ'·A = (W·A)/g = f + z·A] stays integer and
   [β' = μ'·c = (W·c)/g = β + z·c] keeps the SAME fractional part as [β] ([z·c ∈ ℤ]).
   Every tight row is [gₖ·x ≤ cₖ] (equalities as [=]); with [μ'ₖ ≥ 0] on the inequalities
   (equality rows unrestricted), for any feasible integer [x]: [f'·x = μ'·A·x ≤ μ'·c = β']
   and [f'·x ∈ ℤ], so [f'·x ≤ ⌊β'⌋] — a T-valid Chvátal–Gomory cut over a MULTI-ROW
   combination. It separates the vertex: every tight row is [=] at [x0], so
   [f'·x0 = β' > ⌊β'⌋]. All of [W_k ≥ 0] (restricted), [f' = W·A/g] exactly divisible, and
   [β' = W·c/g ∉ ℤ] are RE-VERIFIED against the ORIGINAL A/c before emission; any failure
   drops the cut (never emit unsound). Among all (row, sign) candidates the
   smallest-[‖f'‖₁] cut is emitted (a tighter, lower-churn combination). [None] when no
   fractional row yields a cut, the caps are exceeded, or a contributing row is not a real
   trail literal.

   NOTE (env scope): [cg_cut] is env-INDEPENDENT public API -- the [OXSMT_CG_CUTS] gating
   lives in the adapter ([Lia_adapter], guarded by [cg_cuts_on]; default-ON since #68),
   NOT here. The forced-OFF ([OXSMT_CG_CUTS=0], [false], or [no]) byte-identity is
   therefore scoped to the shipped SOLVE PATH (which never reaches this function when the
   flag is off); a direct API caller invoking [cg_cut] regardless still gets the
   rank-selection behaviour. The optional [cut_gate] (task #60) filters the selected best
   cut; default always-emit. *)
let cg_cut ?(cut_gate = fun ~nnz:_ ~ants:_ ~m:_ ~n:_ -> true) t
  : (Term.t * 'tok list) option
  =
  ensure_live t;
  if Simplex.is_poisoned t.simplex
  then None
  else (
    match assemble_tight_system ~select_rank:true t with
    | None -> None
    | Some { rows = rowsA; mat_a; vec_c; id_of_col; term_of_id; hnf } ->
      let m = Array.length rowsA
      and n = Array.length id_of_col in
      let u = hnf.Hnf.u
      and h = hnf.Hnf.h in
      (* Candidate from HNF row [i], sign [±1], pivot gcd [g > 0]. Returns
         [(coeffs, k_bound, bigW, l1)] where [bigW = W] (for the antecedent support) and
         [l1 = ‖f'‖₁] (for selection), or [None] if this row gives no separating cut. *)
      let try_cg i g sign =
        let w = Array.make m Bigint.zero in
        for k = 0 to m - 1 do
          w.(k) <- (if sign < 0 then Bigint.neg u.(i).(k) else u.(i).(k))
        done;
        (* β numerator (base): Σ wₖ cₖ. Fractionality is sign/shift invariant, so gate
           here. *)
        let base_beta_num = ref Bigint.zero in
        for k = 0 to m - 1 do
          base_beta_num := Bigint.add !base_beta_num (Bigint.mul w.(k) vec_c.(k))
        done;
        let _, rb0 = Bigint.divmod !base_beta_num g in
        if Bigint.is_zero rb0
        then None (* integer β: no separating cut from this row *)
        else (
          let ok = ref true in
          (* minimal nonneg integer shift on restricted rows: W = w + g·z, Wₖ ≥ 0 there *)
          let bigW = Array.make m Bigint.zero in
          for k = 0 to m - 1 do
            let _, _, restricted, _ = rowsA.(k) in
            if restricted && Bigint.sign w.(k) < 0
            then (
              let z = ceil_div_nonneg (Bigint.neg w.(k)) g in
              bigW.(k) <- Bigint.add w.(k) (Bigint.mul g z))
            else bigW.(k) <- w.(k);
            (* SIGN tripwire (re-verified): nonneg multiplier on every inequality row *)
            if restricted && Bigint.sign bigW.(k) < 0 then ok := false
          done;
          if not !ok
          then None
          else (
            (* f'[j] = (Σₖ Wₖ·A[k][j]) / g, recomputed from A; require exact divisibility *)
            let f = Array.make n Bigint.zero in
            for j = 0 to n - 1 do
              let s = ref Bigint.zero in
              for k = 0 to m - 1 do
                s := Bigint.add !s (Bigint.mul bigW.(k) mat_a.(k).(j))
              done;
              let qj, rj = Bigint.divmod !s g in
              if Bigint.is_zero rj then f.(j) <- qj else ok := false
            done;
            (* β' = (Σₖ Wₖ·cₖ)/g, recomputed from c, must be NON-integer (separation) *)
            let beta_num = ref Bigint.zero in
            for k = 0 to m - 1 do
              beta_num := Bigint.add !beta_num (Bigint.mul bigW.(k) vec_c.(k))
            done;
            let _, rb = Bigint.divmod !beta_num g in
            if Bigint.is_zero rb then ok := false;
            if not !ok
            then None
            else (
              let coeffs =
                let acc = ref [] in
                for j = n - 1 downto 0 do
                  if not (Bigint.is_zero f.(j))
                  then acc := (f.(j), Hashtbl.find term_of_id id_of_col.(j)) :: !acc
                done;
                !acc
              in
              match coeffs with
              | [] -> None (* constant cut: never emit (parity with {!hnf_cut}) *)
              | _ ->
                let l1 =
                  Array.fold_left (fun a x -> Bigint.add a (Bigint.abs x)) Bigint.zero f
                in
                let k_bound = floor_div !beta_num g in
                Some (coeffs, k_bound, bigW, l1))))
      in
      (* scan every HNF row and both signs; keep the smallest-‖f'‖₁ separating cut — a
         tighter, lower-churn cut cracks the lattice in fewer rounds (the HNF compute, not
         this scan, dominates a cut call, so scanning for a better cut pays for itself). *)
      let best = ref None in
      for i = 0 to m - 1 do
        let g = Array.fold_left Bigint.gcd Bigint.zero h.(i) in
        if not (Bigint.is_zero g)
        then
          List.iter
            (fun sign ->
              match try_cg i g sign with
              | None -> ()
              | Some (_, _, _, l1) as cand ->
                let better =
                  match !best with
                  | None -> true
                  | Some (_, _, _, bl1) -> Bigint.compare l1 bl1 < 0
                in
                if better then best := cand)
            [ 1; -1 ]
      done;
      (match !best with
       | None -> None
       | Some (coeffs, k_bound, bigW, _l1) ->
         let nnz = List.length coeffs in
         (* antecedent support size: # of tight rows with a nonzero multiplier W_k. A cut
            that combines ALL rows ([ant_count = m]) or most coefficients ([nnz] near [n])
            is a DENSE/global cut — task #60 measured these as the unproductive
            search-lengthening cuts (cut_lemmas), whereas the productive ring cuts are
            sparse (few rows, few coeffs). *)
         let ant_count =
           Array.fold_left (fun a w -> if Bigint.is_zero w then a else a + 1) 0 bigW
         in
         (* SPARSITY GATE (task #60): a caller-supplied predicate decides whether this
            best-candidate cut is worth emitting. The default (no [cut_gate] arg) always
            emits — VERDICT+SEARCH-identical to the pre-policy behaviour and to every
            existing caller / unit test (not allocation-identical: the [nnz]/[ant_count]
            support scan and the always-true gate callback still run). The adapter
            supplies the density policy when CG cuts are on. Rejecting a cut here makes
            {!hnf_lemma} return [None], so the adapter falls back to the B&B branch — a
            strictly weaker action, so soundness is unaffected (the gate can only forgo an
            optimisation, never change a verdict). *)
         if not (cut_gate ~nnz ~ants:ant_count ~m ~n)
         then None
         else (
           let pol = Context.linear_combination_big t.ctx coeffs Bigint.zero in
           let cut_atom = Context.le t.ctx pol (Context.int_const_big t.ctx k_bound) in
           let ants =
             let acc = ref [] in
             for k = m - 1 downto 0 do
               if not (Bigint.is_zero bigW.(k))
               then (
                 let _, _, _, toks = rowsA.(k) in
                 acc := List.rev_append (List.rev toks) !acc)
             done;
             !acc
           in
           Some (cut_atom, ants))))
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
  (* H2 (review census-followups): honor a frame-scoped [Trivially_false] equality here
     too, symmetric with {!check}. Such an equality records a [false_frames] premise but
     adds no simplex bound, so a simplex-only scan below would return a wrong [Int_sat].
     This driver is product-unused today (lia_adapter.mli: the live path calls {!check}
     first), but the guard removes the latent trap for any future caller. Empty on the OFF
     path (trunk). *)
  match first_false_frame t.false_frames with
  | Some premise -> Int_unsat (Some { premises = [ premise ]; farkas = [] })
  | None ->
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
       small, non-adversarial inputs. Raising is sound; here — the complete decision
       driver — we degrade that to [Int_unknown] and count it as a distinct stat so a
       benchmark pass-rate gap is attributable, not a mystery. The fix is
       arbitrary-precision rationals (tracked as the core-bignum row, post-M4). *)
    (match dfs ~depth0:true with
     | `Sat m -> Int_sat m
     | `Unknown -> Int_unknown
     | `Unsat -> Int_unsat !root_conflict
     | exception Rational.Overflow ->
       (* Poison regardless of where the overflow arose (mid-pivot via Simplex.guarded, or
          a branch-point iadd here): the instance is not safe to reuse. *)
       Simplex.poison t.simplex;
       t.overflows <- t.overflows + 1;
       Int_unknown)
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
  t.eq_frames <- [] :: t.eq_frames;
  t.false_frames <- [] :: t.false_frames
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
      | fs -> fs);
  (* task #78 follow-up: drop the [Trivially_false] premises recorded in the unwound
     frames (same plain frame-drop as [eq_frames]; no simplex state). This is what
     retracts the [check] conflict once the SAT core backtracks past the frame that
     asserted the unsatisfiable equality. Inert (always [[]]) on the OFF path. *)
  t.false_frames
  <- (match drop_eq n t.false_frames with
      | [] -> [ [] ]
      | fs -> fs)
;;

(* ADR-0014 Stage 4.2 sub-frame checkpoint/rewind. Simplex bounds ride the Simplex trail;
   the [reported]/[eq] bookkeeping is framed as in [pop]. The chrono checkpoint-driver
   holds the theory at a SINGLE base frame (no per-decision-level frames under CB), so
   both frame lists have exactly one frame here. [checkpoint] captures the simplex
   watermark + the base frame's reported/eq/false counts; [rewind_to_checkpoint] restores
   the simplex bounds and drops the newest reported/eq/false entries recorded since the
   checkpoint — un-reporting + re-dirtying each reported atom exactly as [pop] does
   (CONTRACT-EX), addressed by an absolute count rather than a frame boundary. Fails LOUD
   if a non-base frame is open, rather than silently mis-restoring.

   H6: [false_frames] (the [Trivially_false] premises recorded under the default-ON
   [trivial_eq_fix], task #78) is backtrackable — [pop] drops it in lockstep with
   [eq_frames] — so it MUST ride the checkpoint too. Omitting it (the foundation predated
   [false_frames] and auto-merged over it) is completeness-only — a stale
   tautologically-false premise can only over-report (fail-closed unknown / redundant
   valid lemma), never a wrong verdict — but it breaks the primitive's OBS-EQ contract, so
   it is framed identically to [eq_frames] below. *)
type checkpoint =
  { c_simplex : int
  ; c_reported : int
  ; c_eq : int
  ; c_false : int
  }

let single_base_frame = function
  | [ fr ] -> fr
  | _ ->
    failwith
      "Lia checkpoint/rewind: expected a single base frame (S4.2 CB checkpoint-driver \
       invariant)"
;;

let checkpoint t =
  { c_simplex = Simplex.checkpoint t.simplex
  ; c_reported = List.length (single_base_frame t.report_frames)
  ; c_eq = List.length (single_base_frame t.eq_frames)
  ; c_false = List.length (single_base_frame t.false_frames)
  }
;;

let rewind_to_checkpoint t c =
  ensure_live t;
  Simplex.rewind_to_checkpoint t.simplex c.c_simplex;
  t.check_dirty <- true;
  let fr = single_base_frame t.report_frames in
  let rec drop_reported k fr =
    if k <= 0
    then fr
    else (
      match fr with
      | [] -> []
      | i :: tl ->
        Dynarray.set t.reported i false;
        Hashtbl.replace t.dirty (Dynarray.get t.registered i).var ();
        drop_reported (k - 1) tl)
  in
  t.report_frames <- [ drop_reported (List.length fr - c.c_reported) fr ];
  let efr = single_base_frame t.eq_frames in
  let rec drop_first k l =
    if k <= 0
    then l
    else (
      match l with
      | [] -> []
      | _ :: tl -> drop_first (k - 1) tl)
  in
  t.eq_frames <- [ drop_first (List.length efr - c.c_eq) efr ];
  (* H6: drop the [Trivially_false] premises recorded since the checkpoint, exactly as
     [pop] retracts them (lia.ml [pop] drops [false_frames] in lockstep with [eq_frames]).
     Same shape (a single base [_ list]), same newest-first absolute-count drop as
     [eq_frames]. *)
  let ffr = single_base_frame t.false_frames in
  t.false_frames <- [ drop_first (List.length ffr - c.c_false) ffr ]
;;

(* Diagnostics stay readable after poisoning (you need [overflow_count] precisely to
   attribute the brick). *)
let pivot_count t = Simplex.pivot_count t.simplex
let overflow_count t = t.overflows
let is_poisoned t = Simplex.is_poisoned t.simplex
