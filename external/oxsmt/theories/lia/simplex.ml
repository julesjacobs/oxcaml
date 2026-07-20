(* Incremental general simplex over δ-rationals. See simplex.mli.

   Following Dutertre-de Moura, "A Fast Linear-Arithmetic Solver for DPLL(T)" (2006):
   AssertUpper/AssertLower (§5), Check with Bland's rule, PivotAndUpdate + Pivot.
   Invariants maintained at all times (DdM06): (INV-EQ) every basic variable equals Σ (row
   coeff · nonbasic value); (INV-NB) every nonbasic variable is within its bounds; Check
   restores (INV-EQ)+(INV-NB) for the basic variables too, or reports a conflict. *)

exception Farkas_error of string

module IntSet = Set.Make (Int)

(* A linear expression: var id -> nonzero coefficient (absent = 0). Used both for a
   variable's immutable Farkas half-plane basis ([def], over problem vars) and for a basic
   variable's mutable tableau [row] (over current nonbasic vars).

   Representation: a SPARSE SORTED, growable pair of parallel arrays — [ids.(0..len-1)]
   strictly ascending, [cs.(0..len-1)] the matching nonzero coefficients (no explicit
   zero), with [len <= Array.length ids = Array.length cs] (the tail is spare capacity).
   This is the same abstract var->coeff map the former [Rational.t Map.Make(Int).t] was —
   coeff lookup is a binary search and every iterator ([fold]/[iter]/[bindings]) yields
   entries in ASCENDING id order, exactly as [Map.Make(Int)] did — so verdicts, counters
   and Farkas certificates are byte-identical, and the two order-sensitive consumers
   ([entering] = Bland's smallest-id rule; [conflict_of]/[build_conflict] = the Farkas
   premise/multiplier order) are unchanged.

   Why a MUTABLE row (this lane): a basic variable's [row] is updated incrementally on
   every pivot ([add_scaled_in_place] merge, [remove_in_place]). A persistent Map
   allocates O(log n) tree nodes per update; a fresh sorted array allocates O(n) per
   update (the sparse-array park's regression on wide rows). Here the hot incremental ops
   mutate the row IN PLACE into its own backing arrays (growing capacity only when it
   must), so an incremental update allocates nothing beyond an occasional doubling.

   Ownership discipline (soundness-critical — an in-place write to a shared row would
   corrupt the tableau and mis-verdict): the ONLY mutated destinations are (a) a var's own
   [row], which is uniquely owned by that var, and (b) a fresh accumulator created by
   [expand]/[build_conflict]. A var's immutable [def] and any row read as the SCALED
   SOURCE of [add_scaled_in_place] (e.g. [nj.row] during pivot substitution) are only ever
   read. Rows are never trailed (they are recomputed by [check] after [pop]), so in-place
   mutation is backtrack-safe. The functional constructors ([singleton]/[of_list]/[map]/
   [add]/[remove]) still allocate a fresh row and are used where a fresh value is wanted
   (a var's [def], the pivot's [new_row]); [add_scaled_in_place]/[remove_in_place] are the
   in-place ops used on the incremental hot path. *)
module Lx = struct
  (* INVARIANT: [ids.(0..len-1)] strictly ascending; [cs.(0..len-1)] all nonzero;
     [len <= Array.length ids = Array.length cs]. *)
  type t =
    { mutable ids : int array
    ; mutable cs : Rational.t array
    ; mutable len : int
    }

  let create () = { ids = [||]; cs = [||]; len = 0 }

  (* precondition: [c] nonzero (callers pass Rational.one / a checked coeff). *)
  let singleton i c = { ids = [| i |]; cs = [| c |]; len = 1 }

  let copy (m : t) =
    { ids = Array.sub m.ids 0 m.len
    ; cs = Array.sub m.cs 0 m.len
    ; len = m.len
    }
  ;;

  (* Index of [i] in [ids.(0..len-1)], or -1. *)
  let find_idx (m : t) i =
    let lo = ref 0
    and hi = ref (m.len - 1)
    and res = ref (-1) in
    while !lo <= !hi do
      let mid = (!lo + !hi) / 2 in
      let v = m.ids.(mid) in
      if v = i
      then (
        res := mid;
        lo := !hi + 1)
      else if v < i
      then lo := mid + 1
      else hi := mid - 1
    done;
    !res
  ;;

  let coeff (m : t) i =
    let k = find_idx m i in
    if k < 0 then Rational.zero else m.cs.(k)
  ;;

  (* Ascending-id fold/iter/bindings — identical order to Map.Make(Int). *)
  let fold f (m : t) init =
    let acc = ref init in
    for k = 0 to m.len - 1 do
      acc := f m.ids.(k) m.cs.(k) !acc
    done;
    !acc
  ;;

  let iter f (m : t) =
    for k = 0 to m.len - 1 do
      f m.ids.(k) m.cs.(k)
    done
  ;;

  let bindings (m : t) = List.init m.len (fun k -> m.ids.(k), m.cs.(k))

  let for_all p (m : t) =
    let ok = ref true
    and k = ref 0 in
    while !ok && !k < m.len do
      if not (p m.ids.(!k) m.cs.(!k)) then ok := false;
      incr k
    done;
    !ok
  ;;

  (* Grow [m]'s backing arrays so its capacity is at least [n] (preserving the live
     prefix). Occasional doubling; the incremental ops call this instead of allocating a
     fresh array per update. *)
  let ensure_cap (m : t) n =
    if Array.length m.ids < n
    then (
      let ncap = max n (max 4 (2 * Array.length m.ids)) in
      let nids = Array.make ncap 0
      and ncs = Array.make ncap Rational.zero in
      Array.blit m.ids 0 nids 0 m.len;
      Array.blit m.cs 0 ncs 0 m.len;
      m.ids <- nids;
      m.cs <- ncs)
  ;;

  (* FUNCTIONAL map of coefficients (keys unchanged), dropping any coefficient [f] sends
     to zero so the no-explicit-zero invariant holds. Returns a FRESH row. *)
  let map f (m : t) =
    let rids = Array.make m.len 0
    and rcs = Array.make m.len Rational.zero in
    let w = ref 0 in
    for k = 0 to m.len - 1 do
      let c = f m.cs.(k) in
      if not (Rational.is_zero c)
      then (
        rids.(!w) <- m.ids.(k);
        rcs.(!w) <- c;
        incr w)
    done;
    { ids = rids; cs = rcs; len = !w }
  ;;

  (* FUNCTIONAL insert/replace of [(i,c)]; a zero [c] removes [i]. Returns a FRESH row
     (O(n) splice). Used off the hot path (the pivot's [new_row]). *)
  let add i c (m : t) =
    let k = find_idx m i in
    if Rational.is_zero c
    then
      if k < 0
      then { ids = Array.sub m.ids 0 m.len; cs = Array.sub m.cs 0 m.len; len = m.len }
      else (
        let rids = Array.make (m.len - 1) 0
        and rcs = Array.make (m.len - 1) Rational.zero in
        Array.blit m.ids 0 rids 0 k;
        Array.blit m.cs 0 rcs 0 k;
        Array.blit m.ids (k + 1) rids k (m.len - k - 1);
        Array.blit m.cs (k + 1) rcs k (m.len - k - 1);
        { ids = rids; cs = rcs; len = m.len - 1 })
    else if k >= 0
    then (
      let rids = Array.sub m.ids 0 m.len
      and rcs = Array.sub m.cs 0 m.len in
      rcs.(k) <- c;
      { ids = rids; cs = rcs; len = m.len })
    else (
      let p = ref 0 in
      while !p < m.len && m.ids.(!p) < i do
        incr p
      done;
      let p = !p in
      let rids = Array.make (m.len + 1) 0
      and rcs = Array.make (m.len + 1) Rational.zero in
      Array.blit m.ids 0 rids 0 p;
      Array.blit m.cs 0 rcs 0 p;
      rids.(p) <- i;
      rcs.(p) <- c;
      Array.blit m.ids p rids (p + 1) (m.len - p);
      Array.blit m.cs p rcs (p + 1) (m.len - p);
      { ids = rids; cs = rcs; len = m.len + 1 })
  ;;

  let remove i (m : t) = add i Rational.zero m

  (* Copy an already canonical input directly. The LIA ingest path has already sorted the
     list to build its slack-dedup key, so sorting again here used to dominate
     registration on wide linear atoms. *)
  let of_sorted_unique pairs =
    let len = List.length pairs in
    let ids = Array.make len 0
    and cs = Array.make len Rational.zero in
    List.iteri
      (fun k (id, coeff) ->
        ids.(k) <- id;
        cs.(k) <- coeff)
      pairs;
    { ids; cs; len }
  ;;

  let rec is_sorted_unique_nonzero previous = function
    | [] -> true
    | (id, coeff) :: rest ->
      previous < id
      && not (Rational.is_zero coeff)
      && is_sorted_unique_nonzero id rest
  ;;

  (* Build from (id,coeff) pairs: on the canonical input supplied by [Lia], copy directly;
     otherwise sum repeated ids, drop zeros, and sort. Same abstract map (and same summed
     coefficient values) as folding [add]/[remove] over the list. *)
  let of_list pairs =
    match pairs with
    | [] -> create ()
    | (first_id, first_coeff) :: rest
      when not (Rational.is_zero first_coeff)
           && is_sorted_unique_nonzero first_id rest ->
      of_sorted_unique pairs
    | _ ->
      let sorted = List.stable_sort (fun (a, _) (b, _) -> Int.compare a b) pairs in
      let rids = ref []
      and rcs = ref [] in
      let cur_id = ref 0
      and cur_c = ref Rational.zero
      and have = ref false in
      let flush () =
        if !have && not (Rational.is_zero !cur_c)
        then (
          rids := !cur_id :: !rids;
          rcs := !cur_c :: !rcs)
      in
      List.iter
        (fun (i, c) ->
          if !have && i = !cur_id
          then cur_c := Rational.add !cur_c c
          else (
            flush ();
            cur_id := i;
            cur_c := c;
            have := true))
        sorted;
      flush ();
      let ids = Array.of_list (List.rev !rids)
      and cs = Array.of_list (List.rev !rcs) in
      { ids; cs; len = Array.length ids }
  ;;

  (* Reusable scratch for the [add_scaled_in_place] merge. A single global buffer is safe:
     the solver is single-threaded and [add_scaled_in_place] never re-enters itself, so no
     two merges are ever live at once. Sized on demand; never shrinks. *)
  let scratch_ids = ref [||]
  let scratch_cs = ref [||]

  let ensure_scratch n =
    if Array.length !scratch_ids < n
    then (
      scratch_ids := Array.make n 0;
      scratch_cs := Array.make n Rational.zero)
  ;;

  (* IN-PLACE [dst := dst + s·m], dropping resulting zeros. [dst] is mutated (must be
     uniquely owned); [m] is read-only (and a distinct object from [dst]). A two-pointer
     merge of the two sorted rows into the reusable scratch, then a copy back into [dst]'s
     (grown-if-needed) backing — so no per-call array allocation beyond occasional growth.

     Value contract, matching the former Map fold over [m] EXACTLY (so counters/certs are
     byte-identical): an id only in [dst] is carried unchanged; an id only in [m] becomes
     [Rational.add Rational.zero (Rational.mul s c)]; an id in both becomes
     [Rational.add dst_c (Rational.mul s c)]; a zero result is dropped. *)
  let add_scaled_in_place (dst : t) (s : Rational.t) (m : t) =
    let cap = dst.len + m.len in
    ensure_scratch cap;
    let sids = !scratch_ids
    and scs = !scratch_cs in
    let w = ref 0
    and ia = ref 0
    and im = ref 0 in
    let emit id c =
      if not (Rational.is_zero c)
      then (
        sids.(!w) <- id;
        scs.(!w) <- c;
        incr w)
    in
    while !ia < dst.len || !im < m.len do
      if !im >= m.len || (!ia < dst.len && dst.ids.(!ia) < m.ids.(!im))
      then (
        emit dst.ids.(!ia) dst.cs.(!ia);
        incr ia)
      else if !ia >= dst.len || m.ids.(!im) < dst.ids.(!ia)
      then (
        emit m.ids.(!im) (Rational.add Rational.zero (Rational.mul s m.cs.(!im)));
        incr im)
      else (
        emit dst.ids.(!ia) (Rational.add dst.cs.(!ia) (Rational.mul s m.cs.(!im)));
        incr ia;
        incr im)
    done;
    ensure_cap dst !w;
    Array.blit sids 0 dst.ids 0 !w;
    Array.blit scs 0 dst.cs 0 !w;
    dst.len <- !w
  ;;

  (* IN-PLACE removal of id [i] from [m] (a no-op if absent). [m] uniquely owned. The
     overlapping left-shift is a single [Array.blit] (defined for overlapping ranges). *)
  let remove_in_place (m : t) i =
    let k = find_idx m i in
    if k >= 0
    then (
      Array.blit m.ids (k + 1) m.ids k (m.len - k - 1);
      Array.blit m.cs (k + 1) m.cs k (m.len - k - 1);
      m.len <- m.len - 1)
  ;;
end

type linexp = Lx.t

let coeff = Lx.coeff

type 'a bound =
  { bval : Delta.t
  ; reason : 'a
  }

type 'a var =
  { id : int
  ; def : linexp (* immutable, over problem-variable ids: the Farkas half-plane basis *)
  ; mutable value : Delta.t
  ; mutable lower : 'a bound option
  ; mutable upper : 'a bound option
  ; mutable basic : bool
  ; mutable row : linexp (* meaningful iff [basic]; over current nonbasic ids *)
  }

(* Undo records for push/pop: only bounds change observably across frames. *)
type 'a undo =
  | Undo_lower of int * 'a bound option
  | Undo_upper of int * 'a bound option

type 'a t =
  { vars : 'a var Dynarray.t
  ; (* Bound changes are the only observably backtrackable state (the tableau and
       assignment are recomputed by [check], never restored). The shared substrate owns
       the frame stack + newest-first drain (ADR-0014 Stage 0); this site carries no
       per-frame payload beyond the trail watermark. *)
    trail : ('a undo, unit) Oxsmt_core.Trail.t
  ; mutable pivots : int
  ; mutable dirty_basic : IntSet.t
      (* FIX #3b: a SUPERSET of the basic variables that may violate a bound — every basic
         var whose value changed ([update]/[pivot_and_update]) or whose bound tightened
         ([assert_*]) since [check] last restored feasibility. [first_violating] scans
         only this instead of all vars, pruning non-violating members as it goes. Superset
         invariant: any violating basic var is present, so [check] can never miss a
         violation and certify Sat on an unrepaired tableau. [pop] only loosens bounds
         (never creates a violation), so it needs no maintenance. *)
  ; mutable dirty_bound : IntSet.t
      (* FIX #3b: a SUPERSET of the variables whose [lower]/[upper] may form an empty
         interval (l > u). Such an interval is created only by the [assert_*] that
         tightens one bound past the other (detected there); [pop] only loosens, so it can
         only resolve one. [empty_interval_conflict] scans only this, reading current
         bounds as ground truth. *)
  ; mutable poisoned : bool
  (* set the instant a Rational.Overflow escapes a state-mutating op: the tableau may be
     left mid-pivot (INV-EQ broken), so any further reasoning is unsound and must be
     refused rather than trusted (see is_poisoned / Lia.Poisoned). *)
  }

and 'a conflict =
  { premises : 'a list
  ; farkas : Rational.t list
  }

let create () =
  { vars = Dynarray.create ()
  ; trail = Oxsmt_core.Trail.create ()
  ; pivots = 0
  ; dirty_basic = IntSet.empty
  ; dirty_bound = IntSet.empty
  ; poisoned = false
  }
;;

let is_poisoned t = t.poisoned

(* Brick the instance explicitly. The [Lia] layer calls this when a [Rational.Overflow]
   escapes its OWN arithmetic (atom translation, B&B branch bounds) — outside a {!guarded}
   simplex op — so reuse is refused there too. *)
let poison t = t.poisoned <- true

(* Run [f] (a state-mutating body that does exact arithmetic); if it raises
   [Rational.Overflow] the tableau may be left mid-pivot, so brick the instance before
   re-raising — every entry point then refuses to reason on it. *)
let guarded t f =
  try f () with
  | Rational.Overflow ->
    t.poisoned <- true;
    raise Rational.Overflow
;;

let get t i = Dynarray.get t.vars i
let num_vars t = Dynarray.length t.vars
let pivot_count t = t.pivots
let value t v = (get t v).value

let get_lower t v =
  match (get t v).lower with
  | Some b -> Some (b.reason, b.bval)
  | None -> None
;;

let get_upper t v =
  match (get t v).upper with
  | Some b -> Some (b.reason, b.bval)
  | None -> None
;;

let new_problem_var t =
  let id = Dynarray.length t.vars in
  let v =
    { id
    ; def = Lx.singleton id Rational.one
    ; value = Delta.zero
    ; lower = None
    ; upper = None
    ; basic = false
    ; row = Lx.create ()
    }
  in
  Dynarray.add_last t.vars v;
  id
;;

(* β(s) for a freshly-created slack: Σ coeff · β(var). *)
let eval_def t (def : linexp) =
  Lx.fold (fun j c acc -> Delta.add acc (Delta.scale c (get t j).value)) def Delta.zero
;;

(* Re-express a linear form over problem vars as a form over the {e current} nonbasic set,
   substituting each currently-basic variable by its row. Needed because a slack created
   mid-search may reference problem vars that pivoting has since made basic. *)
let expand t (def : linexp) : linexp =
  (* [acc] is a fresh row uniquely owned here, so it is a legal in-place destination; each
     [vj.row]/singleton is read-only. Same accumulated map as the former fold of
     add_scaled (the pre-in-place merge), built without a fresh array per step. *)
  Lx.fold
    (fun j c acc ->
      let vj = get t j in
      if vj.basic
      then Lx.add_scaled_in_place acc c vj.row
      else Lx.add_scaled_in_place acc c (Lx.singleton j Rational.one);
      acc)
    def
    (Lx.create ())
;;

let new_slack t (pairs : (int * Rational.t) list) =
  guarded t (fun () ->
    (* SUM coefficients on a repeated variable — do NOT overwrite (codex L1). A caller
       that passes e.g. [(x,1);(x,-1)] means s = 1·x + (-1)·x = 0·x, not s = -x. A
       resulting zero coefficient is dropped so [def] keeps its "no explicit zero"
       invariant. [Lx.of_list] sums repeated ids and drops zeros — same abstract map (and
       same summed coefficient values) as the former fold of add/remove. *)
    let def = Lx.of_list pairs in
    let id = Dynarray.length t.vars in
    let v =
      { id
      ; def
      ; value = Delta.zero
      ; lower = None
      ; upper = None
      ; basic = true
      ; row = Lx.create ()
      }
    in
    Dynarray.add_last t.vars v;
    (* Before solving starts, every problem variable is still nonbasic, so expanding the
       new row by merging one singleton at a time can only reproduce [def]. Copy it in one
       pass. Keep [def] and [row] physically separate: pivoting mutates [row], while [def]
       is the immutable meaning of the slack. A definition that references a variable made
       basic by pivoting falls back to the general substitution path. *)
    v.row
    <- (if Lx.for_all (fun j _ -> not (get t j).basic) def
        then Lx.copy def
        else expand t def);
    v.value <- eval_def t def;
    id)
;;

(* ---- Farkas certificate assembly + self-check (always on). ---- *)

(* A contribution uses one active bound of [var]: its lower ([use_lower]) or upper bound,
   weighted by [mult] >= 0. Half-plane (as [expr <= 0]): lower: l - def(var) <= 0 (from
   var >= l) upper: def(var) - u <= 0 (from var <= u) *)
type contribution =
  { var : int
  ; mult : Rational.t
  ; use_lower : bool
  }

let build_conflict t (contribs : contribution list) : 'a conflict =
  (* Self-check: Σ mult · half-plane must cancel all variables and leave a strictly
     positive constant. Accumulate variable coefficients (linexp) and the δ-rational
     constant. *)
  (* [acc_vars] is a fresh row uniquely owned here — a legal in-place destination. Each
     [hp_vars] is either a fresh [Lx.map] result or the shared immutable [v.def], and is
     only ever READ by [add_scaled_in_place], so no [def] is mutated. *)
  let acc_vars = Lx.create () in
  let acc_const = ref Delta.zero in
  let premises = ref [] in
  let farkas = ref [] in
  List.iter
    (fun { var; mult; use_lower } ->
      if Rational.sign mult < 0 then raise (Farkas_error "negative multiplier");
      let v = get t var in
      let bnd =
        match if use_lower then v.lower else v.upper with
        | Some b -> b
        | None -> raise (Farkas_error "contribution references an absent bound")
      in
      (* half-plane variable part and constant part *)
      let hp_vars, hp_const =
        if use_lower
        then Lx.map Rational.neg v.def, bnd.bval (* l - def <= 0 *)
        else v.def, Delta.neg bnd.bval (* def - u <= 0 *)
      in
      Lx.add_scaled_in_place acc_vars mult hp_vars;
      acc_const := Delta.add !acc_const (Delta.scale mult hp_const);
      premises := bnd.reason :: !premises;
      farkas := mult :: !farkas)
    contribs;
  if not (Lx.for_all (fun _ c -> Rational.is_zero c) acc_vars)
  then raise (Farkas_error "variables did not cancel");
  if not (Delta.lt Delta.zero !acc_const)
  then raise (Farkas_error "combined constant is not strictly positive");
  { premises = List.rev !premises; farkas = List.rev !farkas }
;;

(* ---- Bound assertion (DdM06 AssertUpper/AssertLower). ---- *)

let record_lower t (v : 'a var) old =
  Oxsmt_core.Trail.record t.trail (Undo_lower (v.id, old))
;;

let record_upper t (v : 'a var) old =
  Oxsmt_core.Trail.record t.trail (Undo_upper (v.id, old))
;;

(* [dirty_basic]/[dirty_bound] worklist maintenance (FIX #3b). Adding is always sound (the
   sets are supersets); the invariants that make
   [first_violating]/[empty_interval_conflict] complete are that every
   value-changed/bound-tightened basic var lands in [dirty_basic] and every newly-empty
   interval lands in [dirty_bound]. *)
let mark_basic t id = t.dirty_basic <- IntSet.add id t.dirty_basic
let mark_bound t id = t.dirty_bound <- IntSet.add id t.dirty_bound

(* Update a nonbasic var to value d, repairing basic values (INV-EQ) (DdM06 Update). *)
let update t (v : 'a var) (d : Delta.t) =
  let diff = Delta.sub d v.value in
  Dynarray.iter
    (fun b ->
      if b.basic
      then (
        let a = coeff b.row v.id in
        if not (Rational.is_zero a)
        then (
          b.value <- Delta.add b.value (Delta.scale a diff);
          (* value changed -> it may now violate its bound *)
          mark_basic t b.id)))
    t.vars;
  v.value <- d
;;

let assert_lower t vid (d : Delta.t) reason =
  guarded t (fun () ->
    let v = get t vid in
    match v.lower with
    | Some b when Delta.le d b.bval -> None (* not tighter *)
    | _ ->
      (match v.upper with
       | Some u when Delta.lt u.bval d ->
         (* l > u: immediate contradiction. Farkas: 1·(l - def) + 1·(def - u) = l - u > 0. *)
         let old = v.lower in
         record_lower t v old;
         v.lower <- Some { bval = d; reason };
         mark_bound t vid;
         let c =
           build_conflict
             t
             [ { var = vid; mult = Rational.one; use_lower = true }
             ; { var = vid; mult = Rational.one; use_lower = false }
             ]
         in
         Some c
       | _ ->
         let old = v.lower in
         record_lower t v old;
         v.lower <- Some { bval = d; reason };
         (* A tighter lower bound on a basic var may make its (unchanged) value violate;
            for a nonbasic var, [update] moves it in-bounds and marks the basics it
            shifts. *)
         if v.basic then mark_basic t vid;
         if (not v.basic) && Delta.lt v.value d then update t v d;
         None))
;;

let assert_upper t vid (d : Delta.t) reason =
  guarded t (fun () ->
    let v = get t vid in
    match v.upper with
    | Some b when Delta.le b.bval d -> None (* not tighter *)
    | _ ->
      (match v.lower with
       | Some l when Delta.lt d l.bval ->
         let old = v.upper in
         record_upper t v old;
         v.upper <- Some { bval = d; reason };
         mark_bound t vid;
         let c =
           build_conflict
             t
             [ { var = vid; mult = Rational.one; use_lower = false }
             ; { var = vid; mult = Rational.one; use_lower = true }
             ]
         in
         Some c
       | _ ->
         let old = v.upper in
         record_upper t v old;
         v.upper <- Some { bval = d; reason };
         if v.basic then mark_basic t vid;
         if (not v.basic) && Delta.lt d v.value then update t v d;
         None))
;;

(* ---- Pivoting (DdM06 Pivot / PivotAndUpdate). ---- *)

(* Pivot basic [bi] out and nonbasic [nj] in. Precondition: [bi] basic, [nj] in its row
   with nonzero coefficient. Values are updated by the caller (PivotAndUpdate) beforehand. *)
let pivot t (bi : 'a var) (nj : 'a var) =
  let a = coeff bi.row nj.id in
  (* nj = (1/a)·bi - Σ_{k≠nj} (a_bk/a)·k *)
  let inv = Rational.div Rational.one a in
  (* new_row = bi.id:inv + Σ_[{k≠nj}] (-(c·inv))·k, built as ONE fresh row (this runs once
     per pivot, not on the per-basic-row inner loop, so a functional build is fine). [bi]
     is basic, so [bi.id] never appears in [bi.row]; the functional ops read [bi.row]
     only. Same entries/values as the former singleton + per-key add. *)
  let new_row =
    Lx.add
      bi.id
      inv
      (Lx.map (fun c -> Rational.neg (Rational.mul c inv)) (Lx.remove nj.id bi.row))
  in
  bi.basic <- false;
  bi.row <- Lx.create ();
  nj.basic <- true;
  nj.row <- new_row;
  (* Substitute nj's new row into every other basic row that mentions nj. Each [k.row] is
     uniquely owned by [k], so it is mutated IN PLACE (remove nj, then += a_kn·nj.row);
     [nj.row] is the read-only scaled source. This is the incremental hot path the mutable
     row targets: no fresh row array per substituted basic variable. *)
  Dynarray.iter
    (fun k ->
      if k.basic && k.id <> nj.id
      then (
        let a_kn = coeff k.row nj.id in
        if not (Rational.is_zero a_kn)
        then (
          Lx.remove_in_place k.row nj.id;
          Lx.add_scaled_in_place k.row a_kn nj.row)))
    t.vars;
  t.pivots <- t.pivots + 1
;;

let pivot_and_update t (bi : 'a var) (nj : 'a var) (v : Delta.t) =
  let a = coeff bi.row nj.id in
  let theta = Delta.scale (Rational.div Rational.one a) (Delta.sub v bi.value) in
  bi.value <- v;
  nj.value <- Delta.add nj.value theta;
  (* [nj] becomes basic with a new value; the other basic rows that mention [nj] shift
     too. Each such var may now violate its bound -> add to the worklist ([bi] leaves the
     basis at its target value, so it needs no entry). *)
  mark_basic t nj.id;
  Dynarray.iter
    (fun k ->
      if k.basic && k.id <> bi.id
      then (
        let a_kn = coeff k.row nj.id in
        if not (Rational.is_zero a_kn)
        then (
          k.value <- Delta.add k.value (Delta.scale a_kn theta);
          mark_basic t k.id)))
    t.vars;
  pivot t bi nj
;;

(* ---- Check (DdM06). Bland's rule: smallest-id violating basic; smallest-id suitable
   nonbasic. Returns the first basic variable violating a bound, scanning by id. ---- *)

(* FIX #3b: the smallest-id violating basic variable, scanning only the [dirty_basic]
   worklist (a superset of the violating basic vars) instead of all vars. [IntSet] is
   ordered by id, so the first violating member found is the global smallest-id violator —
   Bland's leaving rule and its termination guarantee are unchanged. Confirmed-clean
   members (non-violating, or now nonbasic) are pruned; a returned violator is kept for
   the re-check that follows its pivot. When none violate, [dirty_basic] is emptied. *)
let first_violating t =
  let rec go s =
    match IntSet.min_elt_opt s with
    | None ->
      t.dirty_basic <- s;
      None
    | Some i ->
      let v = get t i in
      let viol =
        if v.basic
        then (
          match v.lower with
          | Some l when Delta.lt v.value l.bval -> Some (v, `Low)
          | _ ->
            (match v.upper with
             | Some u when Delta.lt u.bval v.value -> Some (v, `High)
             | _ -> None))
        else None
      in
      (match viol with
       | Some _ ->
         t.dirty_basic <- s;
         viol
       | None -> go (IntSet.remove i s))
  in
  go t.dirty_basic
;;

(* Smallest-id nonbasic in [bi]'s row that can move [bi] toward feasibility. [dir = `Inc]
   when we must increase β(bi), [`Dec] when we must decrease it. *)
let entering t (bi : 'a var) dir =
  let entries = Lx.bindings bi.row in
  (* [Lx.bindings] yields entries in ascending id order — Bland order (as Map.bindings
     did). *)
  let suitable (nj : 'a var) a =
    let can_increase =
      match nj.upper with
      | Some u -> Delta.lt nj.value u.bval
      | None -> true
    and can_decrease =
      match nj.lower with
      | Some l -> Delta.lt l.bval nj.value
      | None -> true
    in
    match dir with
    | `Inc -> if Rational.sign a > 0 then can_increase else can_decrease
    | `Dec -> if Rational.sign a > 0 then can_decrease else can_increase
  in
  let rec go = function
    | [] -> None
    | (j, a) :: rest ->
      let nj = get t j in
      if suitable nj a then Some nj else go rest
  in
  go entries
;;

(* Build the Farkas conflict for an unfixable basic [bi]. [low] iff β(bi) < lower. *)
let conflict_of t (bi : 'a var) ~low =
  let contribs = ref [ { var = bi.id; mult = Rational.one; use_lower = low } ] in
  Lx.iter
    (fun j a ->
      let pos = Rational.sign a > 0 in
      (* For β(bi) < l (increase case): a>0 uses upper(j), a<0 uses lower(j). For β(bi) >
         u (decrease case): a>0 uses lower(j), a<0 uses upper(j). *)
      let use_lower = if low then not pos else pos in
      let mult = if pos then a else Rational.neg a in
      contribs := { var = j; mult; use_lower } :: !contribs)
    bi.row;
  build_conflict t (List.rev !contribs)
;;

(* An asserted lower bound above an asserted upper bound (empty interval) is an immediate
   contradiction. The pivot loop is basic-only and value-driven, so it does not witness
   this for a nonbasic variable — [check] detects it structurally here. This replaces the
   earlier cached-[pending] scheme, which used a single scalar that a later [assert]
   overwrote and a subsequent [pop] then dropped, losing an earlier still-live
   contradiction (codex R1, false-SAT). Reading current bounds is the ground truth:
   reports the conflict iff both bounds are still asserted, vanishes exactly when a [pop]
   removes one. Scans in id order (determinism, I6). Farkas certificate: 1·(l - def) +
   1·(def - u) = l - u > 0.

   FIX #3b: scans only the [dirty_bound] worklist (a superset of the vars that may have l
   > u), not all vars; current bounds remain the ground truth (a member whose interval a
   [pop] has since reopened is pruned), and [IntSet] order keeps the id-order scan. *)
let empty_interval_conflict t =
  let rec go s =
    match IntSet.min_elt_opt s with
    | None ->
      t.dirty_bound <- s;
      None
    | Some i ->
      let v = get t i in
      (match v.lower, v.upper with
       | Some l, Some u when Delta.lt u.bval l.bval ->
         t.dirty_bound <- s;
         Some
           (build_conflict
              t
              [ { var = i; mult = Rational.one; use_lower = true }
              ; { var = i; mult = Rational.one; use_lower = false }
              ])
       | _ -> go (IntSet.remove i s))
  in
  go t.dirty_bound
;;

let check t =
  guarded t (fun () ->
    match empty_interval_conflict t with
    | Some c -> Some c
    | None ->
      let rec loop () =
        match first_violating t with
        | None -> None
        | Some (bi, `Low) ->
          (match entering t bi `Inc with
           | None -> Some (conflict_of t bi ~low:true)
           | Some nj ->
             let target = (Option.get bi.lower).bval in
             pivot_and_update t bi nj target;
             loop ())
        | Some (bi, `High) ->
          (match entering t bi `Dec with
           | None -> Some (conflict_of t bi ~low:false)
           | Some nj ->
             let target = (Option.get bi.upper).bval in
             pivot_and_update t bi nj target;
             loop ())
      in
      loop ())
;;

(* ---- push / pop ---- *)

(* Reverse one bound change. [pop] only ever loosens bounds (restores an older, weaker
   [bound option]), so no feasibility violation can be created — the [dirty_*] sets need
   no maintenance here (see their invariants). *)
let apply_undo t = function
  | Undo_lower (vid, old) -> (get t vid).lower <- old
  | Undo_upper (vid, old) -> (get t vid).upper <- old
;;

let push t = Oxsmt_core.Trail.push t.trail ()
let pop t n = Oxsmt_core.Trail.pop t.trail ~apply:(apply_undo t) n

(* ADR-0014 Stage 4.2 sub-frame checkpoint/rewind. Bound changes are the only
   backtrackable state (tableau/assignment are recomputed by [check]); a checkpoint is
   simply the undo-trail watermark. [rewind_to_checkpoint] drains the bound-undo trail
   newest-first to that watermark (identical to what a [pop] reverses), without touching
   the frame stack. The [dirty_*] supersets need no maintenance (rewinding only LOOSENS
   bounds, exactly like [pop] — see their invariants). *)
let checkpoint t = Oxsmt_core.Trail.mark t.trail
let rewind_to_checkpoint t m = Oxsmt_core.Trail.rewind_to t.trail ~apply:(apply_undo t) m

(* ---- Unit cube test (Bromberger & Fleury, "Fast cube tests for LIA constraint solving",
   TACAS 2016). A sufficient integer-feasibility test that finds a model with no
   branch-and-bound: shrink every constraint interval inward by half the 1-norm of the
   constraint's coefficient row, and test rational feasibility of the shrunk system. If it
   is feasible, its LP point rounded to the nearest integer satisfies the ORIGINAL system
   — rounding each problem variable by at most 1/2 moves a constraint value [def·x] by at
   most (1/2)·‖def‖₁, exactly the shrink — so it is a genuine integer model. Failure is
   inconclusive (fall back to b&b): the test is sufficient, not necessary. ---- *)

let half = Rational.of_frac 1 2

(* Σ|coeffᵢ| of a var's immutable def over the problem variables (its sensitivity to
   rounding each problem var by ≤ 1/2). A problem var's def is the singleton [{id:1}] so
   its 1-norm is 1; a slack's is the sum of its |coefficients|. *)
let one_norm (def : linexp) =
  Lx.fold (fun _ c acc -> Rational.add acc (Rational.abs c)) def Rational.zero
;;

(* Nearest integer to a δ-rational's finite part, as an integer Rational
   ([floor(c + 1/2)]; the LIA atom path never sets a δ component, so [k] is irrelevant).
   Raises {!Rational.Overflow} only at the int63 output-projection boundary. *)
let round_nearest (d : Delta.t) =
  Rational.of_int (Rational.floor (Rational.add (Delta.c_part d) half))
;;

let cube_test t (problem_vars : int list) : (int * Rational.t) list option =
  let n = Dynarray.length t.vars in
  (* Shrink+re-solve happens under a push/pop so no tightened bound persists; the internal
     exact arithmetic (add/sub/mul/div, two-tier Rational) never raises, so this cannot
     poison the instance. *)
  push t;
  let feasible = ref true in
  let i = ref 0 in
  while !feasible && !i < n do
    let v = get t !i in
    let shift = Delta.of_rat (Rational.mul half (one_norm v.def)) in
    (match v.lower with
     | Some b ->
       (match assert_lower t v.id (Delta.add b.bval shift) b.reason with
        | Some _ -> feasible := false
        | None -> ())
     | None -> ());
    if !feasible
    then (
      match v.upper with
      | Some b ->
        (match assert_upper t v.id (Delta.sub b.bval shift) b.reason with
         | Some _ -> feasible := false
         | None -> ())
      | None -> ());
    incr i
  done;
  (if !feasible
   then
     feasible
     := match check t with
        | None -> true
        | Some _ -> false);
  (* Read the shrunk LP point at the problem vars BEFORE popping: [pop] restores bounds
     but not values, and the restore-check below re-pivots (changing values). *)
  let lp_point =
    if !feasible
    then Some (List.map (fun id -> id, (get t id).value) problem_vars)
    else None
  in
  pop t 1;
  (* Re-establish a feasible untightened tableau for the b&b fall-back (the shrink solve
     may have left [dirty_basic] populated or the tableau at a shrunk-only vertex). *)
  ignore (check t : 'a conflict option);
  match lp_point with
  | None -> None
  | Some pts ->
    (try
       let assignment = List.map (fun (id, d) -> id, round_nearest d) pts in
       (* Re-verify the rounded integer point against the ORIGINAL bounds:
          [def·assignment] must lie within [lower, upper] for every var. This is the
          soundness gate — the point is returned only if the simplex confirms it feasible
          (the cube theorem guarantees this, so a failure here would be a bug, never a
          normal outcome). *)
       (* [assignment] has one entry per problem var (distinct ids); [value_of] reads it
          only via [coeff vals j], for which a dropped zero and an explicit zero are
          indistinguishable — so [Lx.of_list] is value-identical to the former per-id add. *)
       let vals = Lx.of_list assignment in
       let value_of def =
         Lx.fold
           (fun j c acc -> Rational.add acc (Rational.mul c (coeff vals j)))
           def
           Rational.zero
       in
       let ok = ref true in
       let k = ref 0 in
       while !ok && !k < n do
         let v = get t !k in
         let d = Delta.of_rat (value_of v.def) in
         (match v.lower with
          | Some b -> if not (Delta.le b.bval d) then ok := false
          | None -> ());
         (match v.upper with
          | Some b -> if not (Delta.le d b.bval) then ok := false
          | None -> ());
         incr k
       done;
       if !ok then Some assignment else None
     with
     | Rational.Overflow -> None)
;;

(* Stage B2 (HNF tight-constraint cut): every variable whose current value sits EXACTLY on
   one of its finite NON-STRICT bounds — active-at-bound on the current assignment (z3
   [get_equality_and_right_side_for_term_on_current_x]). Each row carries the variable's
   immutable [def] (a linear form over problem-var ids; a problem var is [1·id]), the
   bound VALUE, the SIDE it is tight on, and the bound's reason token. A FIXED variable
   ([lower = upper] both tight) yields TWO rows (both sides) — the caller reads that as an
   equality (the ± pair admits an any-sign lattice multiplier), while a one-sided tight
   bound is a genuine inequality (the caller's Chvátal–Gomory sign discipline restricts
   its multiplier to ≥ 0). Read-only. *)
type 'a tight_row =
  { row_var : int
  ; row_def : (int * Rational.t) list
  ; row_bound : Rational.t
  ; row_side : [ `Lower | `Upper ]
  ; row_reason : 'a
  }

let tight_rows t : 'a tight_row list =
  let acc = ref [] in
  Dynarray.iter
    (fun v ->
      let consider side bopt =
        match bopt with
        | Some b when Delta.is_rational b.bval && Delta.equal v.value b.bval ->
          acc
          := { row_var = v.id
             ; row_def = Lx.bindings v.def
             ; row_bound = Delta.c_part b.bval
             ; row_side = side
             ; row_reason = b.reason
             }
             :: !acc
        | _ -> ()
      in
      consider `Lower v.lower;
      consider `Upper v.upper)
    t.vars;
  List.rev !acc
;;
