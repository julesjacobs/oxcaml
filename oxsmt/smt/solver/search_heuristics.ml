(* Pure, deterministic building blocks for the modern CDCL search layer: LBD ("glue")
   scoring and LBD-based reduceDB selection (S3), and the CaDiCaL-style rephasing cycle
   (#155). Stdlib-only (dependency firewall I3) and STATELESS — every function is a pure
   function of its arguments, holding no solver state. That is deliberate: the frozen
   sat.mli hides the solver internals, so this is where the head-checkable heuristic logic
   lives and is unit-tested directly. [Sat] threads solver state (clause levels, activity,
   the rephase-event counter) through these functions. Novelty-free (DESIGN.md §5): LBD is
   Audemard–Simon 2009; the reduceDB order and rephase schedule mirror Glucose / CaDiCaL.

   Determinism (I6): no wall-clock, no randomness. [reduce_deletions] uses a stable sort
   so ties keep input order; every schedule is a total function of a count. *)

(* LBD ("glue") of a clause = the number of DISTINCT decision levels among its literals
   (Audemard–Simon 2009): a small LBD means the clause ties together few levels, which
   correlates with usefulness. [of_levels] takes the decision level of each literal and
   counts the distinct ones; the empty clause has LBD 0. Sort-and-scan (clauses are small;
   this is never on the conflict-free firehose path). *)
let lbd_of_levels levels =
  let a = Array.copy levels in
  Array.sort compare a;
  let count = ref 0 in
  Array.iteri (fun i x -> if i = 0 || x <> a.(i - 1) then incr count) a;
  !count
;;

(* Clauses with LBD at or below this are "glue": {!reduce_deletions} keeps them
   permanently. (Glucose's canonical value.) *)
let glue_threshold = 2

type clause_stat =
  { lbd : int
  ; activity : float
  ; protected_ : bool
    (* locked (the current reason for its asserting literal) or binary: never deleted,
     structurally — independent of LBD/activity. *)
  }

(* [reduce_deletions stats] returns a bool array parallel to [stats] marking which learned
   clauses to delete in an LBD-based reduceDB. Protected clauses and glue (LBD <=
   {!glue_threshold}) are never marked. Among the remaining (removable) clauses, the worst
   half of the WHOLE set is marked — ordered worst-first by LBD descending, ties broken by
   activity ascending (least active is worst). Half-of-total (not half-of-removable) so a
   DB dominated by glue is barely trimmed, exactly as intended. Deterministic: [List.sort]
   is stable, so equal (lbd, activity) pairs keep their input order. *)
let reduce_deletions stats =
  let n = Array.length stats in
  let del = Array.make n false in
  let removable =
    List.filter
      (fun i -> (not stats.(i).protected_) && stats.(i).lbd > glue_threshold)
      (List.init n (fun i -> i))
  in
  let worst_first =
    List.sort
      (fun i j ->
         if stats.(i).lbd <> stats.(j).lbd
         then compare stats.(j).lbd stats.(i).lbd (* higher LBD first *)
         else compare stats.(i).activity stats.(j).activity (* lower activity first *))
      removable
  in
  let limit = n / 2 in
  List.iteri (fun rank i -> if rank < limit then del.(i) <- true) worst_first;
  del
;;

type rephase_mode =
  | Flipped_true (* decide every var TRUE-first: the flip of the FALSE-first default *)
  | Best_trail
    (* the phases of the longest trail prefix seen so far (best-so-far memory) *)
  | Original_default (* the solver's initial default (FALSE-first) *)
  | Saved (* keep the phase-saving array untouched *)

(* The rephasing cycle (CaDiCaL-style), indexed by the rephase-event count. Front-loads
   [Flipped_true] (event 0) so the very FIRST rephase impulse searches for a TRUE-heavy
   model — the firehose lever (a uniform TRUE-flip measured +44 QF_LIA; see
   logs/phase-hints-report.md) — then cycles through the memory phase [Best_trail], the
   [Original_default], and [Saved]. The cycle is self-correcting: where TRUE-first hurts,
   the very next impulses restore the searched/default/best phases. *)
let rephase_mode event =
  match event mod 4 with
  | 0 -> Flipped_true
  | 1 -> Best_trail
  | 2 -> Original_default
  | _ -> Saved
;;

(* The next rephase interval, grown ~1.5x, so rephasing backs off on long instances
   instead of thrashing (an ever-shorter descent budget would never let a hard instance
   settle). *)
let grow_interval n = n + (n / 2)
