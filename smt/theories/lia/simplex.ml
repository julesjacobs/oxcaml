(* Incremental general simplex over δ-rationals. See simplex.mli.

   Following Dutertre-de Moura, "A Fast Linear-Arithmetic Solver for DPLL(T)" (2006):
   AssertUpper/AssertLower (§5), Check with Bland's rule, PivotAndUpdate + Pivot.
   Invariants maintained at all times (DdM06): (INV-EQ) every basic variable equals Σ (row
   coeff · nonbasic value); (INV-NB) every nonbasic variable is within its bounds; Check
   restores (INV-EQ)+(INV-NB) for the basic variables too, or reports a conflict. *)

exception Farkas_error of string

module IntMap = Map.Make (Int)

(* A linear expression: var id -> nonzero coefficient (absent = 0). Used both for a
   variable's immutable Farkas half-plane basis ([def], over problem vars) and for a basic
   variable's mutable tableau [row] (over current nonbasic vars). *)
type linexp = Rational.t IntMap.t

let coeff (m : linexp) i =
  match IntMap.find_opt i m with
  | Some c -> c
  | None -> Rational.zero
;;

(* acc + s · m, dropping resulting zeros. *)
let add_scaled (acc : linexp) (s : Rational.t) (m : linexp) : linexp =
  IntMap.fold
    (fun j c acc ->
       let nv = Rational.add (coeff acc j) (Rational.mul s c) in
       if Rational.is_zero nv then IntMap.remove j acc else IntMap.add j nv acc)
    m
    acc
;;

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
  ; trail : 'a undo Dynarray.t
  ; scopes : int Dynarray.t (* trail length at each open frame *)
  ; mutable pivots : int
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
  ; trail = Dynarray.create ()
  ; scopes = Dynarray.create ()
  ; pivots = 0
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
    ; def = IntMap.singleton id Rational.one
    ; value = Delta.zero
    ; lower = None
    ; upper = None
    ; basic = false
    ; row = IntMap.empty
    }
  in
  Dynarray.add_last t.vars v;
  id
;;

(* β(s) for a freshly-created slack: Σ coeff · β(var). *)
let eval_def t (def : linexp) =
  IntMap.fold
    (fun j c acc -> Delta.add acc (Delta.scale c (get t j).value))
    def
    Delta.zero
;;

(* Re-express a linear form over problem vars as a form over the {e current} nonbasic set,
   substituting each currently-basic variable by its row. Needed because a slack created
   mid-search may reference problem vars that pivoting has since made basic. *)
let expand t (def : linexp) : linexp =
  IntMap.fold
    (fun j c acc ->
       let vj = get t j in
       if vj.basic
       then add_scaled acc c vj.row
       else add_scaled acc c (IntMap.singleton j Rational.one))
    def
    IntMap.empty
;;

let new_slack t (pairs : (int * Rational.t) list) =
  guarded t (fun () ->
    (* SUM coefficients on a repeated variable — do NOT overwrite (codex L1). A caller
       that passes e.g. [(x,1);(x,-1)] means s = 1·x + (-1)·x = 0·x, not s = -x;
       overwriting with IntMap.add would build the wrong def and Farkas-certify a false
       conflict. A resulting zero coefficient is dropped so [def] keeps its "no explicit
       zero" invariant. *)
    let def =
      List.fold_left
        (fun m (j, c) ->
           let c' = Rational.add (coeff m j) c in
           if Rational.is_zero c' then IntMap.remove j m else IntMap.add j c' m)
        IntMap.empty
        pairs
    in
    let id = Dynarray.length t.vars in
    let v =
      { id
      ; def
      ; value = Delta.zero
      ; lower = None
      ; upper = None
      ; basic = true
      ; row = IntMap.empty
      }
    in
    Dynarray.add_last t.vars v;
    v.row <- expand t def;
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
  let acc_vars = ref IntMap.empty in
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
         then IntMap.map Rational.neg v.def, bnd.bval (* l - def <= 0 *)
         else v.def, Delta.neg bnd.bval (* def - u <= 0 *)
       in
       acc_vars := add_scaled !acc_vars mult hp_vars;
       acc_const := Delta.add !acc_const (Delta.scale mult hp_const);
       premises := bnd.reason :: !premises;
       farkas := mult :: !farkas)
    contribs;
  if not (IntMap.for_all (fun _ c -> Rational.is_zero c) !acc_vars)
  then raise (Farkas_error "variables did not cancel");
  if not (Delta.lt Delta.zero !acc_const)
  then raise (Farkas_error "combined constant is not strictly positive");
  { premises = List.rev !premises; farkas = List.rev !farkas }
;;

(* ---- Bound assertion (DdM06 AssertUpper/AssertLower). ---- *)

let record_lower t (v : 'a var) old = Dynarray.add_last t.trail (Undo_lower (v.id, old))
let record_upper t (v : 'a var) old = Dynarray.add_last t.trail (Undo_upper (v.id, old))

(* Update a nonbasic var to value d, repairing basic values (INV-EQ) (DdM06 Update). *)
let update t (v : 'a var) (d : Delta.t) =
  let diff = Delta.sub d v.value in
  Dynarray.iter
    (fun b ->
       if b.basic
       then (
         let a = coeff b.row v.id in
         if not (Rational.is_zero a)
         then b.value <- Delta.add b.value (Delta.scale a diff)))
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
  let new_row = ref (IntMap.singleton bi.id inv) in
  IntMap.iter
    (fun k c ->
       if k <> nj.id
       then new_row := IntMap.add k (Rational.neg (Rational.mul c inv)) !new_row)
    bi.row;
  bi.basic <- false;
  bi.row <- IntMap.empty;
  nj.basic <- true;
  nj.row <- !new_row;
  (* Substitute nj's new row into every other basic row that mentions nj. *)
  Dynarray.iter
    (fun k ->
       if k.basic && k.id <> nj.id
       then (
         let a_kn = coeff k.row nj.id in
         if not (Rational.is_zero a_kn)
         then k.row <- add_scaled (IntMap.remove nj.id k.row) a_kn nj.row))
    t.vars;
  t.pivots <- t.pivots + 1
;;

let pivot_and_update t (bi : 'a var) (nj : 'a var) (v : Delta.t) =
  let a = coeff bi.row nj.id in
  let theta = Delta.scale (Rational.div Rational.one a) (Delta.sub v bi.value) in
  bi.value <- v;
  nj.value <- Delta.add nj.value theta;
  Dynarray.iter
    (fun k ->
       if k.basic && k.id <> bi.id
       then (
         let a_kn = coeff k.row nj.id in
         if not (Rational.is_zero a_kn)
         then k.value <- Delta.add k.value (Delta.scale a_kn theta)))
    t.vars;
  pivot t bi nj
;;

(* ---- Check (DdM06). Bland's rule: smallest-id violating basic; smallest-id suitable
   nonbasic. Returns the first basic variable violating a bound, scanning by id. ---- *)

let first_violating t =
  let n = Dynarray.length t.vars in
  let rec go i =
    if i >= n
    then None
    else (
      let v = get t i in
      if v.basic
      then (
        match v.lower with
        | Some l when Delta.lt v.value l.bval -> Some (v, `Low)
        | _ ->
          (match v.upper with
           | Some u when Delta.lt u.bval v.value -> Some (v, `High)
           | _ -> go (i + 1)))
      else go (i + 1))
  in
  go 0
;;

(* Smallest-id nonbasic in [bi]'s row that can move [bi] toward feasibility. [dir = `Inc]
   when we must increase β(bi), [`Dec] when we must decrease it. *)
let entering t (bi : 'a var) dir =
  let entries = IntMap.bindings bi.row in
  (* IntMap.bindings is sorted by id — Bland order. *)
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
  IntMap.iter
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
   contradiction. The pivot loop is basic-only and value-driven, so it does not witness this
   for a nonbasic variable — [check] detects it structurally here. This replaces the earlier
   cached-[pending] scheme, which used a single scalar that a later [assert] overwrote and a
   subsequent [pop] then dropped, losing an earlier still-live contradiction (codex R1,
   false-SAT). Reading current bounds is the ground truth: reports the conflict iff both
   bounds are still asserted, vanishes exactly when a [pop] removes one. Scans in id order
   (determinism, I6). Farkas certificate: 1·(l - def) + 1·(def - u) = l - u > 0. *)
let empty_interval_conflict t =
  let n = Dynarray.length t.vars in
  let rec go i =
    if i >= n
    then None
    else (
      let v = get t i in
      match v.lower, v.upper with
      | Some l, Some u when Delta.lt u.bval l.bval ->
        Some
          (build_conflict
             t
             [ { var = i; mult = Rational.one; use_lower = true }
             ; { var = i; mult = Rational.one; use_lower = false }
             ])
      | _ -> go (i + 1))
  in
  go 0
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

let push t = Dynarray.add_last t.scopes (Dynarray.length t.trail)

let pop t n =
  for _ = 1 to n do
    let target = Dynarray.pop_last t.scopes in
    while Dynarray.length t.trail > target do
      match Dynarray.pop_last t.trail with
      | Undo_lower (vid, old) -> (get t vid).lower <- old
      | Undo_upper (vid, old) -> (get t vid).upper <- old
    done
  done
;;
