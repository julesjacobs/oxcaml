(* Linear real arithmetic over the shared exact-rational simplex.  Unlike [Lia], this
   layer deliberately has no integer normalization, cuts, or branch-and-bound. *)

type var = int

type comparison =
  | Le
  | Lt
  | Ge
  | Gt
  | Eq
  | Ne

type constraint_ =
  { coeffs : (var * Rational.t) list
  ; comparison : comparison
  ; rhs : Rational.t
  }

type 'premise half_plane =
  { premise : 'premise
  ; coeffs : (var * Rational.t) list
  ; constant : Delta.t
  }

type 'premise conflict =
  { premises : 'premise list
  ; half_planes : 'premise half_plane list
  ; farkas : Rational.t list
  }

type 'premise assertion =
  | Asserted
  | Immediate_conflict of 'premise conflict
  | Split of constraint_ * constraint_

type 'premise result =
  | Sat
  | Unsat of 'premise conflict

exception Poisoned

type 'premise t =
  { simplex : 'premise half_plane Simplex.t
  ; problem_vars : var Dynarray.t
  ; is_problem_var : (var, unit) Hashtbl.t
  ; slacks : ((var * string) list, int) Hashtbl.t
  ; asserted : constraint_ Dynarray.t
  ; mutable frames : int list
  ; mutable model_ready : bool
  }

let create () =
  { simplex = Simplex.create ()
  ; problem_vars = Dynarray.create ()
  ; is_problem_var = Hashtbl.create 32
  ; slacks = Hashtbl.create 64
  ; asserted = Dynarray.create ()
  ; frames = []
  ; model_ready = false
  }
;;

let is_poisoned t = Simplex.is_poisoned t.simplex
let ensure_live t = if is_poisoned t then raise Poisoned

let new_var t =
  ensure_live t;
  let v = Simplex.new_problem_var t.simplex in
  Dynarray.add_last t.problem_vars v;
  Hashtbl.add t.is_problem_var v ();
  t.model_ready <- false;
  v
;;

(* Canonicalize a caller's sparse row before it reaches either the tableau or a public
   certificate.  In particular, repeated variables are SUMMED rather than overwritten.
   The sorted representation also makes certificates independent of hash-table order. *)
let normalize_coeffs t coeffs =
  List.iter
    (fun (v, _) ->
      if not (Hashtbl.mem t.is_problem_var v)
      then invalid_arg "Lra.assert_constraint: unknown variable")
    coeffs;
  let sorted = List.stable_sort (fun (a, _) (b, _) -> Int.compare a b) coeffs in
  let rec gather acc = function
    | [] -> List.rev acc
    | (v, q) :: rest ->
      let rec same sum = function
        | (w, r) :: tail when v = w -> same (Rational.add sum r) tail
        | tail -> sum, tail
      in
      let sum, tail = same q rest in
      if Rational.is_zero sum then gather acc tail else gather ((v, sum) :: acc) tail
  in
  gather [] sorted
;;

let externalize (c : _ Simplex.conflict) =
  { premises = List.map (fun hp -> hp.premise) c.premises
  ; half_planes = c.premises
  ; farkas = c.farkas
  }
;;

let upper_reason ~premise coeffs bound =
  { premise; coeffs; constant = Delta.neg bound }
;;

let lower_reason ~premise coeffs bound =
  { premise
  ; coeffs = List.map (fun (v, q) -> v, Rational.neg q) coeffs
  ; constant = bound
  }
;;

(* Equal linear forms share one simplex variable.  The key uses canonical coefficient
   strings rather than the abstract two-tier [Rational.t] representation, so value-equal
   rationals always compare equal.  Hash collisions remain harmless because Hashtbl also
   compares the complete [(var, string) list]. *)
let row_for_coeffs t coeffs =
  match coeffs with
  | [ v, q ] when Rational.equal q Rational.one -> v
  | _ ->
    let key = List.map (fun (v, q) -> v, Rational.to_string q) coeffs in
    (match Hashtbl.find_opt t.slacks key with
     | Some row -> row
     | None ->
       let row = Simplex.new_slack t.simplex coeffs in
       Hashtbl.add t.slacks key row;
       row)
;;

let assert_constraint t (c : constraint_) ~premise =
  ensure_live t;
  let coeffs = normalize_coeffs t c.coeffs in
  let c = { c with coeffs } in
  match c.comparison with
  | Ne ->
    (* A disequality is a disjunction, not a convex simplex bound.  Return the exhaustive
       real-order split and leave the current conjunction untouched. *)
    Split ({ c with comparison = Lt }, { c with comparison = Gt })
  | (Le | Lt | Ge | Gt | Eq) as comparison ->
    (* Keep exact rational coefficients in the row.  Clearing denominators is sound only
       with a positive common multiplier, but is unnecessary here: [Simplex.new_slack]
       already stores exact [Rational.t] coefficients, and retaining them avoids a large
       denominator product and preserves the caller's sparse scale. *)
    let row = row_for_coeffs t coeffs in
    let upper strict =
      let bound =
        if strict
        then Delta.make c.rhs (Rational.neg Rational.one)
        else Delta.of_rat c.rhs
      in
      Simplex.assert_upper t.simplex row bound (upper_reason ~premise coeffs bound)
    in
    let lower strict =
      let bound =
        if strict
        then Delta.make c.rhs Rational.one
        else Delta.of_rat c.rhs
      in
      Simplex.assert_lower t.simplex row bound (lower_reason ~premise coeffs bound)
    in
    let immediate =
      match comparison with
      | Le -> upper false
      | Lt -> upper true
      | Ge -> lower false
      | Gt -> lower true
      | Eq ->
        (* A real equality is exactly the conjunction of its two non-strict orientations.
           Install both even if the first reports an immediate conflict; assertions only
           tighten, so that certificate remains valid in the resulting state. *)
        let c1 = upper false in
        let c2 = lower false in
        (match c1 with
         | Some _ -> c1
         | None -> c2)
      | Ne -> assert false
    in
    Dynarray.add_last t.asserted c;
    t.model_ready <- false;
    (match immediate with
     | None -> Asserted
     | Some conflict -> Immediate_conflict (externalize conflict))
;;

let check t =
  ensure_live t;
  match Simplex.check t.simplex with
  | None ->
    t.model_ready <- true;
    Sat
  | Some conflict ->
    t.model_ready <- false;
    Unsat (externalize conflict)
;;

let delta_of_linear t coeffs rhs =
  List.fold_left
    (fun sum (v, q) -> Delta.add sum (Delta.scale q (Simplex.value t.simplex v)))
    (Delta.of_rat (Rational.neg rhs))
    coeffs
;;

(* Add the upper bound on epsilon needed to make [d(epsilon) <= 0] (or [< 0]) concrete.
   Symbolic delta feasibility has already established the requested comparison.  Only a
   positive delta coefficient can move a negative finite gap back toward zero as epsilon
   grows; it contributes the positive ceiling [-c/k]. *)
let epsilon_ceiling d ~strict ceilings =
  let symbolically_satisfied =
    if strict then Delta.lt d Delta.zero else Delta.le d Delta.zero
  in
  if not symbolically_satisfied
  then failwith "Lra.model: simplex assignment violates an asserted constraint";
  let k = Delta.k_part d in
  if Rational.sign k > 0
  then (
    let c = Delta.c_part d in
    let ceiling = Rational.div (Rational.neg c) k in
    if Rational.sign ceiling <= 0
    then failwith "Lra.model: non-positive delta concretization ceiling";
    ceiling :: ceilings)
  else ceilings
;;

let epsilon_for_model t =
  let ceilings = ref [] in
  Dynarray.iter
    (fun (c : constraint_) ->
      let d = delta_of_linear t c.coeffs c.rhs in
      ceilings :=
        (match c.comparison with
         | Le -> epsilon_ceiling d ~strict:false !ceilings
         | Lt -> epsilon_ceiling d ~strict:true !ceilings
         | Ge -> epsilon_ceiling (Delta.neg d) ~strict:false !ceilings
         | Gt -> epsilon_ceiling (Delta.neg d) ~strict:true !ceilings
         | Eq ->
           if not (Delta.equal d Delta.zero)
           then failwith "Lra.model: simplex assignment violates an asserted equality";
           !ceilings
         | Ne -> failwith "Lra.model: an unsplit disequality was installed"))
    t.asserted;
  match !ceilings with
  | [] -> Rational.one
  | first :: rest ->
    let ceiling = List.fold_left Rational.min first rest in
    (* Taking half gives strict room below every finite ceiling.  Cap at one so the
       no-nearby-bound case has the simple, deterministic substitution delta = 1. *)
    Rational.min Rational.one (Rational.div ceiling (Rational.of_int 2))
;;

let eval_rational values coeffs =
  List.fold_left
    (fun sum (v, q) -> Rational.add sum (Rational.mul q (Hashtbl.find values v)))
    Rational.zero
    coeffs
;;

let comparison_holds comparison lhs rhs =
  let cmp = Rational.compare lhs rhs in
  match comparison with
  | Le -> cmp <= 0
  | Lt -> cmp < 0
  | Ge -> cmp >= 0
  | Gt -> cmp > 0
  | Eq -> cmp = 0
  | Ne -> cmp <> 0
;;

let model t =
  ensure_live t;
  if not t.model_ready then invalid_arg "Lra.model: check has not returned Sat";
  let epsilon = epsilon_for_model t in
  let values = Hashtbl.create (Dynarray.length t.problem_vars) in
  let result =
    Dynarray.fold_left
      (fun acc v ->
        let d = Simplex.value t.simplex v in
        let q =
          Rational.add (Delta.c_part d) (Rational.mul (Delta.k_part d) epsilon)
        in
        Hashtbl.add values v q;
        (v, q) :: acc)
      []
      t.problem_vars
    |> List.rev
  in
  (* This check is deliberately independent of delta feasibility and uses the public
     rational comparison.  A future change that breaks concretization fails loudly here
     instead of allowing a strict-boundary value to escape as a purported model. *)
  Dynarray.iter
    (fun (c : constraint_) ->
      let lhs = eval_rational values c.coeffs in
      if not (comparison_holds c.comparison lhs c.rhs)
      then failwith "Lra.model: concretized assignment failed exact recheck")
    t.asserted;
  result
;;

let value t v =
  if not (Hashtbl.mem t.is_problem_var v) then invalid_arg "Lra.value: unknown variable";
  List.assoc v (model t)
;;

let existing_row_for_coeffs t coeffs =
  let coeffs = normalize_coeffs t coeffs in
  match coeffs with
  | [ v, q ] when Rational.equal q Rational.one -> Some v
  | _ ->
    let key = List.map (fun (v, q) -> v, Rational.to_string q) coeffs in
    Hashtbl.find_opt t.slacks key
;;

let fixed_value t ~coeffs ~constant =
  ensure_live t;
  match existing_row_for_coeffs t coeffs with
  | None -> None
  | Some row ->
    (match Simplex.get_lower t.simplex row, Simplex.get_upper t.simplex row with
     | Some (lower_reason, lower), Some (upper_reason, upper)
       when Delta.is_rational lower
            && Delta.is_rational upper
            && Rational.equal (Delta.c_part lower) (Delta.c_part upper) ->
       Some
         ( Rational.add (Delta.c_part lower) constant
         , lower_reason.premise
         , upper_reason.premise )
     | _ -> None)
;;

let oriented_bound t ~coeffs ~constant which =
  ensure_live t;
  match existing_row_for_coeffs t coeffs with
  | None -> None
  | Some row ->
    let bound =
      match which with
      | `Lower -> Simplex.get_lower t.simplex row
      | `Upper -> Simplex.get_upper t.simplex row
    in
    (match bound with
     | Some (reason, delta) when Delta.is_rational delta ->
       Some (reason.premise, Rational.add (Delta.c_part delta) constant)
     | Some _ | None -> None)
;;

let push t =
  ensure_live t;
  Simplex.push t.simplex;
  t.frames <- Dynarray.length t.asserted :: t.frames
;;

let pop t n =
  ensure_live t;
  if n < 0 then invalid_arg "Lra.pop: negative frame count";
  let rec unwind k frames watermark =
    if k = 0
    then watermark, frames
    else
      match frames with
      | [] -> invalid_arg "Lra.pop: too many frames"
      | mark :: rest -> unwind (k - 1) rest mark
  in
  let watermark, frames = unwind n t.frames (Dynarray.length t.asserted) in
  Simplex.pop t.simplex n;
  Dynarray.truncate t.asserted watermark;
  t.frames <- frames;
  if n > 0 then t.model_ready <- false
;;

type checkpoint =
  { simplex : int
  ; asserted : int
  }

let checkpoint t =
  ensure_live t;
  { simplex = Simplex.checkpoint t.simplex; asserted = Dynarray.length t.asserted }
;;

let rewind_to_checkpoint t checkpoint =
  ensure_live t;
  Simplex.rewind_to_checkpoint t.simplex checkpoint.simplex;
  Dynarray.truncate t.asserted checkpoint.asserted;
  t.model_ready <- false
;;

let pivot_count (t : 'premise t) = Simplex.pivot_count t.simplex
