open Oxsmt_core

type linear =
  { coeffs : (Lra.var * Rational.t) list
  ; constant : Rational.t
  }

type disequality =
  { eq : Term.t
  ; lhs : Term.t
  ; rhs : Term.t
  ; lt : Lra.constraint_
  ; gt : Lra.constraint_
  }

type conflict_core =
  { farkas : Rational.t list option
  ; atoms : (Term.t * bool) list
  }

type t =
  { ctx : Context.t
  ; lra : Fabric.justification Lra.t
  ; term_of_atom : Term.t Atom.Table.t
  ; atom_of_term : Atom.t Term.Table.t
  ; var_of_term : Lra.var Term.Table.t
  ; term_of_var : (Lra.var, Term.t) Hashtbl.t
  ; disequalities : disequality Dynarray.t
  ; reasons : Lit.t Dynarray.t
  ; mutable frames : (int * int) list
  ; mutable last_conflict :
      (Rational.t list * Fabric.justification list) option
  }

let create ctx _env =
  { ctx
  ; lra = Lra.create ()
  ; term_of_atom = Atom.Table.create 64
  ; atom_of_term = Term.Table.create 64
  ; var_of_term = Term.Table.create 64
  ; term_of_var = Hashtbl.create 64
  ; disequalities = Dynarray.create ()
  ; reasons = Dynarray.create ()
  ; frames = []
  ; last_conflict = None
  }
;;

let rational (q : Term.rational) = Rational.of_big_frac ~num:q.num ~den:q.den

let rec collect_leaves acc (term : Term.t) =
  if not (Sort.equal term.sort Sort.real)
  then failwith "Lra_adapter: non-Real term in an LRA linear form";
  match term.node with
  | Term.Real_const _ -> acc
  | Term.Real_arith lin ->
    Iarr.fold (fun acc (child, _) -> collect_leaves acc child) acc lin.coeffs
  | Term.App _ -> Term.Set.add term acc
  | Term.Bool_const _
  | Term.Int_const _
  | Term.Arith _
  | Term.Le _
  | Term.Eq _
  | Term.Not _
  | Term.And _
  | Term.Or _
  | Term.Ite _ -> failwith "Lra_adapter: nonlinear or non-leaf Real term"
;;

let ensure_leaves t terms =
  Term.Set.iter
    (fun term ->
      if not (Term.Table.mem t.var_of_term term)
      then (
        let var = Lra.new_var t.lra in
        Term.Table.add t.var_of_term term var;
        Hashtbl.add t.term_of_var var term))
    terms
;;

let flatten t (term : Term.t) =
  match term.node with
  | Term.Real_const q -> { coeffs = []; constant = rational q }
  | Term.Real_arith lin ->
    { coeffs =
        Iarr.to_list lin.coeffs
        |> List.map (fun (child, coeff) ->
          Term.Table.find t.var_of_term child, rational coeff)
    ; constant = rational lin.const
    }
  | Term.App _ when Sort.equal term.sort Sort.real ->
    { coeffs = [ Term.Table.find t.var_of_term term, Rational.one ]
    ; constant = Rational.zero
    }
  | _ -> failwith "Lra_adapter: expected a linear Real term"
;;

let subtract a b =
  { coeffs = a.coeffs @ List.map (fun (v, q) -> v, Rational.neg q) b.coeffs
  ; constant = Rational.sub a.constant b.constant
  }
;;

let internalize_term t term =
  let leaves = collect_leaves Term.Set.empty term in
  ensure_leaves t leaves
;;

let equality t a b =
  internalize_term t a;
  internalize_term t b;
  subtract (flatten t a) (flatten t b)
;;

let register_atom t atom term =
  if not (Atom.Table.mem t.term_of_atom atom)
  then (
    Atom.Table.add t.term_of_atom atom term;
    Term.Table.replace t.atom_of_term term atom);
  match term.node with
  | Term.Le arg when Sort.equal arg.sort Sort.real -> internalize_term t arg
  | Term.Eq (a, b) when Sort.equal a.sort Sort.real ->
    internalize_term t a;
    internalize_term t b
  | _ -> failwith "Lra_adapter.register_atom: atom is outside LRA"
;;

let constraint_of_linear linear comparison =
  { Lra.coeffs = linear.coeffs
  ; comparison
  ; rhs = Rational.neg linear.constant
  }
;;

let assert_convex t linear comparison premise =
  match
    Lra.assert_constraint t.lra (constraint_of_linear linear comparison) ~premise
  with
  | Lra.Asserted | Lra.Immediate_conflict _ -> ()
  | Lra.Split _ -> failwith "Lra_adapter: convex constraint unexpectedly requested split"
;;

let record_disequality t ~eq ~lhs ~rhs linear premise =
  match
    Lra.assert_constraint
      t.lra
      (constraint_of_linear linear Lra.Ne)
      ~premise
  with
  | Lra.Split (lt, gt) -> Dynarray.add_last t.disequalities { eq; lhs; rhs; lt; gt }
  | Lra.Asserted | Lra.Immediate_conflict _ ->
    failwith "Lra_adapter: disequality was silently made convex"
;;

let assert_term t term ~positive ~premise =
  match term.Term.node with
  | Term.Le arg when Sort.equal arg.sort Sort.real ->
    assert_convex t (flatten t arg) (if positive then Lra.Le else Lra.Gt) premise
  | Term.Eq (a, b) when Sort.equal a.sort Sort.real ->
    let linear = equality t a b in
    if positive
    then assert_convex t linear Lra.Eq premise
    else record_disequality t ~eq:term ~lhs:a ~rhs:b linear premise
  | _ -> failwith "Lra_adapter.assert_lit: atom is outside LRA"
;;

let assert_lit t lit =
  match Atom.Table.find_opt t.term_of_atom (Lit.atom lit) with
  | Some term -> assert_term t term ~positive:(Lit.sign lit) ~premise:(Fabric.Real lit)
  | None -> failwith "Lra_adapter.assert_lit: unregistered atom"
;;

let checked_premises premises =
  if premises = []
  then failwith "Lra_adapter: empty conflict premise set (unsound)";
  premises
;;

let fabric_conflict t (conflict : Fabric.justification Lra.conflict) =
  let premises = checked_premises conflict.premises in
  t.last_conflict <- Some (conflict.farkas, premises);
  { Fabric.Explanation.premises; rule = Explanation.Rule_tag.Lia_farkas }
;;

let ordinary_explanation (explanation : Fabric.Explanation.t) =
  let premises =
    List.map
      (function
        | Fabric.Real lit -> lit
        | Fabric.Fabric _ ->
          failwith "Lra_adapter: fabric premise crossed the direct theory seam")
      explanation.premises
  in
  { Explanation.premises; rule = explanation.rule }
;;

let model_values t =
  let values = Hashtbl.create 64 in
  List.iter (fun (var, value) -> Hashtbl.add values var value) (Lra.model t.lra);
  values
;;

let eval_constraint values (constraint_ : Lra.constraint_) =
  List.fold_left
    (fun acc (var, coeff) ->
      Rational.add acc (Rational.mul coeff (Hashtbl.find values var)))
    Rational.zero
    constraint_.coeffs
;;

let violated_disequality t =
  let values = model_values t in
  Dynarray.find_map
    (fun disequality ->
      let lt_value = eval_constraint values disequality.lt in
      let gt_value = eval_constraint values disequality.gt in
      if Rational.equal lt_value disequality.lt.rhs
         && Rational.equal gt_value disequality.gt.rhs
      then
        Some
          [ disequality.eq
          ; Context.lt t.ctx disequality.lhs disequality.rhs
          ; Context.gt t.ctx disequality.lhs disequality.rhs
          ]
      else None)
    t.disequalities
;;

let check_fabric t effort =
  match Lra.check t.lra with
  | Lra.Unsat conflict -> Fabric.Conflict (fabric_conflict t conflict)
  | Lra.Sat ->
    (match effort with
     | Theory.Propagate -> Fabric.Propagations []
     | Theory.Final ->
       (match violated_disequality t with
        | Some split -> Fabric.Split split
        | None -> Fabric.Sat))
;;

let check t effort =
  match check_fabric t effort with
  | Fabric.Sat -> Theory.Sat
  | Fabric.Propagations lits -> Theory.Propagations lits
  | Fabric.Conflict explanation -> Theory.Conflict (ordinary_explanation explanation)
  | Fabric.Split terms -> Theory.Split terms
  | Fabric.Lemma lemma -> Theory.Lemma lemma
;;

let explain_fabric _t _lit =
  failwith "Lra_adapter.explain: LRA does not currently propagate literals"
;;

let explain t lit = ordinary_explanation (explain_fabric t lit)

let flatten_opt t term =
  try
    let leaves = collect_leaves Term.Set.empty term in
    if Term.Set.for_all (Term.Table.mem t.var_of_term) leaves
    then Some (flatten t term)
    else None
  with
  | Failure _ | Not_found -> None
;;

let fixed_bounds t term =
  match flatten_opt t term with
  | None -> None
  | Some linear ->
    (match Lra.fixed_value t.lra ~coeffs:linear.coeffs ~constant:linear.constant with
     | None -> None
     | Some (value, lower, upper) ->
       Some { Fabric.value = Rational.to_string value; lower; upper })
;;

let fabric_verify t term value lower upper =
  match lower, upper, flatten_opt t term with
  | Fabric.Real lower_lit, Fabric.Real upper_lit, Some linear ->
    (try
       let value = Rational.of_string value in
       match
         ( Lra.oriented_bound
             t.lra
             ~coeffs:linear.coeffs
             ~constant:linear.constant
             `Lower
         , Lra.oriented_bound
             t.lra
             ~coeffs:linear.coeffs
             ~constant:linear.constant
             `Upper )
       with
       | Some (Fabric.Real actual_lower, lower_value),
         Some (Fabric.Real actual_upper, upper_value) ->
         Lit.equal actual_lower lower_lit
         && Lit.equal actual_upper upper_lit
         && Rational.equal lower_value value
         && Rational.equal upper_value value
       | _ -> false
     with
     | Invalid_argument _ -> false)
  | _ -> false
;;

let notify_eq t ~edge_id eq =
  match eq.Term.node with
  | Term.Bool_const true -> ()
  | Term.Bool_const false -> failwith "Lra_adapter.notify_eq: contradictory equality"
  | Term.Eq (a, b) when Sort.equal a.sort Sort.real ->
    let linear = equality t a b in
    if linear.coeffs = []
    then (
      if not (Rational.is_zero linear.constant)
      then failwith "Lra_adapter.notify_eq: contradictory constant equality")
    else assert_convex t linear Lra.Eq (Fabric.Fabric edge_id)
  | _ -> failwith "Lra_adapter.notify_eq: expected a Real equality"
;;

let model t =
  Lra.model t.lra
  |> List.map (fun (var, value) ->
    let term = Hashtbl.find t.term_of_var var in
    let q =
      Term.rational_of_frac_big
        ~num:(Rational.num_bigint value)
        ~den:(Rational.den_bigint value)
    in
    term, Model.Real q)
  |> Model.of_alist
;;

let push t =
  Lra.push t.lra;
  t.frames <- (Dynarray.length t.disequalities, Dynarray.length t.reasons) :: t.frames
;;

let pop t n =
  Lra.pop t.lra n;
  if n < 0 then invalid_arg "Lra_adapter.pop: negative frame count";
  let rec unwind k frames disequalities reasons =
    if k = 0
    then disequalities, reasons, frames
    else
      match frames with
      | [] -> invalid_arg "Lra_adapter.pop: too many frames"
      | (d, r) :: rest -> unwind (k - 1) rest d r
  in
  let d, r, frames =
    unwind n t.frames (Dynarray.length t.disequalities) (Dynarray.length t.reasons)
  in
  Dynarray.truncate t.disequalities d;
  Dynarray.truncate t.reasons r;
  t.frames <- frames
;;

type checkpoint =
  { engine : Lra.checkpoint
  ; disequalities : int
  ; reasons : int
  }

let checkpoint t =
  if t.frames <> []
  then failwith "Lra_adapter.checkpoint: expected a single base frame";
  { engine = Lra.checkpoint t.lra
  ; disequalities = Dynarray.length t.disequalities
  ; reasons = Dynarray.length t.reasons
  }
;;

let rewind_to_checkpoint t checkpoint =
  if t.frames <> []
  then failwith "Lra_adapter.rewind_to_checkpoint: expected a single base frame";
  Lra.rewind_to_checkpoint t.lra checkpoint.engine;
  Dynarray.truncate t.disequalities checkpoint.disequalities;
  Dynarray.truncate t.reasons checkpoint.reasons
;;

let clear_last_conflict t = t.last_conflict <- None

let last_conflict_core t =
  match t.last_conflict with
  | None -> None
  | Some (farkas, premises) ->
    let rec terms acc = function
      | [] -> Some (List.rev acc)
      | Fabric.Real lit :: rest ->
        (match Atom.Table.find_opt t.term_of_atom (Lit.atom lit) with
         | Some term -> terms ((term, Lit.sign lit) :: acc) rest
         | None -> None)
      | Fabric.Fabric _ :: _ -> None
    in
    (match terms [] premises with
     | None -> None
     | Some atoms ->
       let has_equality =
         List.exists
           (fun (term, _) ->
             match term.Term.node with
             | Term.Eq (a, _) -> Sort.equal a.sort Sort.real
             | _ -> false)
           atoms
       in
       let farkas =
         if has_equality || farkas = [] || List.compare_lengths farkas atoms <> 0
         then None
         else Some farkas
       in
       Some { farkas; atoms })
;;

let is_poisoned t = Lra.is_poisoned t.lra
let pivot_count t = Lra.pivot_count t.lra
