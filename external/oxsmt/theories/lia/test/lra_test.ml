(* Self-test for the additive linear-real-arithmetic engine.

   This test stays below the frozen term/sort layer: constraints use exact [Rational.t]
   coefficients and LRA variables directly.  Its two independent checks are:

   - Fourier--Motzkin elimination over rational half-planes, including a separate
     strictness bit, decides thousands of small random systems without using the simplex;
   - every returned conflict is reconstructed from its public oriented half-planes.  The
     checker verifies that each row is an orientation implied by its premise, that every
     multiplier is nonnegative, and that the weighted sum cancels all variables and has
     a strictly positive delta-rational constant.

   Fixed seeds and exact arithmetic make the suite deterministic. *)

open Oxsmt_lia

let checks = ref 0
let failures = ref 0

let check name condition =
  incr checks;
  if not condition
  then (
    incr failures;
    Printf.printf "  FAIL %s\n" name)
;;

let q = Rational.of_int
let qf = Rational.of_frac

let normalize_coeffs coeffs =
  let sorted = List.stable_sort (fun (a, _) (b, _) -> Int.compare a b) coeffs in
  let rec loop acc = function
    | [] -> List.rev acc
    | (v, c) :: rest ->
      let rec gather sum = function
        | (v', c') :: tail when v = v' -> gather (Rational.add sum c') tail
        | tail -> sum, tail
      in
      let sum, rest = gather c rest in
      if Rational.is_zero sum then loop acc rest else loop ((v, sum) :: acc) rest
  in
  loop [] sorted
;;

let equal_coeffs a b =
  List.equal
    (fun (va, ca) (vb, cb) -> va = vb && Rational.equal ca cb)
    (normalize_coeffs a)
    (normalize_coeffs b)
;;

let negate_coeffs coeffs =
  List.map (fun (v, c) -> v, Rational.neg c) coeffs |> normalize_coeffs
;;

type recorded =
  { token : int
  ; atom : Lra.constraint_
  }

type fixture =
  { solver : int Lra.t
  ; vars : Lra.var array
  ; by_token : (int, Lra.constraint_) Hashtbl.t
  ; mutable active : recorded list
  ; mutable frames : recorded list list
  ; mutable next_token : int
  }

let make_fixture n =
  let solver = Lra.create () in
  let vars = Array.init n (fun _ -> Lra.new_var solver) in
  { solver
  ; vars
  ; by_token = Hashtbl.create 32
  ; active = []
  ; frames = []
  ; next_token = 0
  }
;;

let make_atom coeffs comparison rhs =
  { Lra.coeffs = normalize_coeffs coeffs; comparison; rhs }
;;

let assert_atom fx atom =
  let token = fx.next_token in
  fx.next_token <- token + 1;
  Hashtbl.add fx.by_token token atom;
  let result = Lra.assert_constraint fx.solver atom ~premise:token in
  (match result with
   | Lra.Asserted | Lra.Immediate_conflict _ ->
     fx.active <- { token; atom } :: fx.active
   | Lra.Split _ -> ());
  token, result
;;

let assert_c fx coeffs comparison rhs =
  assert_atom fx (make_atom coeffs comparison rhs)
;;

let push fx =
  Lra.push fx.solver;
  fx.frames <- fx.active :: fx.frames
;;

let pop fx n =
  let rec restore active frames remaining =
    if remaining = 0
    then active, frames
    else
      match frames with
      | saved :: rest -> restore saved rest (remaining - 1)
      | [] -> invalid_arg "lra_test: pop below root"
  in
  let active, frames = restore fx.active fx.frames n in
  Lra.pop fx.solver n;
  fx.active <- active;
  fx.frames <- frames
;;

(* Public proof rows use [sum coeff*x + constant <= 0].  A positive delta coefficient
   represents the infinitesimal added when a strict input [sum coeff*x < rhs] is lowered
   to a non-strict simplex bound. *)
let expected_half_planes (atom : Lra.constraint_) =
  let upper ~strict =
    let k = if strict then Rational.one else Rational.zero in
    atom.coeffs, Delta.make (Rational.neg atom.rhs) k
  in
  let lower ~strict =
    let k = if strict then Rational.one else Rational.zero in
    negate_coeffs atom.coeffs, Delta.make atom.rhs k
  in
  match atom.comparison with
  | Lra.Le -> [ upper ~strict:false ]
  | Lra.Lt -> [ upper ~strict:true ]
  | Lra.Ge -> [ lower ~strict:false ]
  | Lra.Gt -> [ lower ~strict:true ]
  | Lra.Eq -> [ upper ~strict:false; lower ~strict:false ]
  | Lra.Ne -> []
;;

let half_plane_matches_atom atom coeffs constant =
  List.exists
    (fun (expected_coeffs, expected_constant) ->
      equal_coeffs coeffs expected_coeffs && Delta.equal constant expected_constant)
    (expected_half_planes atom)
;;

let conflict_valid fx (conflict : int Lra.conflict) =
  let premises = conflict.premises
  and multipliers = conflict.farkas
  and half_planes = conflict.half_planes in
  let lengths_match =
    List.length premises = List.length multipliers
    && List.length premises = List.length half_planes
    && premises <> []
  in
  if not lengths_match
  then false
  else (
    let variables = Array.make (Array.length fx.vars) Rational.zero in
    let constant = ref Delta.zero in
    let rows_valid = ref true in
    List.iter2
      (fun (plane : int Lra.half_plane) multiplier ->
        if Rational.sign multiplier < 0 then rows_valid := false;
        let atom = Hashtbl.find_opt fx.by_token plane.premise in
        (match atom with
         | None -> rows_valid := false
         | Some atom ->
           if not (half_plane_matches_atom atom plane.coeffs plane.constant)
           then rows_valid := false);
        if not (List.exists (fun active -> active.token = plane.premise) fx.active)
        then rows_valid := false;
        List.iter
          (fun (v, coeff) ->
            if v < 0 || v >= Array.length variables
            then rows_valid := false
            else
              variables.(v)
              <- Rational.add variables.(v) (Rational.mul multiplier coeff))
          (normalize_coeffs plane.coeffs);
        constant := Delta.add !constant (Delta.scale multiplier plane.constant))
      half_planes
      multipliers;
    let premise_projection_valid =
      List.for_all2
        (fun premise (plane : int Lra.half_plane) -> premise = plane.premise)
        premises
        half_planes
    in
    !rows_valid
    && premise_projection_valid
    && Array.for_all Rational.is_zero variables
    && Delta.lt Delta.zero !constant)
;;

let eval coeffs assignment =
  List.fold_left
    (fun sum (v, coeff) -> Rational.add sum (Rational.mul coeff assignment.(v)))
    Rational.zero
    coeffs
;;

let atom_holds assignment (atom : Lra.constraint_) =
  let cmp = Rational.compare (eval atom.coeffs assignment) atom.rhs in
  match atom.comparison with
  | Lra.Le -> cmp <= 0
  | Lra.Lt -> cmp < 0
  | Lra.Ge -> cmp >= 0
  | Lra.Gt -> cmp > 0
  | Lra.Eq -> cmp = 0
  | Lra.Ne -> cmp <> 0
;;

let model_assignment fx =
  let assignment = Array.make (Array.length fx.vars) None in
  let valid = ref true in
  List.iter
    (fun (v, value) ->
      if v < 0 || v >= Array.length assignment || Option.is_some assignment.(v)
      then valid := false
      else assignment.(v) <- Some value)
    (Lra.model fx.solver);
  Array.iteri
    (fun i value ->
      match value with
      | None -> valid := false
      | Some value ->
        if not (Rational.equal value (Lra.value fx.solver fx.vars.(i)))
        then valid := false)
    assignment;
  let values = Array.map (Option.value ~default:Rational.zero) assignment in
  !valid, values
;;

let model_valid fx =
  let complete, assignment = model_assignment fx in
  complete && List.for_all (fun r -> atom_holds assignment r.atom) fx.active
;;

let expect_sat fx name =
  match Lra.check fx.solver with
  | Lra.Sat ->
    check (name ^ ": exact model satisfies every live constraint") (model_valid fx)
  | Lra.Unsat _ -> check (name ^ ": expected sat") false
;;

let expect_unsat fx name =
  match Lra.check fx.solver with
  | Lra.Sat ->
    check (name ^ ": expected unsat") false;
    None
  | Lra.Unsat conflict ->
    check (name ^ ": independent Farkas check") (conflict_valid fx conflict);
    Some conflict
;;

let test_hand_cases () =
  print_endline "hand cases:";
  (* Fractional coefficients and rhs are kept exact; no integer gcd/ceil normalization is
     valid here. *)
  (let fx = make_fixture 2 in
   let x = fx.vars.(0)
   and y = fx.vars.(1) in
   ignore (assert_c fx [ x, qf 1 2; y, qf 1 3 ] Lra.Eq (qf 7 6));
   ignore (assert_c fx [ x, q 1 ] Lra.Gt (q 1));
   ignore (assert_c fx [ y, q 1 ] Lra.Ge Rational.zero);
   ignore (assert_c fx [ y, q 1 ] Lra.Lt (q 2));
   expect_sat fx "fractional equality with an open interval");
  (let fx = make_fixture 1 in
   let x = fx.vars.(0) in
   ignore (assert_c fx [ x, q 1 ] Lra.Ge (q 1));
   ignore (assert_c fx [ x, q 1 ] Lra.Lt (q 1));
   ignore (expect_unsat fx "x>=1 and x<1"));
  (let fx = make_fixture 3 in
   let x = fx.vars.(0)
   and y = fx.vars.(1)
   and z = fx.vars.(2) in
   ignore (assert_c fx [ x, q 1; y, q (-1) ] Lra.Lt Rational.zero);
   ignore (assert_c fx [ y, q 1; z, q (-1) ] Lra.Le Rational.zero);
   ignore (assert_c fx [ z, q 1; x, q (-1) ] Lra.Le Rational.zero);
   ignore (expect_unsat fx "strict cycle x<y<=z<=x"));
  (let fx = make_fixture 2 in
   let x = fx.vars.(0)
   and y = fx.vars.(1) in
   ignore (assert_c fx [ x, q 1; y, q (-1) ] Lra.Eq Rational.zero);
   ignore (assert_c fx [ x, q 1 ] Lra.Lt Rational.zero);
   ignore (assert_c fx [ y, q 1 ] Lra.Ge Rational.zero);
   ignore (expect_unsat fx "equality orientation is retained in the proof"));
  (* Over the reals, [2*x <= 3] permits x=3/2 and values arbitrarily close below it. *)
  (let fx = make_fixture 1 in
   let x = fx.vars.(0) in
   ignore (assert_c fx [ x, q 2 ] Lra.Le (q 3));
   ignore (assert_c fx [ x, q 1 ] Lra.Gt (qf 7 5));
   expect_sat fx "2*x<=3 is not integer gcd-tightened");
  (let fx = make_fixture 1 in
   let x = fx.vars.(0) in
   ignore (assert_c fx [ x, q 2 ] Lra.Le (q 3));
   ignore (assert_c fx [ x, q 1 ] Lra.Gt (qf 3 2));
   ignore (expect_unsat fx "2*x<=3 and x>3/2"));
  (* Repeated variables must be summed exactly, including cancellation. *)
  let fx = make_fixture 1 in
  let x = fx.vars.(0) in
  ignore (assert_c fx [ x, qf 1 2; x, qf 1 3; x, qf (-5) 6 ] Lra.Eq Rational.zero);
  expect_sat fx "repeated rational coefficients cancel"
;;

let test_disequality_split () =
  print_endline "disequality split hook:";
  let fx = make_fixture 1 in
  let x = fx.vars.(0) in
  let atom = make_atom [ x, qf 2 3 ] Lra.Ne (qf 5 7) in
  match snd (assert_atom fx atom) with
  | Lra.Split (left, right) ->
    check
      "!= split is the exact < or > disjunction"
      (left.comparison = Lra.Lt
       && right.comparison = Lra.Gt
       && equal_coeffs left.coeffs atom.coeffs
       && equal_coeffs right.coeffs atom.coeffs
       && Rational.equal left.rhs atom.rhs
       && Rational.equal right.rhs atom.rhs);
    check
      "!= split hook does not mutate the conjunctive engine"
      (match Lra.check fx.solver with
       | Lra.Sat -> model_valid fx
       | Lra.Unsat _ -> false)
  | Lra.Asserted | Lra.Immediate_conflict _ ->
    check "!= returns a split instead of being asserted conjunctively" false
;;

(* Independent rational Fourier--Motzkin decision procedure.  [strict] distinguishes
   [a*x+b < 0] from [a*x+b <= 0]; eliminating a variable makes a derived row strict iff
   either contributing row was strict. *)
type oracle_row =
  { a : Rational.t array
  ; b : Rational.t
  ; strict : bool
  }

let oracle_rows n (atom : Lra.constraint_) =
  let row coeffs b strict =
    let a = Array.make n Rational.zero in
    List.iter (fun (v, c) -> a.(v) <- Rational.add a.(v) c) coeffs;
    { a; b; strict }
  in
  let upper strict = row atom.coeffs (Rational.neg atom.rhs) strict in
  let lower strict = row (negate_coeffs atom.coeffs) atom.rhs strict in
  match atom.comparison with
  | Lra.Le -> [ upper false ]
  | Lra.Lt -> [ upper true ]
  | Lra.Ge -> [ lower false ]
  | Lra.Gt -> [ lower true ]
  | Lra.Eq -> [ upper false; lower false ]
  | Lra.Ne -> invalid_arg "oracle_rows: disequality is a split, not a conjunction"
;;

let scale_row scale row =
  { a = Array.map (Rational.mul scale) row.a
  ; b = Rational.mul scale row.b
  ; strict = row.strict
  }
;;

let add_rows left right =
  { a = Array.map2 Rational.add left.a right.a
  ; b = Rational.add left.b right.b
  ; strict = left.strict || right.strict
  }
;;

let eliminate variable rows =
  let positive, negative, zero =
    List.fold_left
      (fun (positive, negative, zero) row ->
        let sign = Rational.sign row.a.(variable) in
        if sign > 0
        then row :: positive, negative, zero
        else if sign < 0
        then positive, row :: negative, zero
        else positive, negative, row :: zero)
      ([], [], [])
      rows
  in
  List.fold_left
    (fun result upper ->
      List.fold_left
        (fun result lower ->
          let derived =
            add_rows
              (scale_row (Rational.neg lower.a.(variable)) upper)
              (scale_row upper.a.(variable) lower)
          in
          derived.a.(variable) <- Rational.zero;
          derived :: result)
        result
        negative)
    zero
    positive
;;

let oracle_feasible n atoms =
  let rows = List.concat_map (oracle_rows n) atoms in
  let rows =
    let current = ref rows in
    for variable = 0 to n - 1 do
      current := eliminate variable !current
    done;
    !current
  in
  List.for_all
    (fun row ->
      let comparison = Rational.compare row.b Rational.zero in
      if row.strict then comparison < 0 else comparison <= 0)
    rows
;;

let rng = Random.State.make [| 0x4c52_4101; 0x2026_0717 |]
let rand_range lo hi = lo + Random.State.int rng (hi - lo + 1)

let random_rational ~nonzero =
  let rec choose () =
    let numerator = rand_range (-5) 5 in
    if nonzero && numerator = 0 then choose () else qf numerator (rand_range 1 4)
  in
  choose ()
;;

let random_comparison () =
  match Random.State.int rng 9 with
  | 0 | 1 -> Lra.Le
  | 2 | 3 -> Lra.Lt
  | 4 | 5 -> Lra.Ge
  | 6 | 7 -> Lra.Gt
  | _ -> Lra.Eq
;;

let random_atom fx =
  let coeffs = ref [] in
  Array.iter
    (fun v ->
      if Random.State.bool rng
      then coeffs := (v, random_rational ~nonzero:true) :: !coeffs)
    fx.vars;
  if !coeffs = []
  then coeffs := [ fx.vars.(Random.State.int rng (Array.length fx.vars)), random_rational ~nonzero:true ];
  (* Occasionally add a duplicate id.  Both the independent oracle and the engine must
     sum it exactly rather than use last-write-wins. *)
  if Random.State.int rng 5 = 0
  then (
    let v, _ = List.hd !coeffs in
    coeffs := (v, random_rational ~nonzero:true) :: !coeffs);
  make_atom !coeffs (random_comparison ()) (random_rational ~nonzero:false)
;;

let test_random_cross_check () =
  print_endline "random Fourier--Motzkin cross-check:";
  let systems = 3000 in
  let verdict_mismatches = ref 0
  and bad_models = ref 0
  and bad_conflicts = ref 0
  and immediate_conflicts = ref 0
  and sat_count = ref 0
  and unsat_count = ref 0 in
  for _ = 1 to systems do
    let n = 1 + Random.State.int rng 2 in
    let fx = make_fixture n in
    let atoms = ref [] in
    (* Rational box bounds keep returned models modest while the oracle itself remains a
       full real-feasibility procedure rather than a grid enumeration. *)
    Array.iter
      (fun v ->
        atoms := make_atom [ v, q 1 ] Lra.Ge (q (-3)) :: !atoms;
        atoms := make_atom [ v, q 1 ] Lra.Le (q 3) :: !atoms)
      fx.vars;
    for _ = 1 to 1 + Random.State.int rng 5 do
      atoms := random_atom fx :: !atoms
    done;
    let atoms = List.rev !atoms in
    List.iter
      (fun atom ->
        match snd (assert_atom fx atom) with
        | Lra.Immediate_conflict conflict ->
          incr immediate_conflicts;
          if not (conflict_valid fx conflict) then incr bad_conflicts
        | Lra.Asserted -> ()
        | Lra.Split _ -> incr verdict_mismatches)
      atoms;
    let expected = oracle_feasible n atoms in
    (match Lra.check fx.solver with
     | Lra.Sat ->
       incr sat_count;
       if not expected then incr verdict_mismatches;
       if not (model_valid fx) then incr bad_models
     | Lra.Unsat conflict ->
       incr unsat_count;
       if expected then incr verdict_mismatches;
       if not (conflict_valid fx conflict) then incr bad_conflicts)
  done;
  check "random systems: zero feasibility mismatches" (!verdict_mismatches = 0);
  check "random systems: every sat model satisfies every atom exactly" (!bad_models = 0);
  check
    "random systems: every infeasibility certificate proves contradiction"
    (!bad_conflicts = 0);
  check "random systems: sat arm is non-vacuous" (!sat_count > systems / 10);
  check "random systems: unsat arm is non-vacuous" (!unsat_count > systems / 10);
  Printf.printf
    "    (%d systems: sat=%d unsat=%d immediate=%d mismatches=%d bad-models=%d bad-conflicts=%d)\n"
    systems
    !sat_count
    !unsat_count
    !immediate_conflicts
    !verdict_mismatches
    !bad_models
    !bad_conflicts
;;

let test_push_pop () =
  print_endline "push/pop:";
  (let fx = make_fixture 2 in
   let x = fx.vars.(0)
   and y = fx.vars.(1) in
   ignore (assert_c fx [ x, q 1 ] Lra.Le (q 2));
   expect_sat fx "root x<=2";
   push fx;
   ignore (assert_c fx [ x, q 1 ] Lra.Gt (q 3));
   ignore (expect_unsat fx "pushed x>3 conflicts with root x<=2");
   pop fx 1;
   expect_sat fx "pop removes pushed conflict";
   push fx;
   ignore (assert_c fx [ y, q 1 ] Lra.Eq (qf 1 2));
   push fx;
   ignore (assert_c fx [ y, q 1 ] Lra.Lt (qf 1 2));
   ignore (expect_unsat fx "nested strict/equality conflict");
   pop fx 2;
   expect_sat fx "multi-pop restores root model");
  (* A contradiction below a later frame must not disappear when that later frame pops. *)
  let fx = make_fixture 1 in
  let x = fx.vars.(0) in
  ignore (assert_c fx [ x, q 1 ] Lra.Le Rational.zero);
  ignore (assert_c fx [ x, q 1 ] Lra.Gt Rational.zero);
  ignore (expect_unsat fx "root strict conflict before empty frame");
  push fx;
  pop fx 1;
  ignore (expect_unsat fx "root strict conflict survives push/pop")
;;

let model_is_rejected solver =
  match Lra.model solver with
  | exception Invalid_argument _ -> true
  | exception _ | _ -> false
;;

let test_model_readiness () =
  print_endline "model readiness:";
  let fx = make_fixture 1 in
  let x = fx.vars.(0) in
  ignore (assert_c fx [ x, q 1 ] Lra.Gt Rational.zero);
  expect_sat fx "initial model is available after Sat";
  ignore (assert_c fx [ x, q 1 ] Lra.Lt (q 2));
  check
    "assertion invalidates the old model until re-check"
    (model_is_rejected fx.solver);
  expect_sat fx "model is available after assertion and re-check";
  let y = Lra.new_var fx.solver in
  check
    "allocating a variable invalidates the old total model"
    (model_is_rejected fx.solver);
  check
    "re-check after variable allocation stays Sat"
    (match Lra.check fx.solver with
     | Lra.Sat -> true
     | Lra.Unsat _ -> false);
  let model = Lra.model fx.solver in
  check
    "re-checked model contains the newly allocated unconstrained variable"
    (List.length model = 2 && Option.is_some (List.assoc_opt y model));
  (* A non-unit row allocates an internal slack between [x] and [y].  Problem-variable
     ids therefore need not be dense, and neither assertion nor model extraction may
     confuse [y] with that slack. *)
  let solver = Lra.create () in
  let x = Lra.new_var solver in
  ignore
    (Lra.assert_constraint
       solver
       (make_atom [ x, q 2 ] Lra.Le Rational.zero)
       ~premise:0);
  let y = Lra.new_var solver in
  ignore
    (Lra.assert_constraint
       solver
       (make_atom [ y, q 1 ] Lra.Eq (qf 3 2))
       ~premise:1);
  check
    "variable allocated after a slack has its own exact model value"
    (match Lra.check solver with
     | Lra.Unsat _ -> false
     | Lra.Sat ->
       (match List.assoc_opt y (Lra.model solver) with
        | None -> false
        | Some value -> Rational.equal value (qf 3 2)))
;;

let constraint_digest (atom : Lra.constraint_) =
  let comparison =
    match atom.comparison with
    | Lra.Le -> "<="
    | Lra.Lt -> "<"
    | Lra.Ge -> ">="
    | Lra.Gt -> ">"
    | Lra.Eq -> "="
    | Lra.Ne -> "!="
  in
  let coeffs =
    normalize_coeffs atom.coeffs
    |> List.map (fun (v, c) -> Printf.sprintf "%d:%s" v (Rational.to_string c))
    |> String.concat ","
  in
  Printf.sprintf "%s%s%s" coeffs comparison (Rational.to_string atom.rhs)
;;

let conflict_digest (conflict : int Lra.conflict) =
  List.map2
    (fun (plane : int Lra.half_plane) multiplier ->
      let coeffs =
        normalize_coeffs plane.coeffs
        |> List.map (fun (v, c) -> Printf.sprintf "%d:%s" v (Rational.to_string c))
        |> String.concat ","
      in
      Printf.sprintf
        "%d[%s;%s]*%s"
        plane.premise
        coeffs
        (Delta.to_string plane.constant)
        (Rational.to_string multiplier))
    conflict.half_planes
    conflict.farkas
  |> String.concat "|"
;;

let model_digest model =
  List.sort (fun (a, _) (b, _) -> Int.compare a b) model
  |> List.map (fun (v, value) -> Printf.sprintf "%d=%s" v (Rational.to_string value))
  |> String.concat ";"
;;

let test_determinism () =
  print_endline "determinism:";
  let inputs vars =
    let x = vars.(0)
    and y = vars.(1) in
    [ make_atom [ x, qf 2 3; y, qf (-5) 7 ] Lra.Ge (qf 1 4)
    ; make_atom [ x, q 1 ] Lra.Lt (qf 11 5)
    ; make_atom [ y, q 1 ] Lra.Gt (qf (-7) 6)
    ; make_atom [ x, qf 1 2; y, qf 3 4 ] Lra.Le (qf 13 8)
    ]
  in
  let run_sat () =
    let fx = make_fixture 2 in
    let atoms = inputs fx.vars in
    List.iter (fun atom -> ignore (assert_atom fx atom)) atoms;
    let result =
      match Lra.check fx.solver with
      | Lra.Sat -> "sat:" ^ model_digest (Lra.model fx.solver)
      | Lra.Unsat conflict -> "unsat:" ^ conflict_digest conflict
    in
    String.concat "/" (List.map constraint_digest atoms), result, Lra.pivot_count fx.solver
  in
  let sat1 = run_sat ()
  and sat2 = run_sat () in
  check "identical feasible input has identical model and pivot count" (sat1 = sat2);
  let run_unsat () =
    let fx = make_fixture 2 in
    let x = fx.vars.(0)
    and y = fx.vars.(1) in
    let atoms =
      [ make_atom [ x, q 1; y, q (-1) ] Lra.Lt Rational.zero
      ; make_atom [ y, q 1 ] Lra.Le Rational.zero
      ; make_atom [ x, q 1 ] Lra.Ge Rational.zero
      ]
    in
    List.iter (fun atom -> ignore (assert_atom fx atom)) atoms;
    let result =
      match Lra.check fx.solver with
      | Lra.Sat -> "sat:" ^ model_digest (Lra.model fx.solver)
      | Lra.Unsat conflict -> "unsat:" ^ conflict_digest conflict
    in
    result, Lra.pivot_count fx.solver
  in
  let unsat1 = run_unsat ()
  and unsat2 = run_unsat () in
  check "identical infeasible input has identical proof and pivot count" (unsat1 = unsat2)
;;

let test_adapter () =
  let open Oxsmt_core in
  print_endline "adapter:";
  let env = Env.create () in
  let ctx = Context.create env in
  let x =
    Context.const ctx (Env.declare_fun env "lra-adapter-x" (Rank.create [] Sort.real))
  in
  let zero = Context.real_const_big ctx ~num:Bigint.zero ~den:Bigint.one in
  let one = Context.real_const_big ctx ~num:Bigint.one ~den:Bigint.one in
  let alloc = Atom.create_allocator () in
  let adapter = Lra_adapter.create ctx env in
  let register_signed term =
    let atom_term, positive =
      match term.Term.node with
      | Term.Not atom -> atom, false
      | _ -> term, true
    in
    let atom = Atom.fresh alloc in
    Lra_adapter.register_atom adapter atom atom_term;
    Lit.make atom positive
  in
  let gt_zero = register_signed (Context.gt ctx x zero) in
  let lt_one = register_signed (Context.lt ctx x one) in
  Lra_adapter.assert_lit adapter gt_zero;
  Lra_adapter.assert_lit adapter lt_one;
  check
    "strict open interval is satisfiable"
    (match Lra_adapter.check adapter Theory.Final with
     | Theory.Sat -> true
     | Theory.Propagations _
     | Theory.Conflict _
     | Theory.Split _
     | Theory.Lemma _ -> false);
  let inside =
    match Model.value (Lra_adapter.model adapter) x with
    | Some (Model.Real value) ->
      let value = Rational.of_big_frac ~num:value.num ~den:value.den in
      Rational.compare Rational.zero value < 0 && Rational.compare value Rational.one < 0
    | Some (Model.Int _ | Model.Bool _ | Model.Uninterp _) | None -> false
  in
  check "strict open interval model is strictly interior" inside;
  let diseq_adapter = Lra_adapter.create ctx env in
  let eq = Context.eq ctx x zero in
  let eq_atom = Atom.fresh alloc in
  Lra_adapter.register_atom diseq_adapter eq_atom eq;
  Lra_adapter.push diseq_adapter;
  Lra_adapter.assert_lit diseq_adapter (Lit.make eq_atom false);
  check
    "violated disequality emits equality-guarded real trichotomy"
    (match Lra_adapter.check diseq_adapter Theory.Final with
     | Theory.Split [ guard; _; _ ] -> Term.equal guard eq
     | Theory.Sat
     | Theory.Propagations _
     | Theory.Conflict _
     | Theory.Split _
     | Theory.Lemma _ -> false);
  Lra_adapter.pop diseq_adapter 1;
  check
    "popping disequality permits equality again"
    (match Lra_adapter.check diseq_adapter Theory.Final with
     | Theory.Sat -> true
     | Theory.Propagations _
     | Theory.Conflict _
     | Theory.Split _
     | Theory.Lemma _ -> false)
;;

let () =
  print_endline "lra self-test:";
  test_hand_cases ();
  test_disequality_split ();
  test_push_pop ();
  test_model_readiness ();
  test_random_cross_check ();
  test_determinism ();
  test_adapter ();
  Printf.printf "\nlra self-test: %d checks, %d failure(s)\n" !checks !failures;
  if !failures > 0 then exit 1
;;
