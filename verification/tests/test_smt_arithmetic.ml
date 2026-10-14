module A = Vox_smt_arithmetic
module Z = Vox_smt_integer.Z
module Q = Vox_smt_integer.Q

let check condition message = if not condition then failwith message
let terms xs = List.map (fun (v, c) -> v, Z.of_int c) xs
let register solver literal xs bound =
  A.register solver ~literal (terms xs) (Z.of_int bound)

let expect_conflict solver =
  match A.check solver with
  | A.Conflict core -> core
  | A.Unknown -> failwith "unexpected unknown"
  | A.Consistent _ -> failwith "expected conflict"

let expect_consistent solver =
  match A.check solver with
  | A.Consistent result -> result
  | A.Unknown -> failwith "unexpected unknown"
  | A.Conflict _ -> failwith "expected consistent relaxation"

let basics () =
  let solver = A.create () in
  register solver 1 [17, 1] 2;
  register solver 2 [17, -1] (-3);
  register solver 3 [17, 1] 4;
  A.assert_literal solver 1;
  let result = expect_consistent solver in
  check
    (List.exists (fun (l, reason) -> l = 3 && reason = [|1|])
       result.propagations)
    "missing upper-bound propagation";
  check
    (List.exists (fun (l, reason) -> l = -2 && reason = [|1|])
       result.propagations)
    "missing complement propagation";
  A.push solver;
  A.assert_literal solver 2;
  check (expect_conflict solver = [|1; 2|]) "wrong conflict explanation";
  A.pop solver;
  ignore (expect_consistent solver);
  A.push solver;
  A.assert_literal solver (-1);
  check (expect_conflict solver = [|-1; 1|]) "opposite literals";
  A.push solver;
  A.pop solver;
  ignore (expect_conflict solver);
  A.pop solver;
  ignore (expect_consistent solver);
  A.reset solver;
  A.assert_literal solver 2;
  ignore (expect_consistent solver);
  A.reset solver;
  A.push solver;
  A.assert_literal solver (-2);
  ignore (expect_consistent solver);
  A.backtrack solver 0;
  check (A.level solver = 0) "backtrack level";
  A.assert_literal solver 2;
  ignore (expect_consistent solver)

let rounding_and_splits () =
  let solver = A.create () in
  register solver 1 [0, 2] 1;
  register solver 2 [0, -2] (-1);
  A.assert_literal solver 1;
  A.assert_literal solver 2;
  ignore (expect_conflict solver);
  let solver = A.create () in
  register solver 1 [0, 2; 1, 1] 1;
  register solver 2 [0, -2; 1, -1] (-1);
  register solver 3 [1, 1] 1;
  register solver 4 [1, -1] 0;
  List.iter (A.assert_literal solver) [1; 2; 3; 4];
  (match A.check ~phase:A.Propagation solver with
  | A.Consistent {model = []; split = None; _} -> ()
  | _ -> failwith "arithmetic propagation phase");
  let result = expect_consistent solver in
  check (result.split = None) "integer equality model was not repaired";
  register solver 5 [0, 1] 0;
  A.push solver;
  A.assert_literal solver 5;
  check ((expect_consistent solver).split = None) "integral branch";
  A.pop solver;
  A.push solver;
  A.assert_literal solver (-5);
  ignore (expect_conflict solver);
  A.pop solver;
  ignore (expect_consistent solver)

let divisibility () =
  let solver = A.create () in
  register solver 1 [0, 2; 1, 1] 1;
  register solver 2 [0, -2; 1, -1] (-1);
  register solver 3 [1, 1] 0;
  register solver 4 [1, -1] 0;
  List.iter (A.assert_literal solver) [1; 2; 3; 4];
  check (expect_conflict solver = [|1; 2; 3; 4|])
    "integer equality divisibility";
  A.reset solver;
  List.iter (A.assert_literal solver) [1; 2; 3];
  ignore (expect_consistent solver)

let interrupted_pivot () =
  for budget = 0 to 200 do
    let solver = A.create () in
    register solver 1 [0, 1] 1;
    register solver 2 [1, 1] 0;
    register solver 3 [0, -1; 1, -1] (-2);
    register solver 4 [0, 1] (-1);
    A.assert_literal solver 4;
    ignore (expect_consistent solver);
    A.reset solver;
    List.iter (A.assert_literal solver) [1; 2; 3];
    ignore (A.check ~budget solver);
    ignore (expect_conflict solver);
    A.reset solver;
    List.iter (A.assert_literal solver) [1; 3];
    ignore (expect_consistent solver)
  done

let divisibility_dependencies () =
  let count = 8 in
  let fixture () =
    let solver = A.create () and literal = ref 0 in
    let equality terms boundary =
      incr literal;
      register solver !literal terms boundary;
      incr literal;
      register solver !literal
        (List.map (fun (id, coefficient) -> id, -coefficient) terms)
        (-boundary)
    in
    equality [0, 1] 0;
    for index = 1 to count do
      equality [index, 1; index - 1, -1] 0
    done;
    equality [count + 1, 2; count, 1] 1;
    for literal = 1 to !literal do A.assert_literal solver literal done;
    solver
  in
  let expected = Array.init (2 * count + 4) (fun index -> index + 1) in
  let recover solver =
    check (expect_conflict solver = expected) "transitive divisibility core";
    A.reset solver;
    Array.iter (fun literal ->
      if literal > 2 then A.assert_literal solver literal) expected;
    ignore (expect_consistent solver);
    A.push solver;
    A.assert_literal solver 1;
    A.assert_literal solver 2;
    check (expect_conflict solver = expected) "restored divisibility core";
    A.pop solver;
    ignore (expect_consistent solver)
  in
  let work = ref 0 in
  ignore (A.check ~charge:(fun () -> incr work; true) (fixture ()));
  for budget = 0 to !work - 1 do
    let solver = fixture () in
    check (A.check ~budget solver = A.Unknown) "divisibility budget";
    recover solver;
    let solver = fixture () and remaining = ref budget in
    (match A.check ~charge:(fun () ->
       if !remaining = 0 then raise Exit;
       decr remaining;
       true) solver with
    | exception Exit -> ()
    | _ -> failwith "divisibility cancellation");
    recover solver
  done

type constraint_ = { literal : int; coefficients : int array; bound : int }

let holds model constraint_ literal =
  let sum = ref 0 in
  Array.iteri (fun i c -> sum := !sum + (c * model.(i)))
    constraint_.coefficients;
  let result = !sum <= constraint_.bound in
  if literal = constraint_.literal then result else not result

let enumerate variables radius predicate =
  let model = Array.make variables (-radius) in
  let rec search index =
    if index = variables
    then predicate model
    else
      let rec values value =
        if value > radius
        then false
        else begin
          model.(index) <- value;
          search (index + 1) || values (value + 1)
        end
      in
      values (-radius)
  in
  search 0

let randomized () =
  let cuts = ref 0 in
  let random = Random.State.make [|0x517; 0x1a4|] in
  for trial = 1 to 500 do
    let variables = 1 + Random.State.int random 3 in
    let solver = A.create () and inventory = Hashtbl.create 32 in
    let next = ref 0 and asserted = ref [] in
    let install coefficients bound =
      incr next;
      let literal = !next in
      let constraint_ = { literal; coefficients; bound } in
      Hashtbl.add inventory literal constraint_;
      register solver literal
        (Array.to_list (Array.mapi (fun v c -> v, c) coefficients)) bound;
      literal
    in
    let assert_ literal =
      asserted := literal :: !asserted;
      A.assert_literal solver literal
    in
    for v = 0 to variables - 1 do
      List.iter
        (fun sign ->
          let coefficients = Array.init variables
              (fun i -> if i = v then sign else 0) in
          assert_ (install coefficients 3))
        [-1; 1]
    done;
    for _ = 1 to 2 + Random.State.int random 8 do
      let coefficients = Array.init variables
          (fun _ -> Random.State.int random 7 - 3) in
      let bound = Random.State.int random 15 - 7 in
      let literal = install coefficients bound in
      assert_ (if Random.State.bool random then literal else -literal)
    done;
    let holds model literal =
      holds model (Hashtbl.find inventory (abs literal)) literal
    in
    let expected = enumerate variables 3
        (fun model -> List.for_all (holds model) !asserted) in
    let rec solve fuel =
      if fuel = 0 then failwith "integer branching did not terminate";
      match A.check ~cuts:true ~budget:1_000_000 solver with
      | A.Unknown -> failwith "randomized arithmetic exhausted budget"
      | A.Conflict core ->
        Array.iter
          (fun l -> check (List.mem l !asserted) "nonasserted conflict reason")
          core;
        check
          (not (enumerate variables 4
                  (fun model -> Array.for_all (holds model) core)))
          "satisfiable conflict explanation";
        false
      | A.Consistent result ->
        Option.iter (fun (terms, boundary, reasons) ->
          incr cuts;
          Array.iter (fun l ->
            check (List.mem l !asserted) "nonasserted cut reason") reasons;
          check (not (enumerate variables 4 (fun model ->
            Array.for_all (holds model) reasons
            && Z.compare
                (List.fold_left (fun sum (id, coefficient) ->
                  Z.add sum (Z.mul coefficient (Z.of_int model.(id))))
                  Z.zero terms) boundary > 0)))
            "invalid Gomory cut") result.cut;
        List.iter
          (fun (literal, reason) ->
            Array.iter
              (fun l -> check (List.mem l !asserted) "nonasserted propagation")
              reason;
            check
              (not (enumerate variables 4 (fun model ->
                        Array.for_all (holds model) reason
                        && not (holds model literal))))
              "invalid propagation explanation")
          result.propagations;
        (match result.split with
        | None ->
          let model = Array.make variables 0 in
          List.iter
            (fun (id, value) ->
              check (Q.is_integer value) "fractional integer model";
              model.(id) <- Option.get (Z.to_int_opt (Q.numerator value)))
            result.model;
          check (List.for_all (holds model) !asserted) "invalid integer model";
          true
        | Some (id, bound) ->
          let bound = Option.get (Z.to_int_opt bound) in
          let coefficients = Array.init variables
              (fun i -> if i = id then 1 else 0) in
          let literal = install coefficients bound in
          let branch literal =
            A.push solver;
            let saved = !asserted in
            assert_ literal;
            let result = solve (fuel - 1) in
            A.pop solver;
            asserted := saved;
            result
          in
          branch literal || branch (-literal))
    in
    let actual = solve 1000 in
    if actual <> expected
    then failwith (Printf.sprintf "Differential mismatch in trial %d" trial);
    for budget = 0 to 50 do
      ignore (A.check ~budget solver)
    done;
    check (solve 1000 = expected) "interrupted check changed satisfiability";
    check
      (A.check ~cancelled:(fun () -> true) solver = A.Unknown)
      "missed arithmetic cancellation"
  done;
  check (!cuts > 0) "random arithmetic did not exercise Gomory cuts"

let propagation_cache_lifetimes () =
  let solver = A.create () in
  register solver 1 [0, 1] 2;
  register solver 2 [0, 1] 4;
  register solver 3 [0, 1] 1;
  register solver 4 [0, -1] (-6);
  let reason literal =
    List.assoc literal (expect_consistent solver).propagations in
  A.assert_literal solver 1;
  check (reason 2 = [|1|]) "Initial propagation reason";
  check (reason 2 = [|1|]) "Repeated propagation reason";
  A.push solver;
  A.assert_literal solver 3;
  check (reason 2 = [|3|]) "Improved bound propagation reason";
  A.pop solver;
  check (reason 2 = [|1|]) "Popped bound survived in a reason";
  A.reset solver;
  check ((expect_consistent solver).propagations = [])
    "Reset retained bound implications";
  A.assert_literal solver 4;
  check (reason (-2) = [|4|]) "Opposite bound propagation reason";
  register solver 5 [0, 1] 5;
  check (reason (-5) = [|4|]) "Late atom omitted a propagation";
  let solver = A.create () in
  let reason literal =
    List.assoc literal (expect_consistent solver).propagations in
  register solver 1 [0, 1] (-2);
  register solver 6 [0, 1; 1, 1] 0;
  register solver 7 [1, 1] (-1);
  register solver 8 [1, 1] (-3);
  A.assert_literal solver 1;
  A.assert_literal solver 7;
  check (reason 6 = [|1; 7|]) "Derived interval propagation reason";
  A.push solver;
  A.assert_literal solver 8;
  check (reason 6 = [|1; 8|]) "Derived interval missed a stronger bound";
  A.pop solver;
  check (reason 6 = [|1; 7|]) "Derived interval retained a popped reason";
  for budget = 0 to 100 do
    A.push solver;
    A.assert_literal solver 8;
    ignore (A.check ~budget solver);
    A.pop solver;
    check (reason 6 = [|1; 7|]) "Interrupted propagation retained a reason"
  done

let interrupted_propagation () =
  let stopped = ref 0 in
  for budget = 0 to 1200 do
    let solver = A.create () in
    let old = ref [] and strong = ref [] in
    for i = 0 to 23 do
      let base = i * 6 in
      List.iter (fun (offset, sign, bound) ->
        register solver (base + offset) [i, sign] bound)
        [1, 1, -10; 2, -1, 15; 3, 1, 0;
         4, 1, -20; 5, 1, -12; 6, -1, 13];
      A.assert_literal solver (base + 1);
      A.assert_literal solver (base + 2);
      old := (base + 3, [|base + 1|])
             :: (-base - 4, [|base + 2|]) :: !old;
      strong := (base + 3, [|base + 5|])
                :: (-base - 4, [|base + 6|]) :: !strong
    done;
    let sort = List.sort (fun (a, _) (b, _) -> Int.compare a b) in
    let old = sort !old and strong = sort !strong in
    let expect_propagations expected =
      check ((expect_consistent solver).propagations = expected)
        "Interrupted propagation mismatch"
    in
    (match A.check ~budget solver with
    | A.Unknown -> incr stopped
    | A.Consistent result when result.propagations = old -> ()
    | _ -> failwith "Interrupted initial propagation");
    expect_propagations old;
    A.push solver;
    for i = 0 to 23 do
      A.assert_literal solver (i * 6 + 5);
      A.assert_literal solver (i * 6 + 6)
    done;
    ignore (A.check ~budget solver);
    if budget mod 2 = 0 then expect_propagations strong;
    A.pop solver;
    expect_propagations old;
    A.reset solver;
    expect_propagations []
  done;
  check (!stopped > 0) "No interrupted propagation checks"

let lazy_propagation_lifetimes () =
  let solver = A.create () in
  register solver 1 [0, 1] 2;
  register solver 2 [0, -1] (-3);
  register solver 3 [0, 1] 4;
  let reason literal =
    match A.check_lazy solver with
    | A.Consistent result -> List.assoc literal result.propagations
    | _ -> failwith "Expected lazy propagation" in
  A.push solver;
  A.assert_literal solver 1;
  let upper = reason 3 in
  check (not (Lazy.is_val upper)) "Eager propagation explanation";
  A.pop solver;
  A.assert_literal solver 2;
  let lower = reason (-1) in
  A.reset solver;
  check (Lazy.force upper = [|1|]) "Lost popped propagation reason";
  check (Lazy.force lower = [|2|]) "Lost reset propagation reason";
  A.assert_literal solver 1;
  check (Lazy.force (reason 3) = [|1|]) "Stale lazy propagation reason"

let integer_inequality_recovery () =
  let create () =
    let solver = A.create () in
    List.iter (fun (literal, terms, bound) ->
      register solver literal terms bound)
      [1, [0, -2; 1, -3], -1;
       2, [0, -1], 0; 3, [0, 1], 2;
       4, [1, -1], 0; 5, [1, 1], 2;
       6, [0, 1; 1, 1], 0];
    List.iter (A.assert_literal solver) [1; 2; 3; 4; 5];
    solver in
  let validate = function
  | A.Consistent {model; split = None; _} ->
    let x = List.assoc 0 model and y = List.assoc 1 model in
    check (Q.is_integer x && Q.is_integer y) "Integral model";
    check (Q.compare (Q.add (Q.mul (Q.of_int 2) x)
      (Q.mul (Q.of_int 3) y)) Q.one >= 0) "Inequality";
    check (Q.sign x >= 0 && Q.compare x (Q.of_int 2) <= 0
      && Q.sign y >= 0 && Q.compare y (Q.of_int 2) <= 0) "Bounds"
  | _ -> failwith "Expected inequality model" in
  let recover solver =
    validate (A.check solver);
    A.push solver;
    A.assert_literal solver 6;
    (match A.check solver with
    | A.Conflict _ -> () | _ -> failwith "Conflict");
    A.pop solver;
    validate (A.check solver) in
  (match A.check ~model_search:false (create ()) with
   | A.Consistent {split = Some _; _} -> ()
   | _ -> failwith "Fixture did not need integer proposal");
  let calls = ref 0 in
  validate (A.check ~charge:(fun () -> incr calls; true) (create ()));
  for stopped = 0 to !calls do
    let solver = create () in
    ignore (A.check ~budget:stopped solver);
    recover solver;
    let solver = create () and remaining = ref stopped in
    (try ignore (A.check ~charge:(fun () ->
      if !remaining = 0 then raise Exit;
      decr remaining; true) solver) with Exit -> ());
    recover solver
  done;
  Printf.printf "%d inequality budget and exception positions passed\n"
    (!calls + 1)

let equality_witness_recovery () =
  let check solver = A.check ~model_search:false solver in
  let make () =
    let solver = A.create () in
    register solver 1 [0, 2; 1, 1] 0;
    register solver 2 [0, -2; 1, -1] 0;
    register solver 3 [1, -1] (-1);
    register solver 4 [1, 1] 3;
    register solver 5 [1, 1] 1;
    List.iter (A.assert_literal solver) [1; 2; 3; 4];
    solver
  in
  let consistent solver =
    match check solver with
    | A.Consistent _ -> ()
    | _ -> failwith "expected a consistent relaxation"
  in
  let conflict solver =
    match check solver with
    | A.Conflict _ -> ()
    | _ -> failwith "expected an integer conflict"
  in
  let recover solver =
    consistent solver;
    A.push solver;
    A.assert_literal solver 5;
    conflict solver;
    A.pop solver;
    consistent solver;
    A.reset solver;
    List.iter (A.assert_literal solver) [1; 2; 3; 5];
    conflict solver;
    A.reset solver;
    List.iter (A.assert_literal solver) [1; 2; 3; 4];
    consistent solver;
    register solver 6 [2, 1] 1;
    register solver 7 [2, -1] (-1);
    register solver 8 [3, 2; 2, 1] 0;
    register solver 9 [3, -2; 2, -1] 0;
    List.iter (A.assert_literal solver) [6; 7; 8; 9];
    conflict solver
  in
  let initial = make () in
  (match check initial with
  | A.Consistent { split = Some _; _ } -> ()
  | _ -> failwith "expected a fractional initial model");
  recover initial;
  let calls = ref 0 in
  ignore (A.check ~model_search:false ~charge:(fun () -> incr calls; true)
    (make ()));
  for stop = 1 to !calls do
    let solver = make () and count = ref 0 in
    ignore (A.check ~model_search:false ~charge:(fun () ->
      incr count; !count <> stop) solver);
    recover solver;
    let solver = make () and count = ref 0 in
    (try ignore (A.check ~model_search:false ~charge:(fun () ->
      incr count; if !count = stop then raise Exit; true) solver)
     with Exit -> ());
    recover solver
  done

let missing_interval_bound_recovery () =
  let result solver =
    match A.check ~phase:A.Propagation solver with
    | A.Consistent result -> result
    | _ -> failwith "expected consistent arithmetic propagation"
  in
  let prepare () =
    let solver = A.create () in
    register solver 1 [0, 1; 1, 2] (-2);
    register solver 2 [0, 1; 1, 1] (-1);
    register solver 3 [1, 1] (-1);
    ignore (result solver);
    A.push solver;
    A.assert_literal solver 2;
    A.assert_literal solver 3;
    solver
  in
  let validate solver =
    check
      (List.exists (fun (literal, reason) ->
         literal = 1 && reason = [|2; 3|])
         (result solver).propagations)
      "missing implication after basis change";
    A.pop solver;
    check ((result solver).propagations = [])
      "propagation retained after pop";
    A.assert_literal solver 2;
    ignore (result solver);
    A.assert_literal solver 3;
    check
      (List.exists (fun (literal, _) -> literal = 1)
         (result solver).propagations)
      "missing implication after source bound restored";
    A.reset solver;
    check ((result solver).propagations = [])
      "propagation retained after reset"
  in
  let polls = ref 0 in
  ignore (A.check ~phase:A.Propagation
    ~charge:(fun () -> incr polls; true) (prepare ()));
  validate (prepare ());
  let recover solver =
    ignore (result solver);
    A.push solver;
    A.assert_literal solver (-1);
    ignore (expect_conflict solver);
    A.pop solver;
    ignore (result solver);
    A.pop solver;
    check ((result solver).propagations = [])
      "propagation retained after recovered pop";
    A.reset solver;
    ignore (result solver)
  in
  for budget = 0 to !polls - 1 do
    let solver = prepare () in
    check (A.check ~phase:A.Propagation ~budget solver = A.Unknown)
      "expected interrupted propagation";
    recover solver;
    let solver = prepare () and remaining = ref budget in
    (match A.check ~phase:A.Propagation ~charge:(fun () ->
       if !remaining = 0 then raise Exit;
       decr remaining;
       true) solver with
    | exception Exit -> ()
    | _ -> failwith "expected callback interruption");
    recover solver
  done

let integer_width_recovery () =
  let prepare () =
    let solver = A.create () in
    register solver 1 [0, 2830000; 1, -2450001] 9999;
    register solver 2 [0, -2830000; 1, 2450001] 0;
    register solver 3 [0, 2830001; 1, -2450000] 10000;
    register solver 4 [0, -2830001; 1, 2450000] (-1);
    List.iter (A.assert_literal solver) [1; 2; 3];
    ignore (expect_consistent solver);
    A.push solver;
    A.assert_literal solver 4;
    solver
  in
  let validate solver =
    (match A.check ~model_search:false solver with
    | A.Conflict reasons ->
      check (reasons = [|1; 2; 3; 4|]) "integer width explanation"
    | _ -> failwith "missing integer width conflict");
    A.pop solver;
    ignore (expect_consistent solver);
    A.reset solver;
    ignore (expect_consistent solver)
  in
  let used = ref 0 in
  (match A.check ~model_search:false
      ~charge:(fun () -> incr used; true) (prepare ()) with
  | A.Conflict _ -> ()
  | _ -> failwith "missing charged integer width conflict");
  validate (prepare ());
  for budget = 0 to !used - 1 do
    let solver = prepare () in
    check (A.check ~model_search:false ~budget solver = A.Unknown)
      "integer width budget interruption";
    validate solver;
    let solver = prepare () and remaining = ref budget in
    (match A.check ~model_search:false ~charge:(fun () ->
       if !remaining = 0 then raise Exit;
       decr remaining;
       true) solver with
    | exception Exit -> ()
    | _ -> failwith "integer width callback interruption");
    validate solver
  done

let optimized_cut_recovery cut_basis split_forms =
  let rows = [|
    [0, 4; 1, 1; 2, 1], 2;
    [0, -4; 1, -1; 2, -1], -1;
    [0, 1; 1, 4; 2, 1], 2;
    [0, -1; 1, -4; 2, -1], -1;
    [0, 1; 1, 1; 2, 4], 2;
    [0, -1; 1, -1; 2, -4], -1
  |] in
  let prepare () =
    let solver = A.create () in
    Array.iteri (fun index (terms, boundary) ->
      A.register solver ~literal:(index + 1)
        (List.map (fun (id, coefficient) -> id, Z.of_int coefficient) terms)
        (Z.of_int boundary)) rows;
    A.push solver;
    Array.iteri (fun index _ -> A.assert_literal solver (index + 1)) rows;
    solver
  in
  let result ?budget ?(charge = fun () -> true) solver =
    A.check ~cut_basis ~split_forms ~optimize:true ~cuts:(not split_forms)
      ~model_search:false ?budget ~charge solver
  in
  let validate solver =
    (match result solver with
    | A.Consistent {cut = Some (terms, boundary, reasons); _} ->
      check (Array.length reasons > 0) "missing cut guards";
      Array.iter (fun literal ->
        check (literal >= 1 && literal <= Array.length rows)
          "invalid cut guard") reasons;
      let actual = List.fold_left (fun sum (id, coefficient) ->
        let value = Option.get (A.value solver id) in
        Q.add sum (Q.mul (Q.of_z coefficient) value)) Q.zero terms in
      check (Q.compare actual (Q.of_z boundary) > 0)
        "cut does not exclude the fractional model"
    | A.Consistent {split_form = Some (terms, boundary); _}
        when split_forms ->
      let actual = List.fold_left (fun sum (id, coefficient) ->
        Q.add sum (Q.mul (Q.of_z coefficient)
          (Option.get (A.value solver id)))) Q.zero terms in
      check (terms <> []) "empty integer split";
      check (Q.compare actual (Q.of_z boundary) > 0
        && Q.compare actual (Q.of_z (Z.add boundary Z.one)) < 0)
        "integer split does not straddle the fractional model"
    | A.Conflict _ -> ()
    | _ -> failwith "missing optimized cut or split");
    A.pop solver;
    (match result solver with
    | A.Consistent _ -> () | _ -> failwith "cut survived pop");
    A.reset solver;
    (match result solver with
    | A.Consistent _ -> () | _ -> failwith "cut survived reset")
  in
  let calls = ref 0 in
  ignore (result ~charge:(fun () -> incr calls; true) (prepare ()));
  validate (prepare ());
  for budget = 0 to !calls - 1 do
    let solver = prepare () in
    check (result ~budget solver = A.Unknown) "missing budget interruption";
    validate solver;
    let solver = prepare () and count = ref 0 in
    (match result ~charge:(fun () ->
       if !count = budget then raise Exit;
       incr count; true) solver with
    | exception Exit -> () | _ -> failwith "missing callback interruption");
    validate solver
  done;
  Printf.printf "Optimized cuts: %d interruption positions recovered\n%!"
    !calls

let optimized_cut_validity cut_basis =
  let random = Random.State.make [|373129|] in
  let total values terms = List.fold_left (fun sum (id, coefficient) ->
    Z.add sum (Z.mul coefficient (Z.of_int values.(id)))) Z.zero terms in

  let cut_count = ref 0 and conflict_count = ref 0 in
  for _ = 1 to 300 do
    let solver = A.create () in
    let rows = ref [] in
    for variable = 0 to 2 do
      rows := ([variable, Z.one], Z.of_int 2) ::
        ([variable, Z.minus_one], Z.of_int 2) :: !rows
    done;
    for _ = 1 to 8 do
      let terms = List.init 3 (fun id ->
        id, Z.of_int (Random.State.int random 7 - 3)) in
      rows := (terms, Z.of_int (Random.State.int random 9 - 4)) :: !rows
    done;
    let rows = Array.of_list !rows in
    Array.iteri (fun index (terms, boundary) ->
      A.register solver ~literal:(index + 1) terms boundary;
      A.assert_literal solver (index + 1)) rows;
    let holds values (terms, boundary) =
      Z.compare (total values terms) boundary <= 0 in
    let answer =
      A.check ~cut_basis ~optimize:true ~cuts:true
        ~model_search:false solver in
    let validate values =
      if Array.for_all (holds values) rows then
        match answer with
        | A.Conflict _ -> failwith "false optimized arithmetic conflict"
        | A.Consistent {cut = Some (terms, boundary, _); _} ->
          check (holds values (terms, boundary)) "invalid GMI cut"
        | A.Consistent _ | A.Unknown -> () in
    for x = -2 to 2 do for y = -2 to 2 do for z = -2 to 2 do
      validate [|x; y; z|]
    done done done;
    (match answer with
    | A.Consistent {cut = Some (terms, boundary, reasons); _} ->
      incr cut_count;
      Array.iter (fun literal ->
        check (literal >= 1 && literal <= Array.length rows)
          "invalid GMI guard") reasons;
      let actual = List.fold_left (fun sum (id, coefficient) ->
        Q.add sum (Q.mul (Q.of_z coefficient)
          (Option.get (A.value solver id)))) Q.zero terms in
      check (Q.compare actual (Q.of_z boundary) > 0)
        "GMI cut does not exclude model"
    | A.Conflict _ -> incr conflict_count
    | _ -> ());
    A.reset solver;
    (match A.check ~optimize:true ~cuts:true solver with
    | A.Consistent _ -> () | _ -> failwith "optimized reset")
  done;
  check (!cut_count > 0) "random systems generated no cuts";
  Printf.printf "Optimized cuts: 300 systems, %d cuts, %d conflicts\n%!"
    !cut_count !conflict_count

let cone_model_recovery () =
  let create () =
    let solver = A.create () in
    List.iter (fun (literal, terms, bound) ->
      register solver literal terms bound)
      [1, [0, -2; 1, 3], -1;
       2, [0, -1], 0; 3, [1, -1], 0; 4, [0, 1], 0];
    List.iter (A.assert_literal solver) [1; 2; 3];
    solver in
  let validate solver =
    match A.check solver with
    | A.Consistent {model; split = None; _} ->
      let x = List.assoc 0 model and y = List.assoc 1 model in
      check (Q.is_integer x && Q.is_integer y) "fractional cone model";
      check (Q.sign x >= 0 && Q.sign y >= 0) "negative cone model";
      check (Q.compare (Q.add (Q.mul (Q.of_int (-2)) x)
        (Q.mul (Q.of_int 3) y)) (Q.of_int (-1)) <= 0)
        "cone model violates inequality"
    | _ -> failwith "missing cone model" in
  let recover solver =
    validate solver;
    A.push solver;
    A.assert_literal solver 4;
    ignore (expect_conflict solver);
    A.pop solver;
    validate solver;
    A.reset solver;
    ignore (expect_consistent solver) in
  (match A.check ~model_search:false (create ()) with
  | A.Consistent {split = Some _; _} -> ()
  | _ -> failwith "cone fixture did not need an integer model");
  let calls = ref 0 in
  ignore (A.check ~charge:(fun () -> incr calls; true) (create ()));
  for stopped = 0 to !calls do
    let solver = create () in
    ignore (A.check ~budget:stopped solver);
    recover solver;
    let solver = create () and remaining = ref stopped in
    (try ignore (A.check ~charge:(fun () ->
      if !remaining = 0 then raise Exit;
      decr remaining; true) solver) with Exit -> ());
    recover solver
  done

let () =
  List.iter (fun basis ->
    optimized_cut_recovery basis false;
    optimized_cut_validity basis) [A.Tableau; A.Reduced; A.Combined];
  List.iter (fun basis -> optimized_cut_recovery basis true)
    [A.Reduced; A.Combined];
  cone_model_recovery ();
  integer_width_recovery ();
  integer_inequality_recovery ();
  lazy_propagation_lifetimes ();
  propagation_cache_lifetimes ();
  interrupted_propagation ();
  basics ();
  rounding_and_splits ();
  divisibility ();
  divisibility_dependencies ();
  equality_witness_recovery ();
  missing_interval_bound_recovery ();
  interrupted_pivot ();
  randomized ();
  print_endline "Incremental arithmetic tests passed"
