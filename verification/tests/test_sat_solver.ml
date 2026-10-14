module Sat = Vox_sat_solver

let check condition message = if not condition then failwith message

let literal_value assignment literal = assignment.(abs literal) = (literal > 0)

let satisfies assignment clauses assumptions =
  Array.for_all (literal_value assignment) assumptions
  && List.for_all
       (fun clause -> Array.exists (literal_value assignment) clause)
       clauses

let brute_force variables clauses assumptions =
  let assignment = Array.make (variables + 1) false in
  let rec search bits =
    if bits = 1 lsl variables
    then None
    else (
      for variable = 1 to variables do
        assignment.(variable) <- bits land (1 lsl (variable - 1)) <> 0
      done;
      if satisfies assignment clauses assumptions
      then Some (Array.copy assignment)
      else search (bits + 1))
  in
  search 0

let describe clauses assumptions =
  let clause clause =
    Array.to_list clause |> List.map string_of_int |> String.concat " "
  in
  Printf.sprintf "clauses=[%s], assumptions=[%s]"
    (String.concat "; " (List.map clause clauses))
    (clause assumptions)

let random_literal random variables =
  let variable = 1 + Random.State.int random variables in
  if Random.State.bool random then variable else -variable

let random_clause random variables =
  Array.init
    (1 + Random.State.int random 5)
    (fun _ -> random_literal random variables)

let random_assumptions random variables =
  Array.init
    (Random.State.int random (variables + 2))
    (fun _ -> random_literal random variables)

let verify solver variables clauses assumptions =
  let expected = brute_force variables clauses assumptions in
  let context () = describe clauses assumptions in
  match Sat.solve ~assumptions solver, expected with
  | Sat.Sat model, Some _ ->
    check (Array.length model > variables) "Short model";
    check (satisfies model clauses assumptions) ("Invalid model: " ^ context ())
  | Sat.Unsat core, None ->
    check
      (Array.for_all (fun literal -> Array.mem literal assumptions) core)
      ("Core contains a non-assumption: " ^ context ());
    check
      (brute_force variables clauses core = None)
      ("Satisfiable unsat core: " ^ context ())
  | Sat.Sat _, None -> failwith ("Expected UNSAT: " ^ context ())
  | Sat.Unsat _, Some _ -> failwith ("Expected SAT: " ^ context ())
  | Sat.Unknown, _ -> failwith ("Unexpected UNKNOWN: " ^ context ())

let create_solver ?(completion = false) variables clauses =
  let solver = Sat.create () in
  for _ = 1 to variables do
    ignore (Sat.new_var ~completion solver)
  done;
  List.iter (Sat.add_clause solver) clauses;
  solver

let expect_invalid function_ =
  match function_ () with
  | _ -> failwith "Expected Invalid_argument"
  | exception Invalid_argument _ -> ()

let exhaustive_two_variables ?completion () =
  let clauses =
    Array.init 9 (fun code ->
        List.filter (( <> ) 0)
          [ (match code mod 3 with 0 -> 0 | 1 -> 1 | _ -> -1);
            (match code / 3 with 0 -> 0 | 1 -> 2 | _ -> -2) ]
        |> Array.of_list)
  in
  for selected = 0 to (1 lsl Array.length clauses) - 1 do
    let formula = ref [] in
    Array.iteri
      (fun index clause ->
        if selected land (1 lsl index) <> 0 then formula := clause :: !formula)
      clauses;
    let solver = create_solver ?completion 2 !formula in
    Array.iter (verify solver 2 !formula) clauses;
    verify solver 2 !formula [| 1; -1 |];
    verify solver 2 !formula [| 2; 2; -2 |]
  done

let randomized_formulas ?completion () =
  let random = Random.State.make [| 0x56f3; 0x918 |] in
  for _ = 1 to 2500 do
    let variables = 1 + Random.State.int random 8 in
    let clauses =
      List.init
        (Random.State.int random ((6 * variables) + 1))
        (fun _ -> random_clause random variables)
    in
    let solver = create_solver ?completion variables clauses in
    for _ = 1 to 6 do
      verify solver variables clauses (random_assumptions random variables)
    done;
    verify solver variables clauses [||]
  done

let randomized_scopes ?completion () =
  let random = Random.State.make [| 0x51c0; 0xacc |] in
  for _ = 1 to 200 do
    let variables = 6 in
    let solver = create_solver ?completion variables [] in
    let permanent = ref [] in
    let scopes = ref [[]] in
    let clauses () = !permanent @ List.concat !scopes in
    for _ = 1 to 80 do
      (match Random.State.int random 7, !scopes with
      | 0, _ when List.length !scopes < 5 ->
        Sat.push solver;
        scopes := [] :: !scopes
      | 1, _ :: (_ :: _ as outer) ->
        Sat.pop solver;
        scopes := outer
      | 2, _ ->
        let clause = random_clause random variables in
        Sat.add_clause_global solver clause;
        permanent := clause :: !permanent
      | _, current :: outer ->
        let clause = random_clause random variables in
        Sat.add_clause solver clause;
        scopes := (clause :: current) :: outer
      | _, [] -> failwith "Missing base scope");
      let assumptions = random_assumptions random variables in
      verify solver variables (clauses ()) assumptions;
      verify solver variables (clauses ()) [||]
    done;
    while List.length !scopes > 1 do
      Sat.pop solver;
      scopes := List.tl !scopes;
      verify solver variables (clauses ()) [||]
    done
  done

let incremental_regressions () =
  let clauses =
    [[| -1; 2; 3 |]; [| -1; -2; 3 |]; [| -1; 2; -3 |]; [| -1; -2; -3 |]]
  in
  let solver = create_solver 3 clauses in
  verify solver 3 clauses (Array.make 100 1);
  verify solver 3 clauses [| -1 |];
  let solver = create_solver 3 [[| 1; 2 |]; [| -1; 3 |]] in
  let clauses = [[| 1; 2 |]; [| -1; 3 |]] in
  verify solver 3 clauses [| -2 |];
  verify solver 3 clauses [| -3 |];
  verify solver 3 clauses [| -2; -3 |];
  verify solver 3 clauses [| -2; -3; 1; 1 |];
  verify solver 3 clauses [||];
  Sat.push solver;
  Sat.add_clause solver [||];
  verify solver 3 ([||] :: clauses) [||];
  Sat.push solver;
  verify solver 3 ([||] :: clauses) [||];
  Sat.pop solver;
  verify solver 3 ([||] :: clauses) [||];
  Sat.pop solver;
  verify solver 3 clauses [||];
  Sat.push solver;
  Sat.add_clause solver [| -1 |];
  Sat.add_clause_global solver [| -2 |];
  verify solver 3 ([| -1 |] :: [| -2 |] :: clauses) [||];
  Sat.pop solver;
  verify solver 3 ([| -2 |] :: clauses) [||];
  Sat.add_clause solver [| -3 |];
  verify solver 3 ([| -3 |] :: [| -2 |] :: clauses) [||];
  Sat.push solver;
  Sat.pop solver;
  verify solver 3 ([| -3 |] :: [| -2 |] :: clauses) [| 1 |]

let inputs_and_growth () =
  expect_invalid (fun () -> Sat.create ~variables:(-1) ());
  let empty = create_solver 0 [] in
  verify empty 0 [] [||];
  check
    (Sat.solve ~max_conflicts:0 empty = Sat.Unknown)
    "Zero conflict budget ignored";
  Sat.push empty;
  Sat.add_clause empty [||];
  verify empty 0 [[||]] [||];
  Sat.pop empty;
  verify empty 0 [] [||];
  let solver = create_solver 3 [] in
  check (Sat.variable_count solver = 3) "Initial variable count";
  List.iter
    (fun clause ->
      expect_invalid (fun () -> Sat.add_clause solver clause);
      expect_invalid (fun () -> Sat.add_clause_global solver clause);
      expect_invalid (fun () -> Sat.solve ~assumptions:clause solver);
      verify solver 3 [] [| -1; -2; -3 |])
    [[| 0 |]; [| 4 |]; [| -4 |]; [| min_int |]; [| max_int |]; [| 1; 0 |]];
  expect_invalid (fun () -> Sat.pop solver);
  let clause = [| 1; 1; 2; 2 |] in
  Sat.add_clause solver clause;
  Array.fill clause 0 (Array.length clause) (-1);
  Sat.add_clause solver [| 3; -3; 3 |];
  verify solver 3 [[| 1; 2 |]] [| -1 |];
  let first_model =
    match Sat.solve ~assumptions:[| -1 |] solver with
    | Sat.Sat model -> model
    | _ -> failwith "Expected a model"
  in
  let saved_model = Array.copy first_model in
  verify solver 3 [[| 1; 2 |]] [| -2 |];
  check (first_model = saved_model) "Solve mutated an earlier model";
  let variable = Sat.new_var solver in
  check (variable = 4) "New variable identity";
  Sat.add_clause solver [| -4 |];
  verify solver 4 [[| 1; 2 |]; [| -4 |]] [||];
  Sat.push solver;
  let scoped_variable = Sat.new_var solver in
  check (scoped_variable > variable) "Variable identity reused";
  Sat.add_clause solver [| scoped_variable |];
  (match Sat.solve ~assumptions:[| -scoped_variable |] solver with
  | Sat.Unsat _ -> ()
  | _ -> failwith "Scoped variable constraint lost");
  Sat.pop solver;
  (match Sat.solve ~assumptions:[| -scoped_variable |] solver with
  | Sat.Sat model ->
    check (not model.(scoped_variable)) "Scoped variable assumption lost"
  | _ -> failwith "Popped variable constraint retained");
  check
    (Sat.variable_count solver >= scoped_variable)
    "Pop reclaimed variable identity"

let long_propagation () =
  let variables = 20000 in
  let solver = create_solver variables [] in
  for variable = 1 to variables - 1 do
    Sat.add_clause solver [| -variable; variable + 1 |]
  done;
  (match Sat.solve ~assumptions:[| 1 |] solver with
  | Sat.Sat model ->
    for variable = 1 to variables do
      check model.(variable) "Broken implication chain"
    done
  | _ -> failwith "Satisfiable implication chain rejected");
  (match Sat.solve ~assumptions:[| 1; -variables |] solver with
  | Sat.Unsat core ->
    check
      (Array.mem 1 core && Array.mem (-variables) core)
      "Missing implication chain core assumption"
  | _ -> failwith "Contradictory implication chain accepted");
  (match Sat.solve ~assumptions:[| -variables |] solver with
  | Sat.Sat model ->
    for variable = 1 to variables do
      check (not model.(variable)) "Broken reverse implication chain"
    done
  | _ -> failwith "Implication chain remained contradictory");
  let variables = 4000 in
  let solver = create_solver variables [] in
  Sat.add_clause solver (Array.init variables (fun i -> i + 1));
  let assumptions = Array.init (variables - 1) (fun i -> -(i + 1)) in
  (match Sat.solve ~assumptions solver with
  | Sat.Sat model -> check model.(variables) "Long clause lost its last watch"
  | _ -> failwith "Long clause rejected");
  match
    Sat.solve ~assumptions:(Array.append assumptions [| -variables |]) solver
  with
  | Sat.Unsat core ->
    check (Array.length core = variables) "Incomplete long clause core"
  | _ -> failwith "Falsified long clause accepted"

let pigeonhole pigeons holes =
  let variable pigeon hole = 1 + (pigeon * holes) + hole in
  let clauses = ref [] in
  for pigeon = 0 to pigeons - 1 do
    clauses := Array.init holes (variable pigeon) :: !clauses
  done;
  for hole = 0 to holes - 1 do
    for first = 0 to pigeons - 1 do
      for second = first + 1 to pigeons - 1 do
        clauses := [| -variable first hole; -variable second hole |] :: !clauses
      done
    done
  done;
  pigeons * holes, !clauses

let expect_unsat solver =
  match Sat.solve solver with
  | Sat.Unsat core -> check (core = [||]) "Unexpected assumption in core"
  | _ -> failwith "Expected UNSAT"

let limits_and_restarts () =
  let variables, clauses = pigeonhole 7 6 in
  let solver = create_solver variables clauses in
  check
    (Sat.solve ~should_stop:(fun () -> true) solver = Sat.Unknown)
    "Cancellation ignored";
  check
    (Sat.solve ~max_conflicts:1 solver = Sat.Unknown)
    "Conflict limit ignored";
  let before = Sat.statistics solver in
  check (before.conflicts >= 1) "Conflict limit did not enter search";
  let polls = ref 0 in
  (match
     Sat.solve
       ~should_stop:(fun () ->
         incr polls;
         if !polls = 2 then raise Exit;
         false)
       solver
   with
  | _ -> failwith "Cancellation callback exception was lost"
  | exception Exit -> ());
  expect_unsat solver;
  let statistics = Sat.statistics solver in
  check (statistics.conflicts > before.conflicts) "Conflicts not counted";
  check (statistics.propagations > 0) "Propagations not counted";
  check (statistics.decisions > 0) "Decisions not counted";
  check (statistics.learnt_clauses > 0) "No clauses learned";
  Sat.reset_statistics solver;
  let statistics = Sat.statistics solver in
  check
    (statistics.conflicts = 0
    && statistics.propagations = 0
    && statistics.decisions = 0
    && statistics.learnt_clauses = 0
    && statistics.restarts = 0
    && statistics.deleted_clauses = 0)
    "Statistics reset failed";
  expect_unsat solver;
  expect_invalid (fun () -> Sat.solve ~max_conflicts:(-1) solver)

let clause_reduction () =
  let variables, clauses = pigeonhole 8 7 in
  let solver = create_solver variables clauses in
  expect_unsat solver;
  let statistics = Sat.statistics solver in
  check (statistics.conflicts > 2000) "Clause reduction path not exercised";
  check (statistics.deleted_clauses > 0) "No learned clauses deleted";
  check (statistics.restarts > 0) "Restart path not exercised";
  expect_unsat solver

let empty_extension =
  { Sat.start = (fun () -> ());
    push_level = (fun () -> ());
    pop_levels = (fun _ -> ());
    assigned = (fun _ _ -> ());
    propagate = (fun _ -> ());
    final_check = (fun _ -> Sat.Done);
    finish = (fun () -> ())
  }

let randomized_extensions () =
  let random = Random.State.make [| 0x7e07; 0x11a |] in
  for iteration = 1 to 1500 do
    let variables = 1 + Random.State.int random 7 in
    let theory =
      List.init (Random.State.int random 20) (fun _ ->
          random_clause random variables)
    in
    let clauses =
      List.init (Random.State.int random 12) (fun _ ->
          random_clause random variables)
    in
    let remaining = ref theory in
    let depth = ref 0 in
    let inspect context =
      check (Sat.current_level context = !depth) "Theory level mismatch";
      List.iter
        (fun clause ->
          let satisfied = ref false in
          let undecided = ref [] in
          let antecedents = ref [] in
          Array.iter
            (fun lit ->
              match Sat.assigned_value context lit with
              | Some true -> satisfied := true
              | Some false -> antecedents := -lit :: !antecedents
              | None ->
                if not (List.mem lit !undecided)
                then undecided := lit :: !undecided)
            clause;
          if not !satisfied
          then
            let reason = Array.of_list !antecedents in
            match !undecided with
            | [] -> Sat.conflict context ~reason:(fun () -> reason)
            | [lit] -> Sat.imply context lit ~reason:(fun () -> reason)
            | _ -> ())
        theory
    in
    let lemma context =
      match !remaining with
      | clause :: rest ->
        remaining := rest;
        Sat.add_lemma context clause
      | [] -> ()
    in
    let mode = iteration mod 3 in
    let extension =
      { Sat.start = (fun () -> depth := 0);
        push_level = (fun () -> incr depth);
        pop_levels = (fun count -> depth := !depth - count);
        assigned = (fun context _ -> if mode = 1 then lemma context);
        propagate = (fun context -> if mode = 0 then inspect context);
        final_check =
          (fun context ->
            if mode = 0
            then Sat.Done
            else if mode = 1
            then (
              if !remaining <> [] then lemma context;
              Sat.Done)
            else (
              inspect context;
              Sat.Done));
        finish = (fun () -> check (!depth = 0) "Theory levels leaked")
      }
    in
    let solver = Sat.create ~variables ~extension () in
    List.iter (Sat.add_clause solver) clauses;
    for _ = 1 to 6 do
      verify solver variables (clauses @ theory)
        (random_assumptions random variables)
    done;
    verify solver variables (clauses @ theory) [||]
  done

let extension_lifecycle () =
  let saved_context = ref None in
  let explanations = ref 0 in
  let finishes = ref 0 in
  let extension =
    { empty_extension with
      assigned =
        (fun context lit ->
          saved_context := Some context;
          if lit = 1 || lit = 2
          then
            Sat.imply context (lit + 1) ~reason:(fun () ->
                incr explanations;
                expect_invalid (fun () -> Sat.current_level context);
                [| lit |]));
      finish = (fun () -> incr finishes)
    }
  in
  let solver = Sat.create ~variables:3 ~extension () in
  verify solver 3 [[| -1; 2 |]; [| -2; 3 |]] [| 1; -3 |];
  check (!explanations = 2) "Theory reasons missing from assumption core";
  verify solver 3 [[| -1; 2 |]; [| -2; 3 |]] [| 1 |];
  check (!explanations = 2) "Theory reasons evaluated eagerly";
  (match !saved_context with
  | Some context ->
    expect_invalid (fun () -> Sat.current_level context);
    expect_invalid (fun () -> Sat.new_theory_var context)
  | None -> failwith "No theory assignment callback");
  Sat.add_clause solver [| 1 |];
  verify solver 3 [[| 1 |]; [| 2 |]; [| 3 |]] [||];
  check (!explanations = 2) "Cached theory reasons re-evaluated";
  let solver = Sat.create ~variables:3 ~extension () in
  Sat.add_clause solver [| 1 |];
  verify solver 3 [[| 1 |]; [| 2 |]; [| 3 |]] [||];
  check (!explanations = 4) "Root theory reasons not checked eagerly";
  check (!finishes = 4) "Theory finish count";
  let failed = ref true in
  let extension =
    { empty_extension with
      start = (fun () -> if !failed then raise Exit);
      finish = (fun () -> incr finishes)
    }
  in
  let solver = Sat.create ~variables:1 ~extension () in
  (match Sat.solve solver with
  | _ -> failwith "Theory start exception lost"
  | exception Exit -> ());
  check (!finishes = 5) "Theory finish skipped after start exception";
  failed := false;
  verify solver 1 [] [||];
  let added = ref false in
  let extension =
    { empty_extension with
      final_check =
        (fun context ->
          if not !added
          then (
            added := true;
            let variable = Sat.new_theory_var context in
            Sat.add_lemma context [| variable |]);
          Sat.Done)
    }
  in
  let solver = Sat.create ~extension () in
  (match Sat.solve solver with
  | Sat.Sat model ->
    check (Array.length model = 2 && model.(1)) "Final-check work ignored"
  | _ -> failwith "Fresh theory variable rejected");
  let extension =
    { empty_extension with final_check = (fun _ -> Sat.Incomplete) }
  in
  let solver = Sat.create ~extension () in
  check (Sat.solve solver = Sat.Unknown) "Incomplete theory accepted";
  List.iter
    (fun throw ->
      let added = ref false in
      let extension =
        { empty_extension with
          final_check =
            (fun context ->
              if !added
              then Sat.Done
              else (
                added := true;
                Sat.add_lemma context [| 1 |];
                if throw then raise Exit else Sat.Incomplete))
        }
      in
      let solver = Sat.create ~variables:1 ~extension () in
      (match Sat.solve solver with
      | Sat.Unknown when not throw -> ()
      | exception Exit when throw -> ()
      | _ -> failwith "Theory interruption result");
      verify solver 1 [[| 1 |]] [| -1 |];
      verify solver 1 [[| 1 |]] [||])
    [false; true]

let cached_explanation_interruption () =
  List.iter (fun throw ->
    let explanations = ref 0 in
    let stopped = ref false in
    let extension =
      { empty_extension with
        propagate = (fun context ->
          if Sat.assigned_value context 1 = Some true
             && Sat.assigned_value context 2 = Some true then
            List.iter (fun literal ->
              Sat.imply context literal ~reason:(fun () ->
                incr explanations;
                stopped := true;
                [|1; 2|])) [3; 4])
      }
    in
    let solver = Sat.create ~variables:4 ~extension () in
    Sat.push solver;
    Sat.add_clause solver [|-3; -4|];
    let should_stop () =
      if !stopped && throw then raise Exit else !stopped
    in
    (match Sat.solve ~assumptions:[|1; 2|] ~should_stop solver with
    | Sat.Unknown when not throw -> ()
    | exception Exit when throw -> ()
    | _ -> failwith "Cached explanation interruption result");
    check (!explanations = 2) "Lazy explanations were not expanded";
    Sat.pop solver;
    stopped := false;
    let theory = [[|-1; -2; 3|]; [|-1; -2; 4|]] in
    verify solver 4 theory [|-1; 2; -3; -4|];
    verify solver 4 theory [|1; 2|];
    verify solver 4 theory [|1; 2; -3|];
    verify solver 4 theory [|1; 2; -4|];
    check (!explanations = 2) "Cached explanations did not survive pop")
    [false; true]

let interrupted_theory_conflict () =
  List.iter (fun scoped ->
    List.iter (fun throw ->
      let emitted = ref false and polls = ref 0 in
      let extension =
        { empty_extension with
          propagate = (fun context ->
            if not !emitted && Sat.assigned_value context 1 = Some true
            then begin
              emitted := true;
              Sat.add_lemma context [|-1|]
            end) }
      in
      let solver = Sat.create ~variables:1 ~extension () in
      if scoped then Sat.push solver;
      Sat.add_clause solver [|1|];
      let should_stop () =
        if not !emitted then false else begin
          incr polls;
          if !polls < 2 then false
          else if throw then raise Exit else true
        end
      in
      (match Sat.solve ~should_stop solver with
      | Sat.Unknown when not throw -> ()
      | exception Exit when throw -> ()
      | _ -> failwith "Theory conflict interruption result");
      (match Sat.solve solver with
      | Sat.Unsat _ -> ()
      | Sat.Sat _ | Sat.Unknown -> failwith "Interrupted conflict was lost");
      if scoped then begin
        Sat.pop solver;
        verify solver 1 [[|-1|]] [||]
      end)
      [false; true])
    [false; true]

let cancelled_root_conflict () =
  let emitted = ref false and polls = ref 0 in
  let extension = { empty_extension with
    propagate = (fun context -> if not !emitted then begin
      emitted := true; Sat.add_lemma context [|-1|]
    end) } in
  let solver = Sat.create ~variables:1 ~extension () in
  Sat.add_clause solver [|1|];
  let should_stop () =
    if not !emitted then false else (incr polls; !polls = 2) in
  check (Sat.solve ~should_stop solver = Sat.Unknown)
    "Root conflict cancellation";
  let clauses = Sat.clause_count solver in
  for _ = 1 to 8 do
    let polls = ref 0 in
    (match Sat.solve
      ~should_stop:(fun () -> incr polls; !polls = 3) solver with
    | Sat.Unsat _ -> ()
    | Sat.Unknown | Sat.Sat _ -> failwith "Root conflict retry made no progress");
    check (Sat.clause_count solver = clauses)
      "Root conflict retry duplicated a clause"
  done

let deferred_root_work () =
  let count = 50000 in
  List.iter (fun throw ->
    let solver = Sat.create ~variables:count () in
    for variable = 1 to count - 1 do
      Sat.add_clause solver [|-variable; variable + 1|]
    done;
    Sat.add_clause solver [|1|];
    check ((Sat.statistics solver).propagations = 0)
      "Unit insertion performed unbounded propagation";
    let stopped () = if throw then raise Exit else true in
    (match Sat.solve ~should_stop:stopped solver with
    | Sat.Unknown when not throw -> ()
    | exception Exit when throw -> ()
    | _ -> failwith "Root cancellation was ignored");
    check ((Sat.statistics solver).propagations = 0)
      "Root cancellation propagated pending units";
    let stopped () =
      if (Sat.statistics solver).propagations < 1024 then false
      else if throw then raise Exit else true in
    (match Sat.solve ~should_stop:stopped solver with
    | Sat.Unknown when not throw -> ()
    | exception Exit when throw -> ()
    | _ -> failwith "Long root propagation was not interrupted");
    (match Sat.solve solver with
    | Sat.Sat values ->
      for variable = 1 to count do
        check values.(variable) "Root propagation recovery lost a unit"
      done
    | Sat.Unsat _ | Sat.Unknown -> failwith "Root propagation recovery");
    Sat.add_clause solver [|-count|];
    match Sat.solve solver with
    | Sat.Unsat _ -> ()
    | Sat.Sat _ | Sat.Unknown -> failwith "Recovered root conflict lost")
    [false; true]

let completion_variables () =
  let model solver = match Sat.solve solver with
    | Sat.Sat values -> values
    | Sat.Unsat _ | Sat.Unknown -> failwith "Expected SAT" in
  let solver = Sat.create () in
  let variables = Array.init 10000 (fun _ ->
    Sat.new_var ~completion:true solver) in
  let values = model solver in
  check (Array.for_all (fun v -> not values.(v)) variables)
    "Unused variables did not complete to false";
  check ((Sat.statistics solver).decisions = 0)
    "Unused variables entered search";
  let x = variables.(0) and y = variables.(1) in
  (match Sat.solve ~assumptions:[|x|] solver with
  | Sat.Sat values -> check values.(x) "Completion assumption was ignored"
  | _ -> failwith "Completion assumption was rejected");
  check (not (model solver).(x)) "Assumption leaked into completion";
  Sat.push solver;
  Sat.add_clause solver [|x; y|];
  let values = model solver in
  check (values.(x) || values.(y)) "Scoped clause failed to activate variables";
  Sat.pop solver;
  Sat.add_clause solver [|-x|];
  Sat.add_clause solver [|y|];
  let values = model solver in
  check (not values.(x) && values.(y)) "Later clauses lost activation";
  let stage = ref 0 and first = ref 0 and later = ref 0 in
  let extension : Sat.extension =
    { start = (fun () -> ()); push_level = (fun () -> ());
      pop_levels = (fun _ -> ()); assigned = (fun _ _ -> ());
      propagate = (fun _ -> ()); finish = (fun () -> ());
      final_check = (fun context ->
        match !stage with
        | 0 ->
          check (Sat.assigned_value context !first = None)
            "Completion was reported as an assignment";
          check (Sat.completion_value context !first = Some false)
            "Positive completion value";
          check (Sat.completion_value context (- !first) = Some true)
            "Negative completion value";
          expect_invalid (fun () ->
            Sat.conflict context ~reason:(fun () -> [|- !first|]));
          later := Sat.new_theory_var ~completion:true context;
          Sat.add_lemma context [|!first; !later|];
          stage := 1;
          Sat.Continue
        | _ ->
          check (Sat.assigned_value context !first <> None)
            "Theory lemma failed to activate an existing variable";
          check (Sat.assigned_value context !later <> None)
            "Theory lemma failed to activate a new variable";
          Sat.Done) } in
  let solver = Sat.create ~extension () in
  first := Sat.new_var ~completion:true solver;
  (match Sat.solve ~should_stop:(fun () -> !stage = 1) solver with
  | Sat.Unknown -> ()
  | _ -> failwith "Completion lemma cancellation was ignored");
  let values = model solver in
  check (values.(!first) || values.(!later)) "Theory activation model";
  Sat.add_clause solver [|- !first|];
  Sat.add_clause solver [|- !later|];
  (match Sat.solve solver with Sat.Unsat _ -> ()
  | _ -> failwith "Theory activation lost a permanent lemma")

let late_extension_attachment () =
  let solver = Sat.create ~variables:3 () in
  Sat.add_clause solver [|1|];
  (match Sat.solve solver with
  | Sat.Sat model -> check model.(1) "pure root model"
  | _ -> failwith "pure solve");
  let starts = ref 0 and finishes = ref 0 in
  let seen = ref [] in
  let extension : Sat.extension =
    { start = (fun () -> incr starts; seen := []);
      finish = (fun () -> incr finishes);
      push_level = (fun () -> ()); pop_levels = (fun _ -> ());
      assigned = (fun context literal ->
        seen := literal :: !seen;
        if literal = 1 then
          Sat.imply context 2 ~reason:(fun () -> [|1|]));
      propagate = (fun _ -> ());
      final_check = (fun _ -> Sat.Done) } in
  Sat.attach_extension solver extension;
  (match Sat.solve ~assumptions:[|-3|] solver with
  | Sat.Sat model ->
    check (model.(1) && model.(2) && not model.(3)) "attached model"
  | _ -> failwith "attached solve");
  check (List.mem 1 !seen && List.mem 2 !seen) "root replay";
  check (!starts = 1 && !finishes = 1) "attached lifecycle";
  check (try Sat.attach_extension solver extension; false
    with Invalid_argument _ -> true) "duplicate extension";
  (match Sat.solve ~assumptions:[|-2|] solver with
  | Sat.Unsat core -> check (core = [|-2|]) "attached assumption core"
  | _ -> failwith "attached implication");
  let interrupted = Sat.solve ~should_stop:(fun () -> true) solver in
  check (interrupted = Sat.Unknown) "attached cancellation";
  check (!starts = 3 && !finishes = 3) "attached cancelled lifecycle";
  (match Sat.solve solver with
  | Sat.Sat model -> check model.(2) "attached recovery model"
  | _ -> failwith "attached recovery")

let clause_input_ownership () =
  List.iter (fun completion ->
    List.iter (fun original ->
      let solver = create_solver ~completion 3 [] in
      let clause = Array.copy original in
      Sat.add_clause solver clause;
      check (clause = original) "Clause input reordered";
      Array.fill clause 0 (Array.length clause) 0;
      for bits = 0 to 7 do
        let assumptions = Array.init 3 (fun index ->
          let variable = index + 1 in
          if bits land (1 lsl index) = 0 then -variable else variable) in
        verify solver 3 [original] assumptions
      done)
      [[|3; 1; 2|]; [|3; 2; 1|]; [|2; 3; 1|]; [|1; 3; 2|];
       [|2; 1; 3|]; [|1; 2; 3|]; [|2; 1|]; [|1|]; [|-3; 1; 2|];
       [|2; -2; 1|]; [|3; 3; 1|]])
    [false; true]

let permanent_binary_clauses () =
  List.iter (fun completion ->
    let solver = create_solver ~completion 2 [] in
    let clauses = [|1; 2|] in
    Sat.add_clause solver clauses;
    check (Sat.clause_count solver = 1) "Binary clause count";
    verify solver 2 [clauses] [|-1|];
    verify solver 2 [clauses] [|-1; -2|];
    Sat.push solver;
    let before = Sat.clause_count solver in
    Sat.add_clause solver [|-2|];
    check (Sat.clause_count solver = before + 1) "Guarded unit count";
    verify solver 2 [clauses; [|-2|]] [|-1|];
    verify solver 2 [clauses; [|-2|]] [||];
    Sat.pop solver;
    verify solver 2 [clauses] [|-1|];
    verify solver 2 [clauses] [|-2|]) [false; true]

let lemma_pop_exception () =
  List.iter (fun scoped ->
    List.iter (fun already_true ->
      let emitted = ref false and throw = ref true and q = ref 0 in
      let extension =
        { empty_extension with
          pop_levels = (fun _ ->
            if !throw then begin throw := false; raise Exit end);
          final_check = (fun context ->
            if not !emitted then begin
              q := if already_true then 1
                else Sat.new_theory_var ~completion:true context;
              let clause = if scoped then [|-3; !q|] else [|!q|] in
              Sat.add_lemma context clause;
              emitted := true
            end;
            Sat.Done) }
      in
      let solver = Sat.create ~variables:2 ~extension () in
      if scoped then Sat.push solver;
      (match Sat.solve ~assumptions:[|1; 2|] solver with
      | exception Exit -> ()
      | _ -> failwith "Lemma pop callback did not raise");
      verify solver !q [[|!q|]] [|- !q|];
      if scoped then Sat.pop solver;
      let clauses = if scoped then [] else [[|!q|]] in
      verify solver !q clauses [|- !q|]) [false; true]) [false; true]

let () =
  clause_input_ownership ();
  permanent_binary_clauses ();
  late_extension_attachment ();
  completion_variables ();
  cancelled_root_conflict ();
  deferred_root_work ();
  exhaustive_two_variables ();
  exhaustive_two_variables ~completion:true ();
  randomized_formulas ();
  randomized_formulas ~completion:true ();
  randomized_scopes ();
  randomized_scopes ~completion:true ();
  incremental_regressions ();
  inputs_and_growth ();
  long_propagation ();
  limits_and_restarts ();
  clause_reduction ();
  randomized_extensions ();
  extension_lifecycle ();
  cached_explanation_interruption ();
  interrupted_theory_conflict ();
  lemma_pop_exception ();
  print_endline "SAT solver tests passed"
