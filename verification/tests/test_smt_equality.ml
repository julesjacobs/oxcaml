module Term = Vox_smt_term
module Eq = Vox_smt_equality
module Z = Vox_smt_integer.Z
module Sat = Vox_sat_solver

type binding =
  | Equality of int * int
  | Boolean of int

let check condition message = if not condition then failwith message

let variable terms name sort =
  Term.apply terms
    (Term.symbol terms
       { name; arguments = [||]; result = sort; kind = Term.Uninterpreted })
    [||]

let oracle ?(commutative = fun _ -> false) terms bindings assignments =
  let count = Term.count terms in
  let parents = Array.init count Fun.id in
  let rec find x =
    if parents.(x) = x
    then x
    else
      let root = find parents.(x) in
      parents.(x) <- root;
      root
  in
  let merge x y = parents.(find x) <- find y in
  List.iter
    (fun literal ->
      match bindings.(abs literal - 1) with
      | Equality (x, y) when literal > 0 -> merge x y
      | Boolean x -> merge x (Term.boolean terms (literal > 0))
      | Equality _ -> ())
    assignments;
  let rec close () =
    let changed = ref false in
    for x = 0 to count - 1 do
      for y = x + 1 to count - 1 do
        match Term.node terms x, Term.node terms y with
        | Term.Apply (f, a), Term.Apply (g, b)
          when f = g
               && find x <> find y
               && (if commutative f then
                   let a = Array.map find a and b = Array.map find b in
                   Array.sort Int.compare a; Array.sort Int.compare b;
                   a = b
                 else Array.for_all2 (fun a b -> find a = find b) a b) ->
          merge x y;
          changed := true
        | _ -> ()
      done
    done;
    if !changed then close ()
  in
  close ();
  let conflict = ref false in
  List.iter
    (fun literal ->
      match bindings.(abs literal - 1) with
      | Equality (x, y) when literal < 0 && find x = find y -> conflict := true
      | _ -> ())
    assignments;
  let constants = Hashtbl.create 8 in
  for term = 0 to count - 1 do
    match Term.node terms term with
    | Term.Boolean _ | Term.Integer _ | Term.Bits _ -> (
      let root = find term in
      match Hashtbl.find_opt constants root with
      | Some other when term <> other -> conflict := true
      | _ -> Hashtbl.replace constants root term)
    | Term.Apply _ -> ()
  done;
  !conflict, Array.init count find

let install solver literal = function
  | Equality (x, y) -> Eq.register_equality solver ~literal x y
  | Boolean x -> Eq.register_boolean solver ~literal x

let verify ?(commutative = fun _ -> false) ?negative_propagations
    saved terms bindings solver assignments =
  let expected, classes = oracle ~commutative terms bindings assignments in
  let subset reason =
    check
      (Array.for_all (fun lit -> List.mem lit assignments) reason)
      "Equality explanation contains an unasserted literal"
  in
  let result = Eq.check_lazy solver in
  List.iter (fun (reason, expected) ->
    check (reason () = expected) "Lazy explanation changed after later work")
    !saved;
  match result with
  | Eq.Lazy_conflict reason ->
    check expected "Spurious equality conflict";
    subset reason;
    check
      (fst (oracle ~commutative terms bindings (Array.to_list reason)))
      "Invalid equality conflict explanation"
  | Eq.Lazy_consistent deferred ->
    let propagations = List.map (fun (literal, reason) ->
      let explanation = reason () in
      saved := (reason, explanation) :: !saved;
      literal, explanation) deferred in
    check (not expected) "Missing equality conflict";
    List.iter
      (fun (literal, reason) ->
        (if literal < 0 then match bindings.(-literal - 1) with
        | Equality (left, _) ->
          (match Term.sort terms left with
          | Term.Named _ -> Option.iter incr negative_propagations
          | Term.Bool | Term.Int | Term.Bitvec _ -> ())
        | Boolean _ -> ());
        subset reason;
        check (not (List.mem literal assignments)) "Redundant propagation";
        check
          (fst (oracle ~commutative terms bindings
             (-literal :: Array.to_list reason)))
          "Invalid equality propagation explanation")
      propagations;
    for x = 0 to Term.count terms - 1 do
      for y = x to Term.count terms - 1 do
        check
          (Eq.representative solver x = Eq.representative solver y
          = (classes.(x) = classes.(y)))
          "Congruence closure differs from reference";
        if classes.(x) = classes.(y) && x <> y
        then
          match Eq.explain solver x y with
          | None -> failwith "Missing equality explanation"
          | Some reason ->
            subset reason;
            let _, proof =
              oracle ~commutative terms bindings (Array.to_list reason) in
            check (proof.(x) = proof.(y)) "Invalid equality explanation"
      done
    done

let randomized ?(commutative_enabled = false) ?(pure_uf = false) () =
  let negative_propagations = ref 0 in
  let random = Random.State.make [| 0xc0a9; 0x13 |] in
  for _ = 1 to 200 do
    let terms = Term.create () in
    let named = Term.Named 0 in
    let integer_sort = if pure_uf then Term.Bool else Term.Int in
    let groups = Hashtbl.create 3 in
    let add sort term =
      let previous = Option.value (Hashtbl.find_opt groups sort) ~default:[] in
      Hashtbl.replace groups sort (term :: previous);
      term
    in
    List.iter
      (fun b -> ignore (add Term.Bool (Term.boolean terms b)))
      [false; true];
    if not pure_uf then List.iter
      (fun n -> ignore (add Term.Int (Term.integer terms (Z.of_int n))))
      [0; 1];
    for i = 0 to 3 do
      ignore (add named (variable terms ("a" ^ string_of_int i) named))
    done;
    let signatures =
      [| "f", [| named |], named;
         "g", [| named |], integer_sort;
         "p", [| named |], Term.Bool;
         "h", [| integer_sort; Term.Bool |], named;
         "c", [| named; named |], named;
         "d", [| named; named; named |], named
      |]
      |> Array.map (fun (name, arguments, result) ->
          let symbol =
            Term.symbol terms
              { name; arguments; result; kind = Term.Uninterpreted }
          in
          symbol, arguments, result)
    in
    let choose sort =
      let choices = Array.of_list (Hashtbl.find groups sort) in
      choices.(Random.State.int random (Array.length choices))
    in
    for _ = 1 to 14 do
      let symbol, arguments, result =
        signatures.(Random.State.int random (Array.length signatures))
      in
      ignore
        (add result (Term.apply terms symbol (Array.map choose arguments)))
    done;
    let bindings =
      Array.init 32 (fun _ ->
          if Random.State.int random 4 = 0
          then Boolean (choose Term.Bool)
          else
            let sort =
              [| named; integer_sort; Term.Bool |].(Random.State.int random 3)
            in
            Equality (choose sort, choose sort))
    in
    let commutative symbol =
      commutative_enabled && match (Term.signature terms symbol).name with
        | "c" | "d" -> true | _ -> false in
    let solver = Eq.create ~terms ~commutative () in
    for term = 0 to Term.count terms - 1 do
      Eq.register solver term
    done;
    Array.iteri (fun i binding -> install solver (i + 1) binding) bindings;
    let assignments = ref [] and scopes = ref [] in
    let saved = ref [] in
    for _ = 1 to 60 do
      (match Random.State.int random 6, !scopes with
      | 0, _ ->
        Eq.push solver;
        scopes := (!assignments, !saved) :: !scopes
      | 1, (previous, previous_saved) :: rest ->
        Eq.pop solver;
        assignments := previous;
        saved := previous_saved;
        scopes := rest
      | 2, _ ->
        Eq.reset solver;
        assignments := [];
        saved := [];
        scopes := []
      | _, _ ->
        let literal = 1 + Random.State.int random (Array.length bindings) in
        if not (List.exists (fun x -> abs x = literal) !assignments)
        then begin
          let literal =
            if Random.State.bool random then literal else -literal
          in
          Eq.assert_literal solver literal;
          assignments := literal :: !assignments
        end);
      verify ~commutative ~negative_propagations
        saved terms bindings solver !assignments
    done
  done;
  if pure_uf then
    check (!negative_propagations > 0) "Random disequality propagation absent"

let dynamics () =
  let terms = Term.create () in
  let sort = Term.Named 0 in
  let a = variable terms "a" sort and b = variable terms "b" sort in
  let f =
    Term.symbol terms
      { name = "f";
        arguments = [| sort |];
        result = sort;
        kind = Term.Uninterpreted
      }
  in
  let solver = Eq.create ~terms () in
  Eq.register_equality solver ~literal:1 a b;
  check (Eq.take_changed solver = List.sort Int.compare [a; b])
    "Registration omitted a changed class";
  check (Eq.take_changed solver = []) "Changed classes were not drained";
  Eq.assert_literal solver 1;
  Eq.push solver;
  let fa = Term.apply terms f [| a |] and fb = Term.apply terms f [| b |] in
  Eq.register_equality solver ~literal:2 fa fb;
  (match Eq.check solver with
  | Eq.Consistent props ->
    check (List.mem_assoc 2 props) "New congruence missed"
  | _ -> failwith "Consistent dynamic registration rejected");
  Eq.pop solver;
  (match Eq.check solver with
  | Eq.Consistent props ->
    check (List.mem_assoc 2 props) "Registration lost on pop"
  | _ -> failwith "Pop corrupted dynamic registration");
  Eq.reset solver;
  Eq.assert_literal solver (-2);
  (match Eq.check solver with
  | Eq.Consistent _ -> ()
  | _ -> failwith "Root assertion survived reset");
  Eq.push solver;
  Eq.assert_literal solver 1;
  (match Eq.check solver with
  | Eq.Conflict reason ->
    check
      (Array.mem 1 reason && Array.mem (-2) reason)
      "Congruence conflict core"
  | _ -> failwith "Congruence disequality accepted");
  Eq.pop solver;
  (match Eq.check solver with
  | Eq.Consistent _ -> ()
  | _ -> failwith "Congruence conflict survived pop");
  Eq.reset solver;
  Eq.assert_literal solver 3;
  Eq.push solver;
  Eq.register_equality solver ~literal:3 a b;
  ignore (Eq.check solver);
  Eq.pop solver;
  ignore (Eq.check solver);
  check
    (Eq.representative solver a = Eq.representative solver b)
    "Binding added below its assignment was lost";
  Eq.reset solver;
  Eq.push solver;
  Eq.assert_literal solver 1;
  Eq.push solver;
  Eq.pop solver;
  ignore (Eq.check solver);
  check
    (Eq.representative solver a = Eq.representative solver b)
    "Pending merge lost on nested pop";
  Eq.pop solver;
  ignore (Eq.check solver);
  check
    (Eq.representative solver a <> Eq.representative solver b)
    "Pending merge survived its assertion"

let long_explanation () =
  let terms = Term.create () in
  let count = 10000 in
  let values =
    Array.init count (fun i -> variable terms (string_of_int i) (Term.Named 0))
  in
  let solver = Eq.create ~terms () in
  for i = 1 to count - 1 do
    Eq.register_equality solver ~literal:i values.(i - 1) values.(i)
  done;
  Eq.push solver;
  for i = 1 to count - 1 do
    Eq.assert_literal solver i
  done;
  ignore (Eq.check solver);
  (match Eq.explain solver values.(0) values.(count - 1) with
  | Some reason ->
    check (Array.length reason = count - 1) "Long equality proof"
  | None -> failwith "Long equality chain missing");
  Eq.pop solver;
  ignore (Eq.check solver);
  check
    (Eq.explain solver values.(0) values.(count - 1) = None)
    "Long equality chain survived pop"

let cancellation () =
  let terms = Term.create () in
  let stop = ref false in
  let solver =
    Eq.create ~terms ~poll:(fun () -> if !stop then raise Exit) ()
  in
  let a = variable terms "a" Term.Int and b = variable terms "b" Term.Int in
  Eq.register_equality solver ~literal:1 a b;
  Eq.assert_literal solver 1;
  stop := true;
  (match Eq.check solver with
  | _ -> failwith "Equality cancellation ignored"
  | exception Exit -> ());
  Eq.reset solver;
  stop := false;
  Eq.assert_literal solver (-1);
  match Eq.check solver with
  | Eq.Consistent _ -> ()
  | _ -> failwith "Equality cancellation corrupted state"

let sat_integration () =
  let random = Random.State.make [| 0xec1a; 0x3f1 |] in
  for _ = 1 to 150 do
    let terms = Term.create () in
    let sort = Term.Named 0 in
    let a = variable terms "a" sort and b = variable terms "b" sort in
    let c = variable terms "c" sort in
    let f =
      Term.symbol terms
        { name = "f";
          arguments = [| sort |];
          result = sort;
          kind = Term.Uninterpreted
        }
    in
    let p =
      Term.symbol terms
        { name = "p";
          arguments = [| sort |];
          result = Term.Bool;
          kind = Term.Uninterpreted
        }
    in
    let fa = Term.apply terms f [| a |] and fb = Term.apply terms f [| b |] in
    let fc = Term.apply terms f [| c |] in
    let pa = Term.apply terms p [| fa |]
    and pb = Term.apply terms p [| fb |] in
    ignore (Term.boolean terms true);
    ignore (Term.boolean terms false);
    let bindings =
      [| Equality (a, b);
         Equality (a, c);
         Equality (b, c);
         Equality (fa, fb);
         Equality (fa, fc);
         Equality (fb, fc);
         Boolean pa;
         Boolean pb;
         Equality (pa, pb)
      |]
    in
    let variables = Array.length bindings in
    let equality = Eq.create ~terms () in
    Array.iteri (fun i binding -> install equality (i + 1) binding) bindings;
    let propagate context =
      match Eq.check_lazy equality with
      | Eq.Lazy_conflict reason -> Sat.conflict context ~reason:(fun () -> reason)
      | Eq.Lazy_consistent propagations ->
        List.iter
          (fun (literal, reason) ->
            Sat.imply context literal ~reason)
          propagations
    in
    let extension =
      { Sat.start = (fun () -> Eq.reset equality);
        push_level = (fun () -> Eq.push equality);
        pop_levels =
          (fun n ->
            for _ = 1 to n do
              Eq.pop equality
            done);
        assigned = (fun _ literal -> Eq.assert_literal equality literal);
        propagate;
        final_check =
          (fun context ->
            propagate context;
            Sat.Done);
        finish = (fun () -> Eq.reset equality)
      }
    in
    let sat = Sat.create ~variables ~extension () in
    let literal () =
      let x = 1 + Random.State.int random variables in
      if Random.State.bool random then x else -x
    in
    let clauses =
      List.init (Random.State.int random 20) (fun _ ->
          Array.init (1 + Random.State.int random 3) (fun _ -> literal ()))
    in
    List.iter (Sat.add_clause sat) clauses;
    let possible = ref [] in
    for bits = 0 to (1 lsl variables) - 1 do
      let value literal =
        bits land (1 lsl (abs literal - 1)) <> 0 = (literal > 0)
      in
      if List.for_all (Array.exists value) clauses
      then begin
        let assignment =
          List.init variables (fun i ->
              if value (i + 1) then i + 1 else -(i + 1))
        in
        if not (fst (oracle terms bindings assignment))
        then possible := bits :: !possible
      end
    done;
    for _ = 1 to 10 do
      let assumptions =
        Array.init (Random.State.int random 4) (fun _ -> literal ())
      in
      let admits assumptions bits =
        Array.for_all
          (fun literal ->
            bits land (1 lsl (abs literal - 1)) <> 0 = (literal > 0))
          assumptions
      in
      let expected = List.exists (admits assumptions) !possible in
      match Sat.solve ~assumptions sat with
      | Sat.Sat model ->
        check expected "Spurious CDCL(T) SAT";
        let bits = ref 0 in
        for i = 1 to variables do
          if model.(i) then bits := !bits lor (1 lsl (i - 1))
        done;
        check
          (List.mem !bits !possible && admits assumptions !bits)
          "Invalid CDCL(T) equality model"
      | Sat.Unsat core ->
        check (not expected) "Spurious CDCL(T) UNSAT";
        check
          (Array.for_all (fun x -> Array.mem x assumptions) core)
          "CDCL(T) core contains non-assumption";
        check
          (not (List.exists (admits core) !possible))
          "Invalid CDCL(T) equality core"
      | Sat.Unknown -> failwith "Unexpected CDCL(T) unknown"
    done
  done

let snapshots () =
  let terms = Term.create () in
  let a = variable terms "a" (Term.Named 0) in
  let b = variable terms "b" (Term.Named 0) in
  let c = variable terms "c" (Term.Named 0) in
  let solver = Eq.create ~terms () in
  let revision = Eq.revision solver in
  let empty = Eq.snapshot solver in
  check
    (empty.(a) = a && empty.(b) = b && empty.(c) = c)
    "Unregistered snapshot terms were merged";
  check (Eq.revision solver = revision) "Snapshot changed equality state";
  Eq.register_equality solver ~literal:1 a b;
  check (Eq.revision solver > revision) "Registration did not change revision";
  let registered = Eq.revision solver in
  Eq.push solver;
  Eq.assert_literal solver 1;
  ignore (Eq.check solver);
  let merged = Eq.snapshot solver in
  check (Eq.take_changed solver = [merged.(a)])
    "Merge omitted its changed class";
  check (List.sort Int.compare (Eq.members solver a) =
    List.sort Int.compare [a; b]) "Merged class members are incomplete";
  check (merged.(a) = merged.(b) && merged.(c) = c) "Incorrect merged snapshot";
  check (Eq.revision solver > registered) "Merge did not change revision";
  let revision = Eq.revision solver in
  Eq.pop solver;
  check (Eq.take_changed solver = List.sort Int.compare [a; b])
    "Rollback omitted a restored class";
  check (Eq.revision solver > revision) "Undo did not change revision";
  check (Eq.snapshot solver = empty) "Snapshot retained a popped merge";
  let d = variable terms "d" (Term.Named 0) in
  let revision = Eq.revision solver in
  check ((Eq.snapshot solver).(d) = d) "New term missing from snapshot";
  check (Eq.revision solver = revision) "Snapshot registered a new term"

let late_literal_growth () =
  let terms = Term.create () in
  let a = variable terms "a" (Term.Named 0) in
  let b = variable terms "b" (Term.Named 0) in
  let c = variable terms "c" (Term.Named 0) in
  let solver = Eq.create ~terms () in
  Eq.assert_literal solver 4096;
  Eq.register_equality solver ~literal:4096 a b;
  Eq.register_equality solver ~literal:7 a c;
  ignore (Eq.check solver);
  check (Eq.representative solver a = Eq.representative solver b)
    "Late binding missed an asserted literal";
  Eq.push solver;
  Eq.assert_literal solver 7;
  Eq.assert_literal solver (-8192);
  Eq.register_equality solver ~literal:8192 b c;
  (match Eq.check solver with
  | Eq.Conflict reason ->
    check (List.sort Int.compare (Array.to_list reason) = [-8192; 7; 4096])
      "Resized literal conflict lost an antecedent"
  | Eq.Consistent _ -> failwith "Resized literal conflict was missed");
  Eq.pop solver;
  (match Eq.check solver with
  | Eq.Conflict _ -> failwith "Resized literal conflict survived pop"
  | Eq.Consistent _ -> ());
  check (Eq.representative solver a <> Eq.representative solver c)
    "Resized literal assignment survived pop";
  Eq.reset solver;
  ignore (Eq.check solver);
  check (Eq.representative solver a <> Eq.representative solver b)
    "Resized literal assignment survived reset"

let deferred_cancellation () =
  let terms = Term.create () in
  let stop = ref false in
  let solver = Eq.create ~terms ~poll:(fun () -> if !stop then raise Exit) () in
  let sort = Term.Named 0 in
  let a = variable terms "a" sort and b = variable terms "b" sort in
  let c = variable terms "c" sort in
  Eq.register_equality solver ~literal:1 a b;
  Eq.register_equality solver ~literal:2 b c;
  Eq.register_equality solver ~literal:3 a c;
  Eq.push solver;
  Eq.assert_literal solver 1;
  Eq.assert_literal solver 2;
  let reason = match Eq.check_lazy solver with
    | Eq.Lazy_consistent props -> List.assoc 3 props
    | Eq.Lazy_conflict _ -> failwith "Unexpected deferred conflict" in
  stop := true;
  (match reason () with
   | _ -> failwith "Deferred explanation ignored cancellation"
   | exception Exit -> ());
  Eq.reset solver;
  stop := false;
  Eq.assert_literal solver (-3);
  (match Eq.check_lazy solver with
   | Eq.Lazy_conflict _ -> failwith "Cancellation survived reset"
   | Eq.Lazy_consistent _ -> ());
  Eq.reset solver;
  Eq.assert_literal solver 1;
  Eq.assert_literal solver 2;
  match Eq.check_lazy solver with
  | Eq.Lazy_conflict _ -> failwith "Repeated check failed"
  | Eq.Lazy_consistent props ->
    check ((List.assoc 3 props) () = [|1; 2|])
      "Repeated check lost its deferred reason"

let deferred_reroot () =
  let terms = Term.create () in
  let sort = Term.Named 0 in
  let nodes = Array.init 48 (fun i -> variable terms ("v" ^ string_of_int i) sort) in
  let solver = Eq.create ~terms () in
  for i = 1 to 3 do
    Eq.register_equality solver ~literal:i nodes.(i - 1) nodes.(i)
  done;
  Eq.register_equality solver ~literal:4 nodes.(0) nodes.(3);
  Eq.push solver;
  for i = 1 to 3 do Eq.assert_literal solver i done;
  let reason = match Eq.check_lazy solver with
    | Eq.Lazy_consistent props -> List.assoc 4 props
    | Eq.Lazy_conflict _ -> failwith "Initial chain conflicted" in
  let expected = reason () in
  check (expected = [|1; 2; 3|]) "Initial chain explanation";
  for branch = 0 to 3 do
    Eq.push solver;
    for i = 5 to 47 do
      Eq.register_equality solver ~literal:(i + 10) nodes.(i - 1) nodes.(i);
      Eq.assert_literal solver (i + 10)
    done;
    Eq.register_equality solver ~literal:(100 + branch)
      nodes.(branch) nodes.(47);
    Eq.assert_literal solver (100 + branch);
    ignore (Eq.check_lazy solver);
    check (reason () = expected) "Reroot changed a deferred explanation";
    Eq.pop solver;
    ignore (Eq.check_lazy solver);
    check (reason () = expected) "Deeper pop changed a deferred explanation"
  done;
  Eq.pop solver;
  Eq.reset solver

let nested_theory_explanations () =
  let terms = Term.create () in
  let a = variable terms "a" Term.Bool in
  let b = variable terms "b" Term.Bool in
  let wrap name input datatype =
    let symbol = Term.symbol terms
        { name; arguments = [|input|]; result = Term.Named datatype;
          kind = Term.Constructor datatype } in
    fun argument -> Term.apply terms symbol [|argument|]
  in
  let inner = wrap "Inner" Term.Bool 0 in
  let outer = wrap "Outer" (Term.Named 0) 1 in
  let inner_a = inner a and inner_b = inner b in
  let outer_a = outer inner_a and outer_b = outer inner_b in
  let stopped = ref false and interrupt = ref false in
  let poll () = if !stopped then raise Exit in
  let solver = Eq.create ~terms ~poll () in
  Eq.register_equality solver ~literal:1 outer_a outer_b;
  Eq.register_boolean solver ~literal:2 a;
  Eq.register_boolean solver ~literal:3 b;
  let consistent () = match Eq.check_lazy solver with
    | Eq.Lazy_consistent props -> props
    | Eq.Lazy_conflict _ -> failwith "Nested equality conflict" in
  Eq.push solver;
  Eq.assert_literal solver 1;
  ignore (consistent ());
  Eq.equate solver inner_a inner_b
    ~reason:(Eq.explain_later solver outer_a outer_b);
  check (Eq.has_merges solver) "Missing queued equality";
  ignore (consistent ());
  let reason = Eq.explain_later solver inner_a inner_b in
  Eq.equate solver a b ~reason:(fun () ->
    if !interrupt then stopped := true;
    reason ());
  ignore (consistent ());
  check (not (Eq.has_merges solver)) "Queued equality not saturated";
  Eq.push solver;
  Eq.assert_literal solver 2;
  let deferred = List.assoc 3 (consistent ()) in
  check (deferred () = [|1; 2|]) "Nested equality explanation";
  interrupt := true;
  (match deferred () with
  | _ -> failwith "Nested explanation interruption missed"
  | exception Exit -> ());
  stopped := false;
  interrupt := false;
  Eq.pop solver;
  ignore (consistent ());
  check (reason () = [|1|]) "Nested explanation changed after pop";
  Eq.pop solver;
  Eq.assert_literal solver (-2);
  Eq.assert_literal solver 3;
  ignore (consistent ());
  check (Eq.representative solver a <> Eq.representative solver b)
    "Nested equality survived pop";
  (match Eq.equate solver a inner_a ~reason:(fun () -> [||]) with
  | _ -> failwith "Cross-sort equality accepted"
  | exception Invalid_argument _ -> ());
  Eq.reset solver;
  ignore (consistent ())

let disequality_cancellation () =
  let fixture poll =
    let terms = Term.create () and sort = Term.Named 0 in
    let a = variable terms "a" sort and b = variable terms "b" sort in
    let c = variable terms "c" sort and d = variable terms "d" sort in
    let solver = Eq.create ~terms ~poll () in
    List.iter (fun (literal, left, right) ->
      Eq.register_equality solver ~literal left right)
      [1, a, b; 2, c, d; 3, a, c; 4, b, d];
    let prove () =
      List.iter (Eq.assert_literal solver) [1; 2; -3];
      match Eq.check_lazy solver with
      | Eq.Lazy_consistent props ->
        check ((List.assoc (-4) props) () = [|-3; 1; 2|])
          "Cancelled disequality explanation"
      | Eq.Lazy_conflict _ -> failwith "Cancelled disequality conflicted" in
    let recover () =
      Eq.pop solver;
      (match Eq.check solver with
      | Eq.Consistent props ->
        check (not (List.mem_assoc (-4) props))
          "Cancelled disequality survived pop"
      | Eq.Conflict _ -> failwith "Cancelled disequality conflict survived");
      Eq.push solver;
      prove ();
      Eq.reset solver;
      (match Eq.check solver with
      | Eq.Consistent props ->
        check (not (List.mem_assoc (-4) props))
          "Cancelled disequality survived reset"
      | Eq.Conflict _ -> failwith "Cancelled disequality reset conflicted") in
    Eq.push solver;
    prove, recover in
  let calls = ref 0 and armed = ref false in
  let prove, recover = fixture (fun () -> if !armed then incr calls) in
  armed := true;
  prove ();
  let count = !calls in
  armed := false;
  recover ();
  for stop = 1 to count do
    let calls = ref 0 and armed = ref false in
    let prove, recover = fixture (fun () ->
      if !armed then begin
        incr calls;
        if !calls = stop then raise Exit
      end) in
    armed := true;
    (match prove () with
    | () -> failwith "Disequality cancellation missed"
    | exception Exit -> ());
    armed := false;
    recover ()
  done

let disequality_propagation ?(mixed = false) () =
  let terms = Term.create () in
  let sort = Term.Named 0 in
  let a = variable terms "a" sort and b = variable terms "b" sort in
  let c = variable terms "c" sort and d = variable terms "d" sort in
  let solver = Eq.create ~terms () in
  List.iter (fun (literal, left, right) ->
    Eq.register_equality solver ~literal left right)
    [1, a, b; 2, c, d; 3, a, c; 4, b, d];
  Eq.push solver;
  Eq.assert_literal solver 1;
  Eq.assert_literal solver 2;
  ignore (Eq.check solver);
  Eq.push solver;
  Eq.assert_literal solver (-3);
  let reason = match Eq.check_lazy solver with
    | Eq.Lazy_consistent props -> List.assoc (-4) props
    | Eq.Lazy_conflict _ -> failwith "Disequality propagation conflicted" in
  check (reason () = [|-3; 1; 2|]) "Disequality propagation explanation";
  if mixed then Eq.register solver (Term.integer terms Z.zero);
  Eq.push solver;
  Eq.assert_literal solver 4;
  (match Eq.check solver with
  | Eq.Conflict _ -> ()
  | Eq.Consistent _ -> failwith "Disequality conflict missing");
  check (reason () = [|-3; 1; 2|]) "Deferred disequality changed";
  Eq.pop solver;
  Eq.pop solver;
  (match Eq.check solver with
  | Eq.Consistent props ->
    check (not (List.mem_assoc (-4) props)) "Disequality survived pop"
  | Eq.Conflict _ -> failwith "Disequality conflict survived pop");
  Eq.assert_literal solver 4;
  ignore (Eq.check solver);
  check (Eq.representative solver a = Eq.representative solver c)
    "Disequality rollback retained relation";
  Eq.pop solver

let commutative_cancellation () =
  let fixture poll =
    let terms = Term.create () in
    let sort = Term.Named 0 and width = 65 in
    let variables prefix = Array.init width (fun index ->
      variable terms (prefix ^ string_of_int index) sort) in
    let left = variables "left" and right = variables "right" in
    let symbol = Term.symbol terms
        { name = "commutative"; arguments = Array.make width sort;
          result = sort; kind = Term.Uninterpreted } in
    let a = Term.apply terms symbol left in
    let b = Term.apply terms symbol
        (Array.init width (fun index -> right.(width - index - 1))) in
    let solver = Eq.create ~terms ~poll ~commutative:((=) symbol) () in
    Array.iteri (fun index term ->
      Eq.register_equality solver ~literal:(index + 1) term right.(index))
      left;
    Eq.register_equality solver ~literal:(width + 1) a b;
    let assert_pairs () =
      for literal = 1 to width do Eq.assert_literal solver literal done in
    let verify () = match Eq.check_lazy solver with
      | Eq.Lazy_consistent props ->
        check ((List.assoc (width + 1) props) () = Array.init width ((+) 1))
          "N-ary commutative explanation"
      | Eq.Lazy_conflict _ -> failwith "N-ary commutative conflict" in
    let recover () =
      Eq.pop solver;
      ignore (Eq.check solver);
      check (Eq.representative solver a <> Eq.representative solver b)
        "Interrupted n-ary commutative equality survived pop";
      Eq.push solver;
      assert_pairs ();
      verify ();
      Eq.pop solver in
    Eq.push solver;
    assert_pairs ();
    verify, recover in
  let calls = ref 0 and armed = ref false in
  let verify, recover = fixture (fun () -> if !armed then incr calls) in
  armed := true;
  verify ();
  let count = !calls in
  armed := false;
  recover ();
  for stop = 1 to count do
    let calls = ref 0 and armed = ref false in
    let verify, recover = fixture (fun () ->
      if !armed then begin
        incr calls;
        if !calls = stop then raise Exit
      end) in
    armed := true;
    (match verify () with
    | () -> failwith "N-ary commutative cancellation missed"
    | exception Exit -> ());
    armed := false;
    recover ()
  done

let commutative_deferred () =
  List.iter (fun width ->
    let terms = Term.create () in
    let sort = Term.Named 0 in
    let variables prefix = Array.init width (fun index ->
      variable terms (prefix ^ string_of_int index) sort) in
    let left = variables "left" and right = variables "right" in
    let symbol = Term.symbol terms
        { name = "commutative"; arguments = Array.make width sort;
          result = sort; kind = Term.Uninterpreted } in
    let a = Term.apply terms symbol left in
    let b = Term.apply terms symbol
        (Array.init width (fun index -> right.(width - index - 1))) in
    let solver = Eq.create ~terms ~commutative:((=) symbol) () in
    Array.iteri (fun index term ->
      Eq.register_equality solver ~literal:(index + 1) term right.(index))
      left;
    let conclusion = width + 1 in
    Eq.register_equality solver ~literal:conclusion a b;
    Eq.push solver;
    for literal = 1 to width do Eq.assert_literal solver literal done;
    let reason = match Eq.check_lazy solver with
      | Eq.Lazy_consistent props -> List.assoc conclusion props
      | Eq.Lazy_conflict _ -> failwith "Commutative equality conflicted" in
    let expected = Array.init width ((+) 1) in
    check (reason () = expected) "Commutative explanation";
    Eq.push solver;
    for index = 1 to width - 1 do
      let literal = width + 1 + index in
      Eq.register_equality solver ~literal left.(0) left.(index);
      Eq.assert_literal solver literal
    done;
    ignore (Eq.check solver);
    check (reason () = expected) "Commutative deferred permutation changed";
    Eq.pop solver;
    ignore (Eq.check solver);
    check (reason () = expected) "Commutative deferred permutation after pop";
    Eq.pop solver;
    ignore (Eq.check solver);
    check (Eq.representative solver a <> Eq.representative solver b)
      "Commutative equality survived pop") [2; 3; 65]

let filtered_congruence () =
  let terms = Term.create () in
  let sort = Term.Named 0 in
  let a = variable terms "a" sort and b = variable terms "b" sort in
  let symbol name = Term.symbol terms
      {name; arguments = [|sort|]; result = sort;
       kind = Term.Uninterpreted} in
  let f = symbol "f" and g = symbol "g" in
  let fa = Term.apply terms f [|a|] and fb = Term.apply terms f [|b|] in
  let ga = Term.apply terms g [|fa|] and gb = Term.apply terms g [|fb|] in
  let solver = Eq.create ~terms ~congruence:(fun symbol -> symbol <> g) () in
  Eq.register solver ga;
  Eq.register solver gb;
  check (Eq.is_registered solver fa && Eq.is_registered solver fb)
    "Filtered application omitted its children";
  check (Eq.uses solver fa = [] && Eq.uses solver fb = [])
    "Filtered application entered argument uses";
  check (Eq.uses solver a = [fa] && Eq.uses solver b = [fb])
    "Nested application omitted argument uses";
  Eq.register_equality solver ~literal:1 a b;
  Eq.push solver;
  Eq.assert_literal solver 1;
  ignore (Eq.check solver);
  check (Eq.representative solver fa = Eq.representative solver fb)
    "Nested congruence was filtered";
  check (Eq.representative solver ga <> Eq.representative solver gb)
    "Filtered application entered congruence signatures";
  Eq.equate solver ga gb ~reason:(fun () -> [|1|]);
  ignore (Eq.check solver);
  check (Eq.explain solver ga gb = Some [|1|])
    "Filtered application lost external equality";
  Eq.pop solver;
  ignore (Eq.check solver);
  check (Eq.representative solver fa <> Eq.representative solver fb
    && Eq.representative solver ga <> Eq.representative solver gb)
    "Filtered congruence survived rollback"

let constructor_members () =
  let terms = Term.create () in
  let state = Random.State.make [|2701; 151|] in
  let values = Array.init 96 (fun index ->
    let kind = if index mod 3 = 0 then Term.Constructor 0 else Term.Uninterpreted in
    let symbol = Term.symbol terms
        {name = string_of_int index; arguments = [||];
         result = Term.Named 0; kind} in
    Term.apply terms symbol [||]) in
  let solver = Eq.create ~terms () in
  let registered = ref 0 and depth = ref 0 in
  let inspect () =
    ignore (Eq.check solver);
    for index = 0 to !registered - 1 do
      let expected = Eq.members solver values.(index)
        |> List.filter (fun term -> match Term.node terms term with
          | Term.Apply (symbol, _) ->
            (match (Term.signature terms symbol).kind with
            | Term.Constructor _ -> true
            | _ -> false)
          | _ -> false) in
      if Eq.constructor_members solver values.(index) <> expected then
        failwith "Constructor membership changed across scope operation"
    done in
  for event = 0 to 3000 do
    (match Random.State.int state 8 with
    | 0 when !registered < Array.length values ->
      Eq.register solver values.(!registered); incr registered
    | 1 when !depth < 8 -> Eq.push solver; incr depth
    | 2 when !depth > 0 -> Eq.pop solver; decr depth
    | 3 when event mod 20 = 0 -> Eq.reset solver; depth := 0
    | _ when !registered > 1 ->
      let left = values.(Random.State.int state !registered) in
      let right = values.(Random.State.int state !registered) in
      Eq.equate solver left right ~reason:(fun () -> [|event + 1|])
    | _ -> ());
    inspect ()
  done;
  Eq.reset solver;
  inspect ();
  Printf.printf "%d constructor membership events passed\n" 3001

let disequality_notification_case size_a size_b reverse late =
  let terms = Term.create () in
  let variable name = Term.apply terms (Term.symbol terms
    { name; arguments = [||]; result = Term.Named 0; kind = Term.Uninterpreted })
    [||] in
  let a = Array.init size_a (fun i -> variable ("a" ^ string_of_int i)) in
  let b = Array.init size_b (fun i -> variable ("b" ^ string_of_int i)) in
  let equality = Eq.create ~terms () in
  let bindings = Hashtbl.create 128 in
  let assignments = ref [] and seen = Hashtbl.create 128 in
  let next = ref 1 in
  let bind x y =
    let literal = !next in incr next;
    Hashtbl.add bindings literal (x,y);
    Eq.register_equality equality ~literal x y;
    literal in
  let target = Array.map (fun x -> Array.map (fun y -> bind x y) b) a in
  let links = Array.concat
      [Array.init (size_a-1) (fun i -> bind a.(i) a.(i+1));
       Array.init (size_b-1) (fun i -> bind b.(i) b.(i+1))] in
  let valid reason negative =
    let parents = Array.init (Term.count terms) Fun.id in
    let rec root x = if parents.(x)=x then x else root parents.(x) in
    Array.iter (fun literal ->
      check (List.mem literal !assignments) "Unasserted explanation literal";
      if literal > 0 then let x,y = Hashtbl.find bindings literal in
        parents.(root x) <- root y) reason;
    let x,y = Hashtbl.find bindings (abs negative) in
    Array.exists (fun literal -> literal < 0 &&
      let u,v = Hashtbl.find bindings (-literal) in
      (root x=root u && root y=root v)
      || (root x=root v && root y=root u)) reason in
  let observe () = match Eq.check_lazy equality with
    | Eq.Lazy_conflict _ -> failwith "Unexpected disequality conflict"
    | Eq.Lazy_consistent props -> List.iter (fun (literal, reason) ->
      if literal < 0 then begin
        check (valid (reason ()) literal) "Invalid disequality explanation";
        Hashtbl.replace seen (-literal) ()
      end) props in
  let assert_ literal =
    assignments := literal :: !assignments;
    Eq.assert_literal equality literal; observe () in
  Eq.push equality;
  if reverse then Array.iter assert_ links;
  assert_ (-target.(0).(0));
  if not reverse then Array.iter assert_ links;
  if late then begin
    for i = 0 to size_a-1 do for j = 0 to size_b-1 do
      let literal = bind a.(i) b.(j) in
      observe ();
      check (Hashtbl.mem seen literal) "Missing late disequality propagation"
    done done
  end;
  Array.iter (Array.iter (fun literal ->
    check (literal = target.(0).(0) || Hashtbl.mem seen literal)
      "Missing smaller-class disequality propagation")) target;
  Eq.pop equality; assignments := []; Hashtbl.clear seen;
  observe ();
  check (Hashtbl.length seen=0) "Disequality survived rollback";
  Array.iter assert_ links;
  observe ();
  check (Hashtbl.length seen=0) "Stale disequality after fresh merges";
  Eq.reset equality; assignments := []; Hashtbl.clear seen; observe ();
  check (Hashtbl.length seen=0) "Disequality survived reset"

let disequality_notifications () =
  List.iter (fun (a,b) -> List.iter (fun reverse ->
    List.iter (disequality_notification_case a b reverse) [false;true]) [false;true])
    [1,1;1,13;13,1;3,17;17,3;16,16];
  print_endline "24 asymmetric/late-registration disequality scenarios passed"

let signature_rollback () =
  let terms = Term.create () and sort = Term.Named 0 in
  let width = 129 in
  let values = Array.init width (fun index ->
    variable terms ("v" ^ string_of_int index) sort) in
  let bindings = Array.init (width - 1) (fun index ->
    Equality (values.(index), values.(index + 1))) in
  let remaining = ref 0 in
  let solver = Eq.create ~terms ~poll:(fun () ->
    if !remaining > 0 then begin
      decr remaining;
      if !remaining = 0 then raise Exit
    end) () in
  Array.iteri (fun index -> install solver (index + 1)) bindings;
  let applications arity =
    let symbol = Term.symbol terms
        { name = "f" ^ string_of_int arity;
          arguments = Array.make arity sort;
          result = sort; kind = Term.Uninterpreted } in
    Array.map (fun value ->
      let term = Term.apply terms symbol (Array.make arity value) in
      Eq.register solver term;
      term) values in
  let unary = applications 1 in
  List.iter (fun arity -> ignore (applications arity)) [2; 3; 5];
  let normalize classes =
    let minimum = Array.make (Array.length classes) max_int in
    Array.iteri (fun term root ->
      minimum.(root) <- min term minimum.(root)) classes;
    Array.map (Array.get minimum) classes in
  let inspect assignments =
    (match Eq.check_lazy solver with
    | Eq.Lazy_consistent _ -> ()
    | Eq.Lazy_conflict _ -> failwith "Signature rollback produced a conflict");
    let conflict, expected = oracle terms bindings assignments in
    check (not conflict) "Signature rollback oracle conflicted";
    check (normalize (Eq.snapshot solver) = normalize expected)
      "Signature rollback changed the congruence partition" in
  Eq.push solver;
  inspect [];
  let outer = List.init 32 (fun index -> 2 * index + 1) in
  List.iter (Eq.assert_literal solver) outer;
  inspect outer;
  let deferred = Eq.explain_later solver unary.(0) unary.(1) in
  let expected_reason = deferred () in
  check (expected_reason = [|1|]) "Outer congruence explanation";
  for branch = 0 to 3 do
    Eq.push solver;
    ignore (applications (branch + 6));
    let inner = List.init (width - 1) ((+) 1)
      |> List.filter (fun literal -> literal mod 4 = branch) in
    List.iter (Eq.assert_literal solver) inner;
    remaining := if branch mod 2 = 0 then 3 else 127;
    (match Eq.check_lazy solver with
    | _ -> failwith "Signature registration interruption was missed"
    | exception Exit -> ());
    remaining := 0;
    Eq.pop solver;
    inspect outer;
    check (deferred () = expected_reason)
      "Interrupted registration changed a deferred explanation";
    Eq.push solver;
    List.iter (Eq.assert_literal solver) inner;
    inspect (inner @ outer);
    Eq.push solver;
    let all = List.init (width - 1) ((+) 1) in
    List.iter (Eq.assert_literal solver) all;
    inspect all;
    Eq.pop solver;
    inspect (inner @ outer);
    Eq.pop solver;
    inspect outer;
    check (deferred () = expected_reason)
      "Signature reuse changed a deferred explanation"
  done;
  Eq.pop solver;
  inspect [];
  Eq.reset solver;
  inspect [];
  print_endline "Resized signature rollback and reuse passed"

module Compact_distinct_equality = struct
  module T = Vox_smt_term
  module E = Vox_smt_equality

  let check condition message = if not condition then failwith message

  let fixture ?(poll = fun () -> ()) count =
    let terms = T.create () in
    let values = Array.init count (fun index ->
      T.apply terms (T.symbol terms
        {name = string_of_int index; arguments = [||]; result = T.Named 0;
         kind = T.Uninterpreted}) [||]) in
    terms, values, E.create ~terms ~poll ()

  let consistent solver = match E.check solver with
    | E.Consistent _ -> ()
    | E.Conflict _ -> failwith "unexpected distinct conflict"

  let conflict solver expected = match E.check solver with
    | E.Conflict reason -> check (reason = expected) "distinct explanation"
    | E.Consistent _ -> failwith "missing distinct conflict"

  let assert_all solver = List.iter (E.assert_literal solver)

  let scenarios () =
    for mixed = 0 to 2 do
      let terms, values, solver = fixture 6 in
      E.register_equality solver ~literal:1 values.(0) values.(3);
      E.register_equality solver ~literal:2 values.(1) values.(4);
      E.register_equality solver ~literal:3 values.(3) values.(4);
      let arguments = [|values.(0); values.(1); values.(2)|] in
      E.register_distinct solver ~literal:10 arguments;
      arguments.(1) <- values.(5);
      if mixed = 1 then E.register solver (T.integer terms T.Z.zero);
      assert_all solver [1; 2]; consistent solver;
      E.push solver;
      E.assert_literal solver 10; consistent solver;
      if mixed = 2 then E.register solver (T.integer terms T.Z.zero);
      E.push solver;
      E.assert_literal solver 3;
      conflict solver [|1; 2; 3; 10|];
      E.pop solver; consistent solver;
      E.pop solver; E.assert_literal solver 3; consistent solver;
      E.push solver; E.assert_literal solver 10;
      conflict solver [|1; 2; 3; 10|];
      E.pop solver; E.reset solver;
      assert_all solver [-10; 1; 2; 3]; consistent solver;
      E.reset solver;
      assert_all solver [10; 1; 2; 3];
      conflict solver [|1; 2; 3; 10|]
    done;
    let _, values, solver = fixture 4 in
    E.assert_literal solver (-10);
    E.register_distinct solver ~literal:(-10) [|values.(0); values.(1)|];
    E.register_distinct solver ~literal:(-10) [|values.(0); values.(1)|];
    E.register_distinct solver ~literal:(-10) [|values.(2); values.(3)|];
    E.register_equality solver ~literal:1 values.(2) values.(3);
    consistent solver;
    E.push solver; E.assert_literal solver 1;
    conflict solver [|-10; 1|];
    E.pop solver; E.reset solver;
    assert_all solver [10; 1]; consistent solver;
    E.reset solver;
    E.register_distinct solver ~literal:20 [|values.(0); values.(0)|];
    E.assert_literal solver 20; conflict solver [|20|];
    E.reset solver;
    E.register_distinct solver ~literal:30 [||];
    E.register_distinct solver ~literal:31 [|values.(0)|];
    assert_all solver [30; 31]; consistent solver;
    let terms, values, solver = fixture 2 in
    let symbol = T.symbol terms
      {name = "f"; arguments = [|T.Named 0|]; result = T.Named 0;
       kind = T.Uninterpreted} in
    let applications = Array.map (fun value ->
      T.apply terms symbol [|value|]) values in
    E.register_distinct solver ~literal:10 applications;
    E.register_equality solver ~literal:1 values.(0) values.(1);
    assert_all solver [10; 1]; conflict solver [|1; 10|];
    print_endline "distinct activation, rollback, mixed and congruence passed"

  type binding = Equality of int * int * int | Distinct of int * int array

  let reference count bindings assertions =
    let parents = Array.init count Fun.id in
    let rec root term =
      if parents.(term) = term then term else root parents.(term) in
    List.iter (function
      | Equality (literal, left, right) when List.mem literal assertions ->
        parents.(root left) <- root right
      | _ -> ()) bindings;
    let collision arguments =
      let seen = Hashtbl.create 16 in
      Array.exists (fun term ->
        let term = root term in
        if Hashtbl.mem seen term then true
        else begin Hashtbl.add seen term (); false end) arguments in
    let conflict = List.exists (function
      | Equality (literal, left, right) ->
        List.mem (-literal) assertions && root left = root right
      | Distinct (literal, arguments) ->
        List.mem literal assertions && collision arguments) bindings in
    let negative = List.fold_left (fun pairs -> function
      | Equality (literal, left, right) when List.mem (-literal) assertions ->
        (root left, root right) :: pairs
      | Distinct (literal, arguments) when List.mem literal assertions ->
        let pairs = ref pairs in
        for i = 0 to Array.length arguments - 1 do
          for j = i + 1 to Array.length arguments - 1 do
            pairs := (root arguments.(i), root arguments.(j)) :: !pairs
          done
        done;
        !pairs
      | _ -> pairs) [] bindings in
    let implied = List.fold_left (fun implied -> function
      | Distinct _ -> implied
      | Equality (literal, left, right) ->
        let left = root left and right = root right in
        let implied = if left = right then literal :: implied else implied in
        if List.exists (fun (a, b) ->
            a = left && b = right || a = right && b = left) negative
        then -literal :: implied else implied) [] bindings in
    (conflict || List.exists (fun literal ->
        List.mem (-literal) (implied @ assertions))
      (implied @ assertions)), Array.init count root

  let randomized () =
    let events = ref 0 and reasons = ref 0 in
    for seed = 0 to 63 do
      let random = Random.State.make [|106; seed|] in
      let _, values, solver = fixture 12 in
      let bindings = ref [] and assertions = ref [] and levels = ref [] in
      let fresh = ref 1 in
      let rec observe () =
        let expected, partition = reference 12 !bindings !assertions in
        match E.check_lazy solver with
        | E.Lazy_conflict reason ->
          check expected "spurious group conflict";
          Array.iter (fun literal ->
            check (List.mem literal !assertions) "unasserted group reason")
            reason;
          check (fst (reference 12 !bindings (Array.to_list reason)))
            "invalid group reason";
          incr reasons;
          E.reset solver; assertions := []; levels := []
        | E.Lazy_consistent propagated ->
          check (not expected) "missing group conflict";
          for left = 0 to 11 do
            for right = 0 to 11 do
              check ((E.representative solver values.(left)
                      = E.representative solver values.(right))
                     = (partition.(left) = partition.(right)))
                "wrong group partition"
            done
          done;
          List.iter (fun (literal, explain) ->
            let reason = explain () in
            Array.iter (fun antecedent ->
              check (List.mem antecedent !assertions) "unasserted reason")
              reason;
            check (fst (reference 12 !bindings
              (-literal :: Array.to_list reason))) "invalid implication";
            incr reasons;
            assertions := literal :: !assertions;
            E.assert_literal solver literal) propagated;
          if propagated <> [] then observe () in
      for step = 0 to 299 do
        incr events;
        (match Random.State.int random 10 with
        | 0 -> E.push solver; levels := !assertions :: !levels
        | 1 -> (match !levels with
          | previous :: rest ->
            E.pop solver; assertions := previous; levels := rest
          | [] -> ())
        | 2 when step mod 7 = 0 ->
          E.reset solver; assertions := []; levels := []
        | 2 | 3 | 4 ->
          let literal =
            if !fresh > 1 && Random.State.int random 4 = 0
            then 1 + Random.State.int random (!fresh - 1)
            else let literal = !fresh in incr fresh; literal in
          let literal = if Random.State.int random 5 = 0
            then -literal else literal in
          let binding = if Random.State.bool random then begin
            let left = Random.State.int random 12 in
            let right = Random.State.int random 12 in
            E.register_equality solver ~literal values.(left) values.(right);
            Equality (literal, left, right)
          end else begin
            let members = Array.init (Random.State.int random 7)
                (fun _ -> Random.State.int random 12) in
            E.register_distinct solver ~literal
              (Array.map (Array.get values) members);
            Distinct (literal, members)
          end in
          if not (List.mem binding !bindings) then
            bindings := binding :: !bindings
        | _ when !fresh > 1 ->
          let literal = 1 + Random.State.int random (!fresh - 1) in
          if not (List.exists (fun prior -> abs prior = literal) !assertions)
          then begin
            let literal = if Random.State.bool random
              then literal else -literal in
            assertions := literal :: !assertions;
            E.assert_literal solver literal
          end
        | _ -> ());
        observe ()
      done
    done;
    Printf.printf "%d group events; %d reason oracles passed\n%!"
      !events !reasons

  let interruption late =
    let setup poll =
      let _, values, solver = fixture ~poll 12 in
      E.register_equality solver ~literal:1 values.(0) values.(6);
      E.register_equality solver ~literal:2 values.(1) values.(7);
      E.register_equality solver ~literal:3 values.(6) values.(7);
      let register () =
        E.register_distinct solver ~literal:10 (Array.sub values 0 6);
        E.register_distinct solver ~literal:11 (Array.sub values 6 6) in
      if not late then register ();
      consistent solver; E.push solver;
      let run () =
        assert_all solver [10; 11];
        if late then register ();
        consistent solver;
        assert_all solver [1; 2]; consistent solver;
        E.assert_literal solver 3;
        (match E.check solver with
        | E.Conflict reason ->
          check (Array.mem 10 reason || Array.mem 11 reason)
            "missing activation antecedent"
        | E.Consistent _ -> failwith "missing interrupted group conflict") in
      let recover () =
        E.pop solver; consistent solver;
        E.push solver; run (); E.reset solver; consistent solver in
      run, recover in
    let armed = ref false and total = ref 0 in
    let run, recover = setup (fun () -> if !armed then incr total) in
    armed := true; run (); armed := false; recover ();
    for stop = 1 to !total do
      let armed = ref false and count = ref 0 in
      let run, recover = setup (fun () -> if !armed then begin
        incr count; if !count = stop then raise Exit
      end) in
      armed := true;
      (match run () with
      | exception Exit -> ()
      | () -> failwith "missing group interruption");
      armed := false; recover ()
    done;
    Printf.printf "%d group interruption positions recovered (late=%b)\n%!"
      !total late

  let () =
    scenarios ();
    randomized ();
    interruption false;
    interruption true
end

module Compact_distinct_propagation = struct
  module T = Vox_smt_term
  module E = Vox_smt_equality
  let check condition message = if not condition then failwith message
  let () =
    let terms = T.create () in
    let values = Array.init 8 (fun i -> T.apply terms
      (T.symbol terms {name = string_of_int i; arguments = [||];
        result = T.Named 0; kind = T.Uninterpreted}) [||]) in
    let solver = E.create ~terms () in
    E.register_equality solver ~literal:1 values.(2) values.(3);
    E.register_equality solver ~literal:2 values.(2) values.(4);
    E.register_equality solver ~literal:3 values.(1) values.(2);
    E.register_equality solver ~literal:20 values.(0) values.(4);
    E.register_distinct solver ~literal:10 [|values.(0); values.(1)|];
    List.iter (E.assert_literal solver) [1; 2; 10];
    (match E.check solver with
    | E.Consistent outputs -> check (not (List.mem_assoc (-20) outputs))
        "premature group propagation"
    | E.Conflict _ -> failwith "initial group conflict");
    E.push solver;
    E.assert_literal solver 3;
    let delayed = match E.check_lazy solver with
      | E.Lazy_consistent outputs ->
        (match List.assoc_opt (-20) outputs with
        | Some explain -> explain
        | None -> failwith "missing transferred group propagation")
      | E.Lazy_conflict _ -> failwith "group transfer conflict" in
    check (delayed () = [|2; 3; 10|]) "group propagation explanation";
    E.push solver;
    E.register_equality solver ~literal:4 values.(5) values.(6);
    E.assert_literal solver 4;
    ignore (E.check solver);
    E.pop solver;
    check (delayed () = [|2; 3; 10|]) "delayed group explanation";
    E.pop solver;
    E.assert_literal solver 20;
    (match E.check solver with
    | E.Consistent _ -> ()
    | E.Conflict _ -> failwith "popped group propagation remained");
    E.reset solver;
    E.register_equality solver ~literal:21 values.(0) values.(1);
    E.assert_literal solver 10;
    (match E.check solver with
    | E.Consistent outputs ->
      check (List.assoc_opt (-21) outputs = Some [|10|])
        "activation group propagation"
    | E.Conflict _ -> failwith "activation conflict");
    print_endline "Group propagation, transfer and lazy explanations passed"
end

let () =
  signature_rollback ();
  disequality_notifications ();
  constructor_members ();
  disequality_cancellation ();
  disequality_propagation ();
  disequality_propagation ~mixed:true ();
  commutative_cancellation ();
  commutative_deferred ();
  filtered_congruence ();
  nested_theory_explanations ();
  deferred_cancellation ();
  deferred_reroot ();
  late_literal_growth ();
  snapshots ();
  randomized ();
  randomized ~commutative_enabled:true ();
  randomized ~pure_uf:true ();
  randomized ~commutative_enabled:true ~pure_uf:true ();
  dynamics ();
  long_explanation ();
  cancellation ();
  sat_integration ();
  print_endline "Equality tests passed"
