(* Consumer proof for {!Oxsmt_interface.Optimize}.

   Every satisfiable case has an independent exhaustive Boolean oracle. The test compares
   the reported optimum with that oracle and recomputes the achieved cost from the
   returned user-variable model. The weighted cases force multiple core refinements and
   distinguish exact weighted relaxation from cardinality-only or whole-core relaxation. *)

open Oxsmt_core
module Session = Oxsmt_interface.Session
module Optimize = Oxsmt_interface.Optimize

let checks = ref 0
let failures = ref 0

let fail name message =
  incr failures;
  Printf.printf "  FAIL %s: %s\n" name message
;;

let check name condition =
  incr checks;
  if not condition then fail name "expected true"
;;

type soft_spec =
  { var : int
  ; wants : bool
  ; weight : int
  }

let assignment_of_mask count mask = Array.init count (fun i -> mask land (1 lsl i) <> 0)

let cost_of_assignment assignment softs =
  List.fold_left
    (fun cost soft ->
      if Bool.equal assignment.(soft.var) soft.wants then cost else cost + soft.weight)
    0
    softs
;;

let brute_force variable_count hard softs =
  let best = ref None in
  for mask = 0 to (1 lsl variable_count) - 1 do
    let assignment = assignment_of_mask variable_count mask in
    if hard assignment
    then (
      let cost = cost_of_assignment assignment softs in
      match !best with
      | None -> best := Some cost
      | Some prior when cost < prior -> best := Some cost
      | Some _ -> ())
  done;
  !best
;;

let bool_vars session names =
  Array.map
    (fun name ->
      Context.const
        (Session.context session)
        (Session.declare_const session name Sort.bool))
    names
;;

let model_assignment names (_sorts, bindings) =
  let wanted = Hashtbl.create (Array.length names) in
  Array.iteri (fun index name -> Hashtbl.add wanted name index) names;
  let values = Array.make (Array.length names) None in
  let valid = ref true in
  List.iter
    (fun binding ->
      match binding with
      | Session.Const (name, Session.VBool value) ->
        (match Hashtbl.find_opt wanted name with
         | None -> ()
         | Some index ->
           (match values.(index) with
            | None -> values.(index) <- Some value
            | Some _ -> valid := false))
      | Session.Const (name, _) | Session.Fun (name, _) ->
        if Hashtbl.mem wanted name then valid := false)
    bindings;
  if !valid && Array.for_all Option.is_some values
  then Some (Array.map Option.get values)
  else None
;;

let has_prefix string prefix =
  String.length string >= String.length prefix
  && String.sub string 0 (String.length prefix) = prefix
;;

let selector_bindings_absent (_sorts, bindings) =
  List.for_all
    (fun binding ->
      let name =
        match binding with
        | Session.Const (name, _) | Session.Fun (name, _) -> name
      in
      not (has_prefix name "@oxsmt.optimize.selector."))
    bindings
;;

let run_problem name ~names ~hard ~build_hard ~softs =
  let expected = brute_force (Array.length names) hard softs in
  let session = Session.create () in
  let variables = bool_vars session names in
  List.iter (Session.assert_term session) (build_hard session variables);
  let optimize_softs =
    List.map
      (fun soft ->
        let term =
          if soft.wants
          then variables.(soft.var)
          else Context.not_ (Session.context session) variables.(soft.var)
        in
        { Optimize.term; weight = Bigint.of_int soft.weight })
      softs
  in
  match expected, Optimize.max_smt session optimize_softs with
  | None, Optimize.Hard_unsat -> check (name ^ ": hard unsat") true
  | None, Optimize.Optimal _ -> fail name "reported an optimum for unsatisfiable hard constraints"
  | None, Optimize.Unknown -> fail name "returned unknown for a small Boolean hard refutation"
  | Some _, Optimize.Hard_unsat -> fail name "reported satisfiable hard constraints as unsat"
  | Some _, Optimize.Unknown -> fail name "returned unknown for a small Boolean problem"
  | Some expected, Optimize.Optimal optimum ->
    check
      (name ^ ": oracle cost")
      (Bigint.equal optimum.cost (Bigint.of_int expected));
    check (name ^ ": selectors stripped") (selector_bindings_absent optimum.model);
    (match model_assignment names optimum.model with
     | None -> fail name "returned model does not bind every user Boolean"
     | Some assignment ->
       check (name ^ ": model satisfies hard constraints") (hard assignment);
       let achieved = cost_of_assignment assignment softs in
       check (name ^ ": model achieves reported cost") (achieved = expected);
       check
         (name ^ ": violated occurrence count")
         (List.length optimum.violated
          = List.fold_left
              (fun count soft ->
                if Bool.equal assignment.(soft.var) soft.wants then count else count + 1)
              0
              softs))
;;

let forbid_pair session variables i j =
  Context.not_
    (Session.context session)
    (Context.and_ (Session.context session) [ variables.(i); variables.(j) ])
;;

let () =
  run_problem
    "all-soft-satisfiable"
    ~names:[| "all_a"; "all_b" |]
    ~hard:(fun _ -> true)
    ~build_hard:(fun _ _ -> [])
    ~softs:
      [ { var = 0; wants = true; weight = 2 }
      ; { var = 1; wants = false; weight = 3 }
      ];

  (* All feasible maxima satisfy one soft and violate two: cardinality ties, weights do
     not. Whole-core relaxation or stopping after an early core returns cost 14/15 rather
     than the oracle's 11. *)
  run_problem
    "weighted-at-most-one"
    ~names:[| "amo_a"; "amo_b"; "amo_c" |]
    ~hard:(fun a -> not (a.(0) && a.(1)) && not (a.(0) && a.(2)) && not (a.(1) && a.(2)))
    ~build_hard:(fun session variables ->
      [ forbid_pair session variables 0 1
      ; forbid_pair session variables 0 2
      ; forbid_pair session variables 1 2
      ])
    ~softs:
      [ { var = 0; wants = true; weight = 9 }
      ; { var = 1; wants = true; weight = 6 }
      ; { var = 2; wants = true; weight = 5 }
      ];

  (* The two cores overlap at the expensive middle literal. Greedily dropping the
     cheapest member of each core costs 8; the exact hitting set drops only b for 5. *)
  run_problem
    "overlapping-weighted-cores"
    ~names:[| "overlap_a"; "overlap_b"; "overlap_c" |]
    ~hard:(fun a -> not (a.(0) && a.(1)) && not (a.(1) && a.(2)))
    ~build_hard:(fun session variables ->
      [ forbid_pair session variables 0 1; forbid_pair session variables 1 2 ])
    ~softs:
      [ { var = 0; wants = true; weight = 4 }
      ; { var = 1; wants = true; weight = 5 }
      ; { var = 2; wants = true; weight = 4 }
      ];

  (* Equal soft terms remain separate paid occurrences. Reusing one selector without
     aggregating both weights would incorrectly report 2 or 3 rather than 5. *)
  run_problem
    "duplicate-soft-occurrences"
    ~names:[| "dup_a" |]
    ~hard:(fun a -> not a.(0))
    ~build_hard:(fun session variables ->
      [ Context.not_ (Session.context session) variables.(0) ])
    ~softs:
      [ { var = 0; wants = true; weight = 2 }
      ; { var = 0; wants = true; weight = 3 }
      ];

  run_problem
    "hard-unsat"
    ~names:[| "hard_p" |]
    ~hard:(fun _ -> false)
    ~build_hard:(fun session variables ->
      [ variables.(0); Context.not_ (Session.context session) variables.(0) ])
    ~softs:[ { var = 0; wants = true; weight = 7 } ]
;;

let () =
  let session = Session.create () in
  let term = (bool_vars session [| "budget_a" |]).(0) in
  check
    "optimizer query budget fails closed"
    (match
       Optimize.max_smt
         ~max_checks:0
         session
         [ { Optimize.term; weight = Bigint.one } ]
     with
     | Optimize.Unknown -> true
     | Optimize.Optimal _ | Optimize.Hard_unsat -> false);
  check
    "zero weight rejected"
    (match Optimize.max_smt session [ { Optimize.term; weight = Bigint.zero } ] with
     | exception Invalid_argument _ -> true
     | Optimize.Optimal _ | Optimize.Hard_unsat | Optimize.Unknown -> false)
;;

let () =
  let session = Session.create ~max_effort:0 () in
  let variables = bool_vars session [| "unknown_a"; "unknown_b" |] in
  Session.assert_term
    session
    (Context.or_ (Session.context session) [ variables.(0); variables.(1) ]);
  check
    "underlying unknown fails closed"
    (match Optimize.max_smt session [] with
     | Optimize.Unknown -> true
     | Optimize.Optimal _ | Optimize.Hard_unsat -> false)
;;

let () =
  Printf.printf "optimize_test: %d checks, %d failures\n" !checks !failures;
  if !failures <> 0 then exit 1
;;
