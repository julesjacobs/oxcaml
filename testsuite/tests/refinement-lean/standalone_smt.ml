(* TEST
 include ocamlcommon;
*)

open Types

module R = Types.Refinement

let next_type_id = ref 30_000

let fresh_type_id () =
  incr next_type_id;
  !next_type_id

let arrow argument result =
  create_expr
    (Tarrow
       ( (Nolabel, Mode.Alloc.legacy, Mode.Alloc.legacy),
         argument,
         result,
         commu_ok ))
    ~level:0
    ~scope:0
    ~id:(fresh_type_id ())

let tuple_type fields =
  create_expr
    (Ttuple fields)
    ~level:0
    ~scope:0
    ~id:(fresh_type_id ())

let int_type = Predef.type_int
let bool_type = Predef.type_bool
let option_type = Predef.type_option int_type
let pair_type = tuple_type [None, int_type; None, bool_type]
let loc = Location.in_file "standalone_smt.ml"

let env =
  Compmisc.init_path ();
  Compmisc.initial_env ()

let node type_ rexp_desc = R.create ~loc ~type_ rexp_desc
let int value = node int_type (Rexp_constant (Const_int value))
let bound binder = node binder.rb_type (Rexp_ident (Rbound binder.rb_id))
let free type_ name = node type_ (Rexp_ident (Rfree (Rfun name)))

let bool value =
  node bool_type
    (Rexp_construct
       ( { rconstr_type_path = Predef.path_bool;
           rconstr_name = if value then "true" else "false";
         },
         [] ))

let some value =
  node option_type
    (Rexp_construct
       ( { rconstr_type_path = Predef.path_option;
           rconstr_name = "Some";
         },
         [value] ))

let primitive type_ name =
  let stdlib = Path.Pident (Ident.create_persistent "Stdlib") in
  let path = Path.Pdot (stdlib, name) in
  node type_ (Rexp_ident (Rfree (Rapp path)))

let apply type_ function_ arguments =
  node type_
    (Rexp_apply
       (function_, List.map (fun argument -> Nolabel, argument) arguments))

let binary name argument_type result_type left right =
  let function_type = arrow argument_type (arrow argument_type result_type) in
  apply result_type (primitive function_type name) [left; right]

let equal type_ left right = binary "=" type_ bool_type left right
let add left right = binary "+" int_type int_type left right
let subtract left right = binary "-" int_type int_type left right
let multiply left right = binary "*" int_type int_type left right
let greater left right = binary ">" int_type bool_type left right
let less_equal left right = binary "<=" int_type bool_type left right

let conjunction left right =
  binary "&&" bool_type bool_type left right

let disjunction left right =
  binary "||" bool_type bool_type left right

let negate argument =
  apply bool_type (primitive (arrow bool_type bool_type) "not") [argument]

let test_origin =
  Vox_vc.{ kind = "test"; name = Some "standalone_smt"; span = Some loc }

let fact expression =
  Vox_vc.
    { expression; location = Some loc; scope = None; origin = test_origin }

let vc ?(facts = []) goal = Vox_vc.create ~loc ~facts ~goal

let x =
  { rb_id = Ident.create_scoped ~scope:1 "x";
    rb_type = int_type;
  }

let arithmetic_and_booleans =
  let x_value = equal int_type (bound x) (int 4) in
  let arithmetic =
    less_equal
      (subtract (multiply (add (bound x) (int 2)) (int 3)) (int 1))
      (int 20)
  in
  let booleans =
    disjunction (negate (bool false))
      (conjunction (bool true) (greater (bound x) (int (-1))))
  in
  vc ~facts:[fact x_value] (conjunction arithmetic booleans)

let unused_fact_usage =
  let x_value = equal int_type (bound x) (int 4) in
  vc ~facts:[fact x_value; fact (bool true)]
    (greater (bound x) (int 0))

let used_everywhere =
  let lower = less_equal (int 0) (bound x) in
  let upper = less_equal (bound x) (int 0) in
  vc ~facts:[fact lower; fact upper]
    (equal int_type (bound x) (int 0))

let open_goal = vc (free bool_type "open_predicate")

let ground_defeq_shape =
  let function_type = arrow int_type int_type in
  let function_ = free function_type "successor_def" in
  let call = apply int_type function_ [int 4] in
  vc ~facts:[fact (equal int_type call (int 5))]
    (greater call (int 0))

let bound_function_symbol =
  let function_ =
    { rb_id = Ident.create_scoped ~scope:2 "bound_successor";
      rb_type = arrow int_type int_type;
    }
  in
  let call = apply int_type (bound function_) [int 9] in
  vc ~facts:[fact (equal int_type call (int 10))]
    (greater call (int 0))

let tuple =
  let value = node pair_type (Rexp_tuple [None, int 7; None, bool true]) in
  vc (equal pair_type value value)

let datatype =
  let value = some (int 7) in
  vc (equal option_type value value)

let reduction =
  let local =
    { rb_id = Ident.create_scoped ~scope:2 "local";
      rb_type = int_type;
    }
  in
  let local_equality = equal int_type (bound local) (int 3) in
  let let_expression =
    node bool_type
      (Rexp_let
         ( [{ rbind_binder = local; rbind_expr = int 3 }],
           local_equality ))
  in
  let conditional =
    node bool_type
      (Rexp_ifthenelse (bool true, let_expression, Some (bool false)))
  in
  let parameter =
    { rb_id = Ident.create_scoped ~scope:3 "parameter";
      rb_type = int_type;
    }
  in
  let lambda =
    node (arrow int_type bool_type)
      (Rexp_function
         { arg_label = Nolabel;
           param = parameter;
           body = equal int_type (bound parameter) (int 4);
         })
  in
  let beta_redex = apply bool_type lambda [int 4] in
  vc (conjunction conditional beta_redex)

let contains text pattern =
  let text_length = String.length text in
  let pattern_length = String.length pattern in
  let rec loop index =
    if index + pattern_length > text_length then false
    else if String.sub text index pattern_length = pattern then true
    else loop (index + 1)
  in
  pattern_length = 0 || loop 0

let emit_query query condition =
  match Vox_smt.emit ~query ~env condition with
  | Ok text -> text
  | Error error -> failwith error.message

let emit condition = emit_query Vox_smt.Prove condition

let cases =
  [ "arithmetic-and-booleans", arithmetic_and_booleans;
    "ground-defeq-shape", ground_defeq_shape;
    "bound-function-symbol", bound_function_symbol;
    "tuple", tuple;
    "datatype", datatype;
    "let-ite-lambda-beta", reduction;
  ]

let () =
  List.iter
    (fun (name, condition) ->
      let first = emit condition in
      let second = emit condition in
      assert (String.equal first second);
      Printf.printf "=== %s ===\n%s" name first;
      if not (String.ends_with ~suffix:"\n" first) then print_newline ())
    cases

let () =
  let emitted =
    String.concat "\n" (List.map (fun (_, condition) -> emit condition) cases)
    |> String.lowercase_ascii
  in
  List.iter
    (fun forbidden ->
      if contains emitted forbidden then
        failwith ("SMT emission contains forbidden token: " ^ forbidden))
    [ "forall"; "exists"; "pattern"; "quantifier" ];
  print_endline "quantifier guard: no forall/exists/pattern/quantifier"

let () =
  let ignored =
    { rb_id = Ident.create_scoped ~scope:4 "ignored";
      rb_type = bool_type;
    }
  in
  let ignoring_lambda =
    node (arrow bool_type bool_type)
      (Rexp_function
         { arg_label = Nolabel;
           param = ignored;
           body = bool true;
         })
  in
  let erased_quantifier =
    vc (apply bool_type ignoring_lambda [free bool_type "forall_"])
  in
  begin
    match Vox_smt.emit ~query:Vox_smt.Prove ~env erased_quantifier with
    | Error error ->
      assert (contains error.message "quantifier combinator forall_")
    | Ok _ -> failwith "beta reduction erased a forbidden quantifier"
  end;
  let parameter =
    { rb_id = Ident.create_scoped ~scope:5 "parameter";
      rb_type = int_type;
    }
  in
  let function_type = arrow int_type bool_type in
  let residual_lambda =
    node function_type
      (Rexp_function
         { arg_label = Nolabel;
           param = parameter;
           body = bool true;
         })
  in
  begin
    match
      Vox_smt.emit ~query:Vox_smt.Prove ~env
        (vc (equal function_type residual_lambda residual_lambda))
    with
    | Error error -> assert (contains error.message "lambda remains")
    | Ok _ -> failwith "residual higher-order lambda was emitted"
  end;
  print_endline "rejection guards: erased quantifier and residual lambda"

let () =
  let disprove = emit_query Vox_smt.Disprove (vc (bool false)) in
  assert (contains disprove "(assert (= false true))\n(check-sat)\n");
  print_endline "disprove query: positive goal asserted"

let () =
  let check expected text = assert (Vox_smt.parse_status text = expected) in
  check (Some Vox_smt.Sat) "sat\n";
  check (Some Vox_smt.Unsat) "  unsat\r\n";
  check (Some Vox_smt.Unknown) "unknown\n";
  check None
    "(result (verdict unsat) (counters (conflicts 0)))\n";
  check None "diagnostic: expected (verdict unsat) but got garbage\n";
  check None "success\n";
  check None "(error \"invalid input\")\n";
  check None "sat\nunsat\n";
  print_endline "status parser: sat/unsat/unknown/malformed checked"

let shell_command script = "/bin/sh -c " ^ Filename.quote script

let fixed_status status =
  shell_command ("printf '" ^ status ^ "\\n'")

let fixed_unsat_core names =
  fixed_status ("unsat\\n(" ^ String.concat " " names ^ ")")

let discharge ?(backend = `Z3) command condition =
  Vox_smt.discharge ~backend ~command:(Some command) ~env condition

let () =
  let missing =
    discharge "/definitely/missing/vox-z3" arithmetic_and_booleans
  in
  assert (missing.verdict = Vox_smt.Unavailable);
  let unknown = discharge (fixed_status "unknown") arithmetic_and_booleans in
  assert (unknown.verdict = Vox_smt.Not_proved);
  assert (unknown.unused_facts = []);
  assert
    (unknown.detail
     = Some "prove query: unknown; disprove query: unknown");
  let proved =
    discharge (fixed_unsat_core ["h_0"]) arithmetic_and_booleans
  in
  assert (proved.verdict = Vox_smt.Proved);
  assert (proved.unused_facts = []);
  let open_ = discharge (fixed_status "sat") arithmetic_and_booleans in
  assert (open_.verdict = Vox_smt.Not_proved);
  let discriminate =
    shell_command
      "if grep -Fq '(assert (= false true))'; then printf 'unsat\\n'; \
       else printf 'sat\\n'; fi"
  in
  let disproved = discharge discriminate (vc (bool false)) in
  assert (disproved.verdict = Vox_smt.Disproved);
  let solver_error =
    discharge (shell_command "printf '(error \"bad input\")\\n'")
      arithmetic_and_booleans
  in
  assert (solver_error.verdict = Vox_smt.Solver_error);
  let false_status =
    discharge
      (shell_command
         "printf 'diagnostic: expected (verdict unsat) but got garbage\\n'")
      arithmetic_and_booleans
  in
  assert (false_status.verdict = Vox_smt.Solver_error);
  let unused =
    discharge (fixed_unsat_core []) arithmetic_and_booleans
  in
  assert (unused.verdict = Vox_smt.Proved);
  assert (unused.unused_facts = [0]);
  let malformed_core =
    discharge (fixed_status "unsat\\n(not_a_fact)")
      arithmetic_and_booleans
  in
  assert (malformed_core.verdict = Vox_smt.Solver_error);
  let unavailable_core_exit =
    shell_command
      "if grep -Fq '(get-unsat-core)'; then \
       printf 'sat\\n(error \"line 1: unsat core is not available\")\\n'; \
       exit 1; else printf 'sat\\n'; fi"
  in
  let not_proved = discharge unavailable_core_exit open_goal in
  assert (not_proved.verdict = Vox_smt.Not_proved);
  print_endline
    "verdicts: proved/disproved/unknown/error/unavailable distinguished";
  let unsupported =
    discharge ~backend:`Oxsmt
      (shell_command
         "printf 'unsupported SMT-LIB2: nonlinear multiplication\\n' >&2; \
          exit 3")
      arithmetic_and_booleans
  in
  assert (unsupported.verdict = Vox_smt.Not_proved);
  assert
    (unsupported.detail
     = Some "prove query: unknown; disprove query: unknown");
  let malformed =
    discharge ~backend:`Oxsmt
      (shell_command
         "printf 'malformed SMT-LIB2: unbalanced input\\n' >&2; exit 2")
      arithmetic_and_booleans
  in
  assert (malformed.verdict = Vox_smt.Solver_error);
  let unexpected =
    discharge
      (shell_command "printf 'unexpected z3 failure\\n' >&2; exit 3")
      arithmetic_and_booleans
  in
  assert (unexpected.verdict = Vox_smt.Solver_error);
  print_endline
    "oxsmt exits: unsupported is unknown; malformed is solver-error";
  assert
    (Vox_backend.selection_of_string "cross" = Ok Vox_backend.Cross);
  assert
    ((Vox_backend.capabilities Vox_backend.Lean).fact_usage
     = Vox_backend.Fact_usage);
  assert
    ((Vox_backend.capabilities Vox_backend.Z3).fact_usage
     = Vox_backend.Fact_usage);
  assert
    ((Vox_backend.capabilities Vox_backend.Oxsmt).fact_usage
     = Vox_backend.Fact_usage);
  print_endline "backend selection and usage capabilities checked";
  if Vox_lean.lean_available () then begin
    let cross =
      Vox_backend.discharge ~selection:Vox_backend.Cross
        ~smt_solver:(Some (fixed_status "unknown"))
        ~oxsmt_solver:(Some (fixed_unsat_core [])) ~env
        arithmetic_and_booleans
    in
    assert (cross.verdict = Vox_backend.Solver_error);
    assert
      (cross.detail
       = Some
           "cross-check failed: lean=proved, z3=unknown, oxsmt=proved");
    begin
      match cross.backend_results with
      | [lean; z3; oxsmt] ->
        assert (lean.backend = Vox_backend.Lean);
        assert (lean.verdict = Vox_backend.Proved);
        assert (Option.is_some lean.unused_facts);
        assert (z3.backend = Vox_backend.Z3);
        assert (z3.verdict = Vox_backend.Unknown);
        assert (z3.unused_facts = None);
        assert (oxsmt.backend = Vox_backend.Oxsmt);
        assert (oxsmt.verdict = Vox_backend.Proved);
        assert (oxsmt.unused_facts = Some [0])
      | _ -> failwith "cross backend result order changed"
    end;
    let all_proved =
      Vox_backend.discharge ~selection:Vox_backend.Cross
        ~smt_solver:(Some (fixed_unsat_core ["h_0"]))
        ~oxsmt_solver:(Some (fixed_unsat_core ["h_0"])) ~env
        arithmetic_and_booleans
    in
    assert (all_proved.verdict = Vox_backend.Proved);
    print_endline "cross aggregation: ordered divergence and unanimity checked"
  end else
    print_endline "cross aggregation: SKIPPED (Lean is unavailable)";
  match Sys.getenv_opt "VOX_SMT_TEST_SOLVER" with
  | None ->
    print_endline
      "z3 subprocess: SKIPPED (VOX_SMT_TEST_SOLVER is not provisioned)"
  | Some command ->
    let result =
      Vox_smt.discharge ~backend:`Z3 ~command:(Some command)
        ~env unused_fact_usage
    in
    begin
      match result.verdict with
      | Vox_smt.Unavailable ->
        failwith "VOX_SMT_TEST_SOLVER was configured but is unavailable"
      | Vox_smt.Proved -> assert (result.unused_facts = [1])
      | _ -> failwith "z3 did not prove the unused-fact test"
    end;
    let all_used =
      Vox_smt.discharge ~backend:`Z3 ~command:(Some command)
        ~env used_everywhere
    in
    begin
      match all_used.verdict with
      | Vox_smt.Proved -> assert (all_used.unused_facts = [])
      | _ -> failwith "z3 did not prove the all-facts-used test"
    end;
    print_endline "z3 subprocess: usage cores checked"

let () =
  let binary = "/j/office/app/z3/prod/4.8.5/install/bin/z3" in
  if Sys.file_exists binary then begin
    let command = binary ^ " -in" in
    let discharge condition =
      Vox_smt.discharge ~backend:`Z3 ~command:(Some command) ~env condition
    in
    let disproved = discharge (vc (bool false)) in
    assert (disproved.verdict = Vox_smt.Disproved);
    let not_proved = discharge open_goal in
    assert (not_proved.verdict = Vox_smt.Not_proved)
  end
