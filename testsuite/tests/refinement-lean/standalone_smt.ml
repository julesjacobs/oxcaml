(* TEST
 include unix;
 include ocamlcommon;
*)

open Types

let helper_starts_with prefix text =
  let length = String.length prefix in
  String.length text >= length
  && String.equal (String.sub text 0 length) prefix

let helper_touch filename =
  let channel = open_out filename in
  close_out channel

let run_persistent_helper mode counter state =
  let counter_channel =
    open_out_gen [Open_wronly; Open_creat; Open_append; Open_text] 0o600
      counter
  in
  output_string counter_channel "start\n";
  close_out counter_channel;
  if String.equal mode "startup_error" then begin
    print_endline "(error \"startup diagnostic\")";
    flush stdout
  end;
  let declarations = ref [] in
  let global_datatypes = ref [] in
  let core_enabled = ref false in
  let declaration_frames = ref [] in
  let checks = ref 0 in
  let echo_prefix = "(echo \"" in
  let rec loop () =
    match input_line stdin with
    | line when String.equal (String.trim line) "(push 1)" ->
      declaration_frames := !declarations :: !declaration_frames;
      loop ()
    | line when String.equal (String.trim line) "(pop 1)" ->
      begin
        match !declaration_frames with
        | saved :: rest ->
          declarations := saved;
          declaration_frames := rest
        | [] -> print_endline "(error \"unbalanced pop\")"
      end;
      loop ()
    | line
      when String.equal (String.trim line)
             "(set-option :produce-unsat-cores true)" ->
      core_enabled := true;
      loop ()
    | line
      when String.equal (String.trim line)
             "(set-option :produce-unsat-cores false)" ->
      core_enabled := false;
      loop ()
    | line when helper_starts_with echo_prefix (String.trim line) ->
      let line = String.trim line in
      let marker =
        String.sub line (String.length echo_prefix)
          (String.length line - String.length echo_prefix - 2)
      in
      print_endline marker;
      flush stdout;
      if
        String.equal mode "early_exit_once"
        && String.equal marker "__vox2_z3_ready__"
        && not (Sys.file_exists state)
      then begin
        helper_touch state;
        exit 9
      end;
      loop ()
    | line when helper_starts_with "(declare-datatypes" line ->
      if List.mem line !global_datatypes
      then print_endline "(error \"duplicate global datatype\")"
      else global_datatypes := line :: !global_datatypes;
      flush stdout;
      loop ()
    | line when helper_starts_with "(declare-" line ->
      if List.mem line !declarations
      then print_endline "(error \"duplicate declaration\")"
      else declarations := line :: !declarations;
      flush stdout;
      loop ()
    | line when String.equal (String.trim line) "(check-sat)" ->
      if String.equal mode "delay_once" && not (Sys.file_exists state)
      then begin
        helper_touch state;
        Unix.sleep 5
      end;
      if String.equal mode "option_check" && !checks = 0
      then print_endline "sat"
      else if String.equal mode "option_check" && not !core_enabled
      then print_endline "(error \"unsat cores unavailable\")"
      else print_endline "unsat";
      incr checks;
      flush stdout;
      loop ()
    | line when String.equal (String.trim line) "(get-unsat-core)" ->
      if !core_enabled then print_endline "()"
      else print_endline "(error \"unsat core is not available\")";
      flush stdout;
      loop ()
    | _ -> loop ()
    | exception End_of_file -> ()
  in
  loop ()

let () =
  match
    Sys.getenv_opt "VOX_Z3_HELPER_MODE",
    Sys.getenv_opt "VOX_Z3_HELPER_COUNT",
    Sys.getenv_opt "VOX_Z3_HELPER_STATE"
  with
  | Some mode, Some counter, Some state ->
    run_persistent_helper mode counter state;
    exit 0
  | (Some _ | None), (Some _ | None), (Some _ | None) -> ()

module R = Types.Refinement

let next_type_id = ref 30_000

let fresh_type_id () =
  incr next_type_id;
  !next_type_id

let arrow argument result =
  create_expr
    (Tarrow
       ( (Nolabel, Mode.Alloc.legacy, Mode.Alloc.legacy, None),
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
let list_type = Predef.type_list int_type
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

let nil =
  node list_type
    (Rexp_construct
       ({ rconstr_type_path = Predef.path_list; rconstr_name = "[]" }, []))

let cons head tail =
  node list_type
    (Rexp_construct
       ( { rconstr_type_path = Predef.path_list; rconstr_name = "::" },
         [head; tail] ))

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

let recursive_datatype =
  let value = cons (int 7) nil in
  vc (equal list_type value value)

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
    "recursive-datatype", recursive_datatype;
    "let-ite-lambda-beta", reduction;
  ]

let () =
  List.iter
    (fun (_, condition) ->
      let result = Vox_smt.discharge_oxsmt ~env condition in
      assert (result.verdict = Vox_smt.Proved))
    cases;
  let disproved = Vox_smt.discharge_oxsmt ~env (vc (bool false)) in
  assert (disproved.verdict = Vox_smt.Disproved);
  let unused = Vox_smt.discharge_oxsmt ~env unused_fact_usage in
  assert (unused.verdict = Vox_smt.Proved);
  assert (unused.unused_facts = [1]);
  let all_used = Vox_smt.discharge_oxsmt ~env used_everywhere in
  assert (all_used.verdict = Vox_smt.Proved);
  assert (all_used.unused_facts = []);
  let through_backend condition =
    Vox_backend.discharge
      ~selection:(Vox_backend.Single Vox_backend.Oxsmt)
      ~smt_solver:None
      ~oxsmt_solver:(Some "/definitely/missing/oxsmt-command")
      ~env condition
  in
  let unused = through_backend unused_fact_usage in
  assert (unused.verdict = Vox_backend.Proved);
  assert (unused.unused_facts = Some [1]);
  let all_used = through_backend used_everywhere in
  assert (all_used.verdict = Vox_backend.Proved);
  assert (all_used.unused_facts = Some []);
  let nonlinear =
    let square = multiply (bound x) (bound x) in
    vc (equal int_type square square)
  in
  let unknown = Vox_smt.discharge_oxsmt ~env nonlinear in
  assert (unknown.verdict = Vox_smt.Not_proved);
  let original_mask =
    Unix.sigprocmask Unix.SIG_BLOCK [Sys.sigalrm]
  in
  Unix.putenv "VOX_OXSMT_TEST_SPIN" "1";
  let started = Unix.gettimeofday () in
  let blocked_timeout, remained_blocked =
    Fun.protect
      ~finally:(fun () ->
        Unix.putenv "VOX_OXSMT_TEST_SPIN" "0";
        ignore (Unix.sigprocmask Unix.SIG_SETMASK original_mask))
      (fun () ->
        let result =
          Vox_smt.discharge_oxsmt ~timeout_seconds:1 ~env
            arithmetic_and_booleans
        in
        let current_mask =
          Unix.sigprocmask Unix.SIG_BLOCK []
        in
        result, List.mem Sys.sigalrm current_mask)
  in
  let elapsed = Unix.gettimeofday () -. started in
  assert (blocked_timeout.verdict = Vox_smt.Solver_error);
  assert remained_blocked;
  assert (elapsed >= 0.75 && elapsed <= 2.5);
  let bad_timeout =
    Vox_smt.discharge_oxsmt ~timeout_seconds:0 ~env arithmetic_and_booleans
  in
  assert (bad_timeout.verdict = Vox_smt.Solver_error);
  Unix.putenv "VOX_OXSMT_TEST_RAISE" "1";
  let internal_failure =
    Fun.protect
      ~finally:(fun () -> Unix.putenv "VOX_OXSMT_TEST_RAISE" "0")
      (fun () -> Vox_smt.discharge_oxsmt ~env arithmetic_and_booleans)
  in
  assert (internal_failure.verdict = Vox_smt.Solver_error);
  let with_injected_core core function_ =
    Unix.putenv "VOX_OXSMT_TEST_UNSAT_CORE" core;
    Fun.protect
      ~finally:(fun () -> Unix.putenv "VOX_OXSMT_TEST_UNSAT_CORE" "none")
      function_
  in
  let empty_core =
    with_injected_core "empty" (fun () ->
      Vox_smt.discharge_oxsmt ~env unused_fact_usage)
  in
  assert (empty_core.verdict = Vox_smt.Solver_error);
  let non_covering_core =
    with_injected_core "non-covering" (fun () ->
      Vox_smt.discharge_oxsmt ~env unused_fact_usage)
  in
  assert (non_covering_core.verdict = Vox_smt.Solver_error);
  Oxsmt_interface.Session.inject_replay_verdict_for_test
    (Some Oxsmt_interface.Session.Unknown);
  let missing_core = Vox_smt.discharge_oxsmt ~env unused_fact_usage in
  assert (missing_core.verdict = Vox_smt.Proved);
  assert (missing_core.unused_facts = [])

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

let helper_line_count filename =
  let channel = open_in filename in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      let rec loop count =
        match input_line channel with
        | _ -> loop (count + 1)
        | exception End_of_file -> count
      in
      loop 0)

let helper_command mode counter state =
  String.concat " "
    [ "env";
      "VOX_Z3_HELPER_MODE=" ^ Filename.quote mode;
      "VOX_Z3_HELPER_COUNT=" ^ Filename.quote counter;
      "VOX_Z3_HELPER_STATE=" ^ Filename.quote state;
      Filename.quote Sys.executable_name;
    ]

let with_helper_files function_ =
  let counter = Filename.temp_file "vox-z3-starts" ".txt" in
  let state = Filename.temp_file "vox-z3-state" ".txt" in
  Sys.remove state;
  Fun.protect
    ~finally:(fun () ->
      if Sys.file_exists counter then Sys.remove counter;
      if Sys.file_exists state then Sys.remove state)
    (fun () -> function_ counter state)

let persistent_contents =
  "(set-option :produce-unsat-cores true)\n\
   (declare-const shared Int)\n\
   (assert (= shared 0))\n\
   (check-sat)\n\
   (get-unsat-core)\n"

let persistent_discharge ?(timeout_seconds = 30) command =
  Vox_smt.discharge ~backend:`Z3 ~command:(Some command)
    ~timeout_seconds ~env unused_fact_usage

let custom_discharge command =
  Vox_smt.discharge ~backend:`Z3 ~command:(Some command)
    ~prove_contents:persistent_contents ~env (vc (bool true))

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
  let markerless = discharge "true" arithmetic_and_booleans in
  assert (markerless.verdict = Vox_smt.Solver_error);
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
        assert (oxsmt.unused_facts = Some [])
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
  with_helper_files (fun counter state ->
    let command = helper_command "normal" counter state in
    let first = persistent_discharge command in
    let second = persistent_discharge command in
    assert (first.verdict = Vox_smt.Proved);
    if second.verdict <> Vox_smt.Proved then
      failwith
        (Option.value second.detail
           ~default:"second persistent discharge did not prove");
    assert (helper_line_count counter = 1));
  print_endline "z3 persistent session: one start and scoped isolation checked";
  with_helper_files (fun counter state ->
    let command = helper_command "normal" counter state in
    let discharge condition =
      Vox_smt.discharge ~backend:`Z3 ~command:(Some command) ~env condition
    in
    let first = discharge datatype in
    let second = discharge datatype in
    assert (first.verdict = Vox_smt.Proved);
    assert (second.verdict = Vox_smt.Proved);
    assert (helper_line_count counter = 1));
  print_endline "z3 persistent session: global datatype names isolated";
  with_helper_files (fun counter state ->
    let command = helper_command "option_check" counter state in
    let result =
      Vox_smt.discharge ~backend:`Z3 ~command:(Some command)
        ~env open_goal
    in
    assert (result.verdict = Vox_smt.Disproved);
    assert (helper_line_count counter = 1));
  print_endline "z3 persistent session: option baseline checked";
  with_helper_files (fun counter state ->
    let command = helper_command "early_exit_once" counter state in
    let recovered = persistent_discharge command in
    assert (recovered.verdict = Vox_smt.Proved);
    assert (helper_line_count counter = 2));
  print_endline "z3 persistent session: early exit restart checked";
  with_helper_files (fun counter state ->
    let command = helper_command "delay_once" counter state in
    let delayed = persistent_discharge ~timeout_seconds:1 command in
    assert (delayed.verdict = Vox_smt.Solver_error);
    let recovered = persistent_discharge command in
    assert (recovered.verdict = Vox_smt.Proved);
    assert (helper_line_count counter = 2));
  print_endline "z3 persistent session: timeout restart checked";
  with_helper_files (fun counter state ->
    let command = helper_command "normal" counter state in
    let first = custom_discharge command in
    let second = custom_discharge command in
    assert (first.verdict = Vox_smt.Proved);
    assert (second.verdict = Vox_smt.Proved);
    assert (helper_line_count counter = 2));
  print_endline "z3 custom contents: one-shot fallback checked";
  with_helper_files (fun counter state ->
    let command = helper_command "startup_error" counter state in
    let result = persistent_discharge command in
    assert (result.verdict = Vox_smt.Solver_error);
    assert (helper_line_count counter = 2));
  print_endline "z3 startup output: one-shot fallback checked"

let () =
  match Sys.getenv_opt "VOX_SMT_TEST_SOLVER" with
  | Some command ->
    let discharge condition =
      Vox_smt.discharge ~backend:`Z3 ~command:(Some command) ~env condition
    in
    let disproved = discharge (vc (bool false)) in
    assert (disproved.verdict = Vox_smt.Disproved);
    let not_proved = discharge open_goal in
    assert (not_proved.verdict = Vox_smt.Not_proved);
    List.iter
      (fun condition ->
        let result = discharge condition in
        assert (result.verdict = Vox_smt.Proved))
      [datatype; recursive_datatype; datatype; recursive_datatype]
  | None -> ()

let cache_test_directory =
  let path = Filename.temp_file "solver-cache-test-" "" in
  Sys.remove path;
  path

let clear_cache_test_directory () =
  if Sys.file_exists cache_test_directory then begin
    Array.iter
      (fun basename ->
        Sys.remove (Filename.concat cache_test_directory basename))
      (Sys.readdir cache_test_directory);
    Sys.rmdir cache_test_directory
  end

module Cache_test_backend = struct
  let backend = Vox_backend.Z3
  let capabilities = Vox_backend.capabilities backend
  let calls = Atomic.make 0
  let concurrent_calls = Atomic.make 0

  let cache_key ~command (obligation : Vox_backend.obligation) =
    match Vox_smt.emit ~query:Vox_smt.Prove ~env:obligation.Vox_backend.env
            obligation.condition
    with
    | Error _ -> None
    | Ok payload ->
      Some
        (String.concat "|"
           [ "test-schema";
             Vox_backend.string_of_backend backend;
             Option.value command ~default:"none";
             payload;
           ])

  let discharge ~command (obligation : Vox_backend.obligation) =
    ignore (Atomic.fetch_and_add calls 1);
    begin
      match command with
      | Some "concurrent" ->
        ignore (Atomic.fetch_and_add concurrent_calls 1);
        while Atomic.get concurrent_calls < 2 do
          Domain.cpu_relax ()
        done
      | Some _ | None -> ()
    end;
    let verdict =
      match command with
      | Some "failure" -> Vox_backend.Solver_error
      | Some "disproved" -> Vox_backend.Disproved
      | Some "not-proved" -> Vox_backend.Not_proved
      | Some "unknown" -> Vox_backend.Unknown
      | Some "solver-error" -> Vox_backend.Solver_error
      | Some "unavailable" -> Vox_backend.Unavailable
      | Some _ | None -> Vox_backend.Proved
    in
    { Vox_backend.backend;
      capabilities;
      verdict;
      location = obligation.condition.location;
      detail =
        (match command with
         | Some "metadata" -> Some "preserved detail"
         | Some _ | None -> None);
      unused_facts =
        (match command with
         | Some "metadata" -> Some [1]
         | Some _ | None -> Some []);
    }
end

module Cache_test_backend_again = struct
  include Cache_test_backend
end

module Cached_test = Vox_backend.Cached (Cache_test_backend)
module Cached_test_again = Vox_backend.Cached (Cache_test_backend_again)

module Other_cache_test_backend = struct
  include Cache_test_backend
  let backend = Vox_backend.Oxsmt
  let capabilities = Vox_backend.capabilities backend

  let cache_key ~command obligation =
    Option.map (fun key -> "other-backend|" ^ key)
      (Cache_test_backend.cache_key ~command obligation)
end

module Other_cached_test = Vox_backend.Cached (Other_cache_test_backend)

let cache_test_obligation ?prove_contents condition =
  Vox_backend.{ env; condition; prove_contents }

let cache_discriminate =
  shell_command
    "if grep -Fq '(assert (= false true))'; then printf 'unsat\\n'; \
     else printf 'sat\\n'; fi"

let cache_test_environment =
  [ "VOX_SOLVER_CACHE";
    "VOX_SOLVER_CACHE_DIR";
    "VOX_SOLVER_CACHE_COMPILER_IDENTITY";
    "VOX_Z3_SOLVER_VERSION";
    "VOX_OXSMT_LEGACY_EXTERNAL";
    "VOX_SOLVER_CACHE_MAX_BYTES";
    "VOX_LEAN";
    "PATH";
  ]

external unset_environment_variable : string -> bool
  = "caml_vox_unset_environment_variable"

let restore_environment saved =
  List.iter
    (fun (name, value) ->
      match value with
      | Some value -> Unix.putenv name value
      | None -> assert (unset_environment_variable name))
    saved

let restore_environment_variable saved name =
  match List.assoc name saved with
  | Some value -> Unix.putenv name value
  | None -> assert (unset_environment_variable name)

let cache_test_hex string =
  let digit value =
    if value < 10 then Char.chr (Char.code '0' + value)
    else Char.chr (Char.code 'a' + value - 10)
  in
  let result = Bytes.create (2 * String.length string) in
  String.iteri
    (fun index character ->
      let byte = Char.code character in
      Bytes.set result (2 * index) (digit (byte lsr 4));
      Bytes.set result ((2 * index) + 1) (digit (byte land 0xf)))
    string;
  Bytes.unsafe_to_string result

let write_private_file filename contents =
  let channel = open_out_bin filename in
  output_string channel contents;
  close_out channel;
  Unix.chmod filename 0o600

let string_contains ~needle haystack =
  let needle_length = String.length needle in
  let haystack_length = String.length haystack in
  let rec loop index =
    index + needle_length <= haystack_length
    &&
    (String.equal (String.sub haystack index needle_length) needle
     || loop (index + 1))
  in
  loop 0

let cache_test_record ?(verdict = "proved") key =
  let body =
    String.concat "\n"
      [ "vox-solver-cache-v1";
        cache_test_hex key;
        verdict;
        "none";
        "some:";
        "";
      ]
  in
  body ^ Digest.to_hex (Digest.string body) ^ "\n"

let corrupt_cache_test_checksum record =
  let index = String.length record - 2 in
  let bytes = Bytes.of_string record in
  Bytes.set bytes index (if Bytes.get bytes index = '0' then '1' else '0');
  Bytes.unsafe_to_string bytes

let cache_test_path key =
  Filename.concat cache_test_directory
    (Digest.to_hex (Digest.string key) ^ ".cache")

let () =
  let saved_environment =
    List.map (fun name -> name, Sys.getenv_opt name) cache_test_environment
  in
  clear_cache_test_directory ();
  Sys.mkdir cache_test_directory 0o700;
  Unix.putenv "VOX_SOLVER_CACHE" "1";
  Unix.putenv "VOX_SOLVER_CACHE_DIR" cache_test_directory;
  Unix.putenv "VOX_Z3_SOLVER_VERSION" "test-solver-v1";
  Unix.putenv "VOX_OXSMT_LEGACY_EXTERNAL" "0";
  Fun.protect
    ~finally:(fun () ->
      clear_cache_test_directory ();
      restore_environment saved_environment)
    (fun () ->
      let obligation = cache_test_obligation arithmetic_and_booleans in
      let first = Cached_test.discharge ~command:(Some "same") obligation in
      let second =
        Cached_test_again.discharge ~command:(Some "same") obligation
      in
      assert (first.verdict = Vox_backend.Proved);
      assert (second.verdict = Vox_backend.Proved);
      assert (Atomic.get Cache_test_backend.calls = 1);
      let changed_goal = cache_test_obligation (vc (bool true)) in
      ignore (Cached_test.discharge ~command:(Some "same") changed_goal);
      let changed_facts =
        cache_test_obligation (vc ~facts:[fact (bool true)] (bool true))
      in
      ignore (Cached_test.discharge ~command:(Some "same") changed_facts);
      ignore (Cached_test.discharge ~command:(Some "option") obligation);
      ignore (Other_cached_test.discharge ~command:(Some "same") obligation);
      assert (Atomic.get Cache_test_backend.calls = 5);
      let failing =
        Cached_test.discharge ~command:(Some "failure") obligation
      in
      let failing_again =
        Cached_test.discharge ~command:(Some "failure") obligation
      in
      assert (failing.verdict = Vox_backend.Solver_error);
      assert (failing_again.verdict = Vox_backend.Solver_error);
      assert (Atomic.get Cache_test_backend.calls = 7);
      let corrupt_key =
        Option.get
          (Cache_test_backend.cache_key ~command:(Some "corrupt") obligation)
      in
      let corrupt_path =
        Filename.concat cache_test_directory
          (Digest.to_hex (Digest.string corrupt_key) ^ ".cache")
      in
      let channel = open_out_bin corrupt_path in
      output_string channel "not a cache entry";
      close_out channel;
      ignore (Cached_test.discharge ~command:(Some "corrupt") obligation);
      assert (Atomic.get Cache_test_backend.calls = 8);
      let run_concurrent () =
        Cached_test.discharge ~command:(Some "concurrent") obligation
      in
      let domain = Domain.spawn run_concurrent in
      let local = run_concurrent () in
      let remote = Domain.join domain in
      assert (local.verdict = Vox_backend.Proved);
      assert (remote.verdict = Vox_backend.Proved);
      let cached = run_concurrent () in
      assert (cached.verdict = Vox_backend.Proved);
      assert (Atomic.get Cache_test_backend.calls = 10);
      let expect_repeated_calls ~command ~verdict ~calls =
        let before = Atomic.get Cache_test_backend.calls in
        let first =
          Cached_test.discharge ~command:(Some command) obligation
        in
        let second =
          Cached_test.discharge ~command:(Some command) obligation
        in
        assert (first.verdict = verdict);
        assert (second.verdict = verdict);
        assert (Atomic.get Cache_test_backend.calls = before + calls)
      in
      expect_repeated_calls ~command:"disproved"
        ~verdict:Vox_backend.Disproved ~calls:1;
      List.iter
        (fun (command, verdict) ->
          expect_repeated_calls ~command ~verdict ~calls:2)
        [ "not-proved", Vox_backend.Not_proved;
          "unknown", Vox_backend.Unknown;
          "solver-error", Vox_backend.Solver_error;
          "unavailable", Vox_backend.Unavailable;
        ];
      Fun.protect
        ~finally:(fun () -> Unix.chmod cache_test_directory 0o700)
        (fun () ->
          Unix.chmod cache_test_directory 0o755;
          expect_repeated_calls ~command:"public-directory"
            ~verdict:Vox_backend.Proved ~calls:2);
      let public_entry_key =
        Option.get
          (Cache_test_backend.cache_key ~command:(Some "public-entry") obligation)
      in
      let public_entry_path = cache_test_path public_entry_key in
      write_private_file public_entry_path
        (cache_test_record public_entry_key);
      Unix.chmod public_entry_path 0o644;
      let before_public_entry = Atomic.get Cache_test_backend.calls in
      let public_entry =
        Cached_test.discharge ~command:(Some "public-entry") obligation
      in
      assert (public_entry.verdict = Vox_backend.Proved);
      assert (Atomic.get Cache_test_backend.calls = before_public_entry + 1);
      let metadata_obligation = cache_test_obligation arithmetic_and_booleans in
      let metadata_location = Location.in_file "cache-hit-location.ml" in
      let relocated_condition =
        { arithmetic_and_booleans with location = metadata_location }
      in
      let relocated_obligation = cache_test_obligation relocated_condition in
      let metadata_first =
        Cached_test.discharge ~command:(Some "metadata") metadata_obligation
      in
      let metadata_cached =
        Cached_test.discharge ~command:(Some "metadata") relocated_obligation
      in
      assert (metadata_first.detail = Some "preserved detail");
      assert (metadata_cached.detail = Some "preserved detail");
      assert (metadata_cached.unused_facts = Some [1]);
      assert (metadata_cached.location = metadata_location);
      let expect_corrupt_miss command contents =
        let key =
          Option.get
            (Cache_test_backend.cache_key ~command:(Some command) obligation)
        in
        write_private_file (cache_test_path key) contents;
        let before = Atomic.get Cache_test_backend.calls in
        ignore (Cached_test.discharge ~command:(Some command) obligation);
        assert (Atomic.get Cache_test_backend.calls = before + 1)
      in
      let checksum_key =
        Option.get
          (Cache_test_backend.cache_key ~command:(Some "checksum") obligation)
      in
      expect_corrupt_miss "checksum"
        (corrupt_cache_test_checksum (cache_test_record checksum_key));
      let verdict_key =
        Option.get
          (Cache_test_backend.cache_key ~command:(Some "verdict") obligation)
      in
      expect_corrupt_miss "verdict"
        (cache_test_record ~verdict:"unknown" verdict_key);
      expect_corrupt_miss "oversized"
        (String.make ((8 * 1024 * 1024) + 1) 'x');
      let mismatch_target =
        Option.get
          (Cache_test_backend.cache_key ~command:(Some "mismatch") obligation)
      in
      expect_corrupt_miss "mismatch"
        (cache_test_record (mismatch_target ^ "-different"));
      Unix.putenv "VOX_SOLVER_CACHE_MAX_BYTES" "1";
      let before_eviction = Atomic.get Cache_test_backend.calls in
      ignore (Cached_test.discharge ~command:(Some "eviction") obligation);
      ignore (Cached_test.discharge ~command:(Some "eviction") obligation);
      assert (Atomic.get Cache_test_backend.calls = before_eviction + 2);
      restore_environment_variable saved_environment
        "VOX_SOLVER_CACHE_MAX_BYTES";
      let first_write_directory = Filename.temp_file "first-write-eviction-" "" in
      Sys.remove first_write_directory;
      Sys.mkdir first_write_directory 0o700;
      Fun.protect
        ~finally:(fun () ->
          Unix.putenv "VOX_SOLVER_CACHE_DIR" cache_test_directory;
          restore_environment_variable saved_environment
            "VOX_SOLVER_CACHE_MAX_BYTES";
          Array.iter
            (fun basename ->
              Sys.remove (Filename.concat first_write_directory basename))
            (Sys.readdir first_write_directory);
          Sys.rmdir first_write_directory)
        (fun () ->
          let abandoned =
            Filename.concat first_write_directory "write-abandoned.tmp"
          in
          let foreign = Filename.concat first_write_directory "z-foreign.cache" in
          write_private_file abandoned (String.make (700 * 1024) 'a');
          write_private_file foreign (String.make (700 * 1024) 'b');
          Unix.putenv "VOX_SOLVER_CACHE_DIR" first_write_directory;
          Unix.putenv "VOX_SOLVER_CACHE_MAX_BYTES" (string_of_int (1024 * 1024));
          ignore
            (Cached_test.discharge ~command:(Some "first-write-eviction")
               obligation);
          assert (not (Sys.file_exists abandoned));
          assert (Sys.file_exists foreign));
      let z3_obligation condition prove_contents =
        Vox_backend.{ env; condition; prove_contents = Some prove_contents }
      in
      Unix.putenv "VOX_SOLVER_CACHE_COMPILER_IDENTITY" "test-build-a";
      let z3_key_a =
        Vox_backend.Z3_backend.cache_key ~command:(Some cache_discriminate)
          (z3_obligation (vc (bool false)) persistent_contents)
      in
      let compiler_executable =
        if Filename.is_implicit Sys.executable_name then
          Sys.getenv "PATH"
          |> String.split_on_char ':'
          |> List.find_map (fun directory ->
            let candidate = Filename.concat directory Sys.executable_name in
            try
              let status = Unix.stat candidate in
              if status.st_kind <> Unix.S_REG then None
              else begin
                Unix.access candidate [Unix.X_OK];
                Some candidate
              end
            with Unix.Unix_error _ -> None)
          |> Option.get
        else Sys.executable_name
      in
      let compiler_digest = Digest.to_hex (Digest.file compiler_executable) in
      assert
        (match z3_key_a with
         | Some key -> string_contains ~needle:compiler_digest key
         | None -> false);
      Unix.putenv "VOX_SOLVER_CACHE_COMPILER_IDENTITY" "test-build-b";
      let z3_key_b =
        Vox_backend.Z3_backend.cache_key ~command:(Some cache_discriminate)
          (z3_obligation (vc (bool false)) persistent_contents)
      in
      assert (z3_key_a <> z3_key_b);
      let no_custom =
        Vox_backend.{ env; condition = vc (bool false); prove_contents = None }
      in
      let custom_none =
        Vox_backend.
          { env; condition = vc (bool false); prove_contents = Some "none" }
      in
      assert
        (Vox_backend.Z3_backend.cache_key ~command:(Some cache_discriminate) no_custom
         <> Vox_backend.Z3_backend.cache_key ~command:(Some cache_discriminate)
              custom_none);
      assert (unset_environment_variable "VOX_Z3_SOLVER_VERSION");
      List.iter
        (fun command ->
          assert
            (Vox_backend.Z3_backend.cache_key ~command:(Some command) no_custom
             = None))
        [ "env X=1 z3 -in";
          "sh -c solver";
          "'z3' -in";
          "z3\\ -in";
          "timeout 25 z3 -in";
          "python3 mysolver.py";
        ];
      let shadow_directory = Filename.temp_file "solver-shadow-" "" in
      let actual_directory = Filename.temp_file "solver-actual-" "" in
      Sys.remove shadow_directory;
      Sys.remove actual_directory;
      Sys.mkdir shadow_directory 0o700;
      Sys.mkdir actual_directory 0o700;
      let shadow_solver = Filename.concat shadow_directory "z3" in
      let actual_solver = Filename.concat actual_directory "z3" in
      Fun.protect
        ~finally:(fun () ->
          if Sys.file_exists shadow_solver then Sys.remove shadow_solver;
          if Sys.file_exists actual_solver then Sys.remove actual_solver;
          Sys.rmdir shadow_directory;
          Sys.rmdir actual_directory)
        (fun () ->
          write_private_file shadow_solver "not executable";
          write_private_file actual_solver "first";
          Unix.chmod actual_solver 0o700;
          Unix.putenv "PATH" (shadow_directory ^ ":" ^ actual_directory);
          let path_key_a =
            Vox_backend.Z3_backend.cache_key ~command:(Some "z3 -in") no_custom
          in
          let replacement =
            Filename.temp_file ~temp_dir:actual_directory "replacement-" ""
          in
          write_private_file replacement "other";
          Unix.chmod replacement 0o700;
          Sys.rename replacement actual_solver;
          let path_key_b =
            Vox_backend.Z3_backend.cache_key ~command:(Some "z3 -in") no_custom
          in
          assert (Option.is_some path_key_a);
          assert (Option.is_some path_key_b);
          assert (path_key_a <> path_key_b));
      restore_environment_variable saved_environment "PATH";
      let original_directory = Sys.getcwd () in
      let lean_working_directory = Filename.temp_file "lean-working-" "" in
      let lean_actual_directory = Filename.temp_file "lean-actual-" "" in
      Sys.remove lean_working_directory;
      Sys.remove lean_actual_directory;
      Sys.mkdir lean_working_directory 0o700;
      Sys.mkdir lean_actual_directory 0o700;
      let working_lean = Filename.concat lean_working_directory "lean" in
      let actual_lean = Filename.concat lean_actual_directory "lean" in
      Fun.protect
        ~finally:(fun () ->
          Sys.chdir original_directory;
          if Sys.file_exists working_lean then Sys.remove working_lean;
          if Sys.file_exists actual_lean then Sys.remove actual_lean;
          Sys.rmdir lean_working_directory;
          Sys.rmdir lean_actual_directory)
        (fun () ->
          write_private_file working_lean "cwd decoy";
          Unix.chmod working_lean 0o700;
          write_private_file actual_lean "first";
          Unix.chmod actual_lean 0o700;
          Unix.putenv "VOX_LEAN" "lean";
          Unix.putenv "PATH" lean_actual_directory;
          Sys.chdir lean_working_directory;
          let lean_key_a =
            Vox_backend.Lean_backend.cache_key ~command:None obligation
          in
          let replacement =
            Filename.temp_file ~temp_dir:lean_actual_directory "replacement-" ""
          in
          write_private_file replacement "other";
          Unix.chmod replacement 0o700;
          Sys.rename replacement actual_lean;
          let lean_key_b =
            Vox_backend.Lean_backend.cache_key ~command:None obligation
          in
          assert (Option.is_some lean_key_a);
          assert (Option.is_some lean_key_b);
          assert (lean_key_a <> lean_key_b));
      restore_environment_variable saved_environment "VOX_LEAN";
      restore_environment_variable saved_environment "PATH";
      Unix.putenv "VOX_Z3_SOLVER_VERSION" "test-solver-v1";
      assert
        (Option.is_some
           (Vox_backend.Z3_backend.cache_key ~command:(Some "sh -c solver")
              no_custom));
      let replaceable_solver =
        Filename.concat cache_test_directory "z3"
      in
      write_private_file replaceable_solver "first";
      Unix.chmod replaceable_solver 0o700;
      let solver_key_a =
        Vox_backend.Z3_backend.cache_key
          ~command:(Some replaceable_solver) no_custom
      in
      let replacement =
        Filename.temp_file ~temp_dir:cache_test_directory "replacement-" ""
      in
      write_private_file replacement "other";
      Unix.chmod replacement 0o700;
      Sys.rename replacement replaceable_solver;
      let solver_key_b =
        Vox_backend.Z3_backend.cache_key
          ~command:(Some replaceable_solver) no_custom
      in
      Sys.remove replaceable_solver;
      assert (Option.is_some solver_key_a);
      assert (Option.is_some solver_key_b);
      assert (solver_key_a <> solver_key_b);
      Unix.putenv "VOX_SOLVER_CACHE_COMPILER_IDENTITY" "test-build-a";
      let discharge_custom condition =
        Vox_backend.discharge
          ~selection:(Vox_backend.Single Vox_backend.Z3)
          ~smt_solver:(Some cache_discriminate) ~oxsmt_solver:None
          ~prove_contents:persistent_contents ~env condition
      in
      let disproved = discharge_custom (vc (bool false)) in
      let not_proved = discharge_custom (vc (bool true)) in
      assert (disproved.verdict = Vox_backend.Disproved);
      assert (not_proved.verdict = Vox_backend.Not_proved);
      let oxsmt_obligation prove_contents =
        Vox_backend.{ env; condition = arithmetic_and_booleans; prove_contents }
      in
      let oxsmt_key_a =
        Vox_backend.Oxsmt_backend.cache_key ~command:None
          (oxsmt_obligation (Some "unused custom text a"))
      in
      let oxsmt_key_b =
        Vox_backend.Oxsmt_backend.cache_key ~command:None
          (oxsmt_obligation (Some "unused custom text b"))
      in
      assert (oxsmt_key_a = oxsmt_key_b));
  print_endline
    "solver cache: persistent hits, exact inputs, invalidation, corruption, failures, and concurrent writes checked"
