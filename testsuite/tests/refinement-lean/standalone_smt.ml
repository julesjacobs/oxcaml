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
let stdlib_path = Path.Pident (Ident.create_persistent "Stdlib")
let bigint_path = Path.Pdot (stdlib_path, "Bigint")

let bigint_type =
  create_expr
    (Tconstr (Path.Pdot (bigint_path, "t"), [], ref Mnil))
    ~level:0
    ~scope:0
    ~id:(fresh_type_id ())

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
  let path = Path.Pdot (stdlib_path, name) in
  node type_ (Rexp_ident (Rfree (Rapp path)))

let bigint_primitive type_ name =
  let path = Path.Pdot (bigint_path, name) in
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
let greater_equal left right = binary ">=" int_type bool_type left right
let less_equal left right = binary "<=" int_type bool_type left right

let bigint_binary name result_type left right =
  let function_type =
    arrow bigint_type (arrow bigint_type result_type)
  in
  apply result_type (bigint_primitive function_type name) [left; right]

let bigint_of_int value =
  apply bigint_type
    (bigint_primitive (arrow int_type bigint_type) "of_int")
    [int value]

let bigint_equal left right =
  bigint_binary "equal" bool_type left right

let bigint_add left right =
  bigint_binary "add" bigint_type left right

let bigint_multiply left right =
  bigint_binary "mul" bigint_type left right

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
    { expression;
      location = Some loc;
      scope = None;
      origin = test_origin;
      producers = [test_origin];
    }

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

let nonlinear_square_sum =
  let left =
    { rb_id = Ident.create_scoped ~scope:10 "square_sum_left";
      rb_type = bigint_type;
    }
  in
  let right =
    { rb_id = Ident.create_scoped ~scope:10 "square_sum_right";
      rb_type = bigint_type;
    }
  in
  let sum = bigint_add (bound left) (bound right) in
  let lhs = bigint_multiply sum sum in
  let rhs =
    bigint_add
      (bigint_add
         (bigint_multiply (bound left) (bound left))
         (bigint_multiply
            (bigint_multiply (bigint_of_int 2) (bound left))
            (bound right)))
      (bigint_multiply (bound right) (bound right))
  in
  vc (bigint_equal lhs rhs)

let nonlinear_false_square_sum =
  let left =
    { rb_id = Ident.create_scoped ~scope:14 "false_square_sum_left";
      rb_type = bigint_type;
    }
  in
  let right =
    { rb_id = Ident.create_scoped ~scope:14 "false_square_sum_right";
      rb_type = bigint_type;
    }
  in
  let sum = bigint_add (bound left) (bound right) in
  let lhs = bigint_multiply sum sum in
  let rhs =
    bigint_add
      (bigint_add
         (bigint_multiply (bound left) (bound left))
         (bigint_multiply
            (bigint_multiply (bigint_of_int 3) (bound left))
            (bound right)))
      (bigint_multiply (bound right) (bound right))
  in
  vc
    ~facts:
      [ fact
          (disjunction
             (bigint_equal (bound left) (bigint_of_int 0))
             (bigint_equal (bound left) (bigint_of_int 1)));
        fact (bigint_equal (bound right) (bigint_of_int 1));
      ]
    (bigint_equal lhs rhs)

let nonlinear_false_pair_conflation =
  let variable name =
    { rb_id = Ident.create_scoped ~scope:15 name; rb_type = bigint_type }
  in
  let a = variable "pair_conflation_a" in
  let b = variable "pair_conflation_b" in
  let c = variable "pair_conflation_c" in
  let d = variable "pair_conflation_d" in
  let lhs =
    bigint_add
      (bigint_multiply (bound a) (bound b))
      (bigint_multiply (bound c) (bound d))
  in
  let rhs =
    bigint_add
      (bigint_multiply (bound a) (bound d))
      (bigint_multiply (bound c) (bound b))
  in
  vc
    ~facts:
      [ fact
          (disjunction
             (bigint_equal (bound a) (bigint_of_int 0))
             (bigint_equal (bound a) (bigint_of_int 1)));
        fact (bigint_equal (bound b) (bigint_of_int 1));
        fact (bigint_equal (bound c) (bigint_of_int 0));
        fact (bigint_equal (bound d) (bigint_of_int 0));
      ]
    (bigint_equal lhs rhs)

let nonlinear_commutativity ~scope ~left_count ~right_count =
  let variables side count =
    List.init count (fun index ->
      { rb_id =
          Ident.create_scoped ~scope
            (Printf.sprintf "commutativity_%s_%d" side index);
        rb_type = bigint_type;
      })
  in
  let sum variables =
    List.fold_left
      (fun sum variable -> bigint_add sum (bound variable))
      (bigint_of_int 0) variables
  in
  let left = sum (variables "left" left_count) in
  let right = sum (variables "right" right_count) in
  vc
    (bigint_equal
       (bigint_multiply left right)
       (bigint_multiply right left))

let nonlinear_commutativity_at_cap =
  nonlinear_commutativity ~scope:16 ~left_count:7 ~right_count:7

let nonlinear_commutativity_over_cap =
  nonlinear_commutativity ~scope:17 ~left_count:8 ~right_count:7

let nonlinear_nested =
  let value =
    { rb_id = Ident.create_scoped ~scope:11 "nested_value";
      rb_type = bigint_type;
    }
  in
  let value_term = bound value in
  let cube =
    bigint_multiply (bigint_multiply value_term value_term) value_term
  in
  vc ~facts:[fact (bigint_equal value_term (bigint_of_int 2))]
    (bigint_equal cube (bigint_of_int 8))

let nonlinear_zero_lemma =
  let left =
    { rb_id = Ident.create_scoped ~scope:12 "zero_left";
      rb_type = bigint_type;
    }
  in
  let right =
    { rb_id = Ident.create_scoped ~scope:12 "zero_right";
      rb_type = bigint_type;
    }
  in
  vc ~facts:[fact (bigint_equal (bound left) (bigint_of_int 0))]
    (bigint_equal
       (bigint_multiply (bound left) (bound right))
       (bigint_of_int 0))

let nonlinear_over_cap =
  let variables =
    List.init 9 (fun index ->
      { rb_id =
          Ident.create_scoped ~scope:13
            (Printf.sprintf "over_cap_%d" index);
        rb_type = bigint_type;
      })
  in
  let sum =
    List.fold_left
      (fun sum variable -> bigint_add sum (bound variable))
      (bigint_of_int 0) variables
  in
  let facts =
    List.map
      (fun variable ->
        fact (bigint_equal (bound variable) (bigint_of_int 2)))
      variables
  in
  vc ~facts
    (bigint_equal (bigint_multiply sum sum) (bigint_of_int 324))

let ordinary_nonlinear_wrap_sign =
  let value =
    { rb_id = Ident.create_scoped ~scope:18 "wrap_sign_value";
      rb_type = int_type;
    }
  in
  vc (greater_equal (multiply (bound value) (bound value)) (int 0))

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
  match Sys.getenv_opt "VOX_OXSMT_NIA_TEST_MODE" with
  | Some "on" ->
    assert
      ((Vox_smt.discharge_oxsmt ~env nonlinear_square_sum).verdict
       = Vox_smt.Proved);
    assert
      ((Vox_smt.discharge_oxsmt ~env nonlinear_zero_lemma).verdict
       = Vox_smt.Proved);
    List.iter
      (fun condition ->
        let result = Vox_smt.discharge_oxsmt ~env condition in
        assert (result.verdict = Vox_smt.Not_proved);
        assert
          (result.detail = Some "prove query: sat; disprove query: sat"))
      [nonlinear_false_square_sum; nonlinear_false_pair_conflation];
    assert
      ((Vox_smt.discharge_oxsmt
          ~env nonlinear_commutativity_at_cap).verdict
       = Vox_smt.Proved);
    let over_cap_commutativity =
      Vox_smt.discharge_oxsmt ~env nonlinear_commutativity_over_cap
    in
    assert (over_cap_commutativity.verdict = Vox_smt.Not_proved);
    assert
      (over_cap_commutativity.detail
       = Some "prove query: unknown; disprove query: sat");
    let nested = Vox_smt.discharge_oxsmt ~env nonlinear_nested in
    assert (nested.verdict = Vox_smt.Not_proved);
    assert
      (nested.detail = Some "prove query: unknown; disprove query: sat");
    let over_cap = Vox_smt.discharge_oxsmt ~env nonlinear_over_cap in
    assert (over_cap.verdict = Vox_smt.Not_proved);
    assert
      (over_cap.detail = Some "prove query: unknown; disprove query: sat");
    exit 0
  | Some "off" ->
    let result = Vox_smt.discharge_oxsmt ~env nonlinear_square_sum in
    assert (result.verdict = Vox_smt.Not_proved);
    assert
      (result.detail = Some "prove query: unknown; disprove query: unknown");
    exit 0
  | Some mode -> failwith ("unknown VOX_OXSMT_NIA_TEST_MODE: " ^ mode)
  | None -> ()

let nia_test_command ~mode ~nia =
  let environment =
    match nia with
    | None -> ["env"; "-u"; "OXSMT_NIA"]
    | Some value -> ["env"; "OXSMT_NIA=" ^ Filename.quote value]
  in
  String.concat " "
    (environment
     @ [ "VOX_OXSMT_NIA_TEST_MODE=" ^ Filename.quote mode;
         Filename.quote Sys.executable_name;
       ])

let () =
  assert (Sys.command (nia_test_command ~mode:"on" ~nia:None) = 0);
  assert (Sys.command (nia_test_command ~mode:"off" ~nia:(Some "0")) = 0);
  assert (Sys.command (nia_test_command ~mode:"off" ~nia:(Some "off")) = 0);
  assert
    (Sys.command
       (nia_test_command ~mode:"on" ~nia:(Some "unrecognized"))
     = 0)

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
  List.iter
    (fun condition ->
      let rejected = Vox_smt.discharge_oxsmt ~env condition in
      assert (rejected.verdict = Vox_smt.Not_proved);
      assert
        (rejected.detail
         = Some "prove query: unknown; disprove query: unknown"))
    [nonlinear; ordinary_nonlinear_wrap_sign];
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
