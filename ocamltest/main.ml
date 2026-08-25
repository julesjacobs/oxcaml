(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Main program of the ocamltest test driver *)

open Ocamltest_stdlib
open Tsl_ast
open Tsl_semantics

type behavior =
  | Skip_all
  | Run

(* this primitive announce should be used for tests
   that were aborted on system error before ocamltest
   could parse them *)
let announce_test_error test_filename error =
  Printf.printf " ... testing '%s' => unexpected error (%s)\n%!"
    (Filename.basename test_filename) error

let print_exn loc e bt =
  let open Printf in
  let locstring =
    if loc = Location.none then "" else begin
      let file = loc.Location.loc_start.Lexing.pos_fname in
      let line = loc.Location.loc_start.Lexing.pos_lnum in
      sprintf "%s:%d: " file line
    end
  in
  let msg =
    match e with
    | Variables.Variable_already_registered v ->
      sprintf "Variable \"%s\" is already in the environment.\n" v
    | Variables.No_such_variable v ->
      sprintf "Variable \"%s\" is not in the environment.\n" v
    | Environments.Modifiers_name_not_found name ->
      sprintf "Environment modifier \"%s\" does not exist.\n" name
    | Tsl_semantics.No_such_test_or_action name ->
      sprintf "This is not the name of a test or an action: \"%s\".\n" name
    | Ocaml_actions.Cannot_compile_file_type t ->
      sprintf "Cannot compile files of type %s.\n" t
    | _ ->
      sprintf "Unexpected exception: %s\n%s" (Printexc.to_string e) bt
  in
  eprintf "\n%s%s%!" locstring msg

exception Syntax_error of Lexing.position

let tsl_parse_file test_filename =
  let input_channel = open_in test_filename in
  let lexbuf = Lexing.from_channel input_channel in
  Location.init lexbuf test_filename;
  match Tsl_parser.tsl_script Tsl_lexer.token lexbuf with
    | exception Parsing.Parse_error ->
      raise (Syntax_error lexbuf.Lexing.lex_start_p)
    | exception e -> close_in input_channel; raise e
    | _ as tsl_block -> close_in input_channel; tsl_block

let tsl_parse_file_safe test_filename =
  try tsl_parse_file test_filename with
  | Sys_error message ->
    Printf.eprintf "%s\n%!" message;
    announce_test_error test_filename message;
    exit 1
  | Syntax_error p ->
    let open Lexing in
    Printf.eprintf "%s:%d.%d: syntax error in test script\n%!"
      test_filename p.pos_lnum (p.pos_cnum - p.pos_bol);
    announce_test_error test_filename "could not read test script";
    exit 1

let print_usage () =
  Printf.printf "%s\n%!" Options.usage

let report_error loc e bt =
  print_exn loc e bt;
  "=> error in test script"

type summary = Pass | Skip | Fail

let summary_of_status : Result.status -> summary = function
  | Pass | Predicate true -> Pass
  | Skip | Predicate false -> Skip
  | Fail -> Fail

(* The sequential join passes if both tests pass.

   This implies that a linear sequence of actions, a path along the
   test tree, is considered successful if all actions passed. *)
let join_sequential r1 r2 =
  match r1, r2 with
  | Fail, _ | _, Fail -> Fail
  | Pass, Pass -> Pass
  | Skip, _ | _, Skip -> Skip

(* The parallel join passes if either test passes.

   This implies that a test formed of several parallel branches is
   considered successful if at least one of the branches is successful.
*)
let join_parallel r1 r2 =
  match r1, r2 with
  | Fail, _ | _, Fail -> Fail
  | Pass, _ | _, Pass -> Pass
  | Skip, Skip -> Skip

let join_list_parallel results = List.fold_left join_parallel Skip results

let string_of_summary = function
  | Pass -> "passed"
  | Fail -> "failed"
  | Skip -> "skipped"

let string_of_test_location ~branches line =
  String.concat ""
    (List.rev_map (fun branch -> Printf.sprintf "branch %d: " branch)
       branches)
  ^ Printf.sprintf "line %d" line

let run_test_tree log add_msg behavior env summ ast =
  (* Interpret the statements contained in [stmts] and [segments] (effectively,
     run [stmts @ List.concat segments]), then go into each of the subtrees in
     [subs]. [branches] is a list of indices of [split] arms that we've
     descended into. *)
  let rec run_statements behavior env summ stmts segments subs ~branches
      ~must_do_something =
    match stmts with
    | Environment_statement s :: rest ->
      begin match interpret_environment_statement env s with
      | env ->
        run_statements behavior env summ rest segments subs ~branches
          ~must_do_something
      | exception e ->
        let bt = Printexc.get_backtrace () in
        let line = s.loc.Location.loc_start.Lexing.pos_lnum in
        Printf.ksprintf add_msg "%s %s"
          (string_of_test_location ~branches line)
          (report_error s.loc e bt);
        Fail
      end
    | Test (_, name, mods) :: rest ->
      let skip_all =
        match behavior with
        | Skip_all -> true
        | Run -> false
      in
      let locstr =
        if name.loc = Location.none then
          "default"
        else
          string_of_test_location ~branches
            name.loc.Location.loc_start.Lexing.pos_lnum
      in
      let name = { name with node = Environments.expand env name.node } in
      let test = lookup_test name in
      let (msg, behavior, env, result) =
        match behavior with
        | Skip_all -> ("", Skip_all, env, Result.skip)
        | Run ->
          begin try
            let testenv = List.fold_left apply_modifiers env mods in
            let (result, newenv) = Tests.run log testenv test in
            let msg = Result.string_of_result result in
            let sub_behavior =
              if Result.is_pass result then Run else Skip_all in
            (msg, sub_behavior, newenv, result)
          with e ->
            let bt = Printexc.get_backtrace () in
            (report_error name.loc e bt, Skip_all, env, Result.fail)
          end
      in
      if not skip_all then
        Printf.ksprintf add_msg "%s (%s) %s" locstr name.node msg;
      let summ = join_sequential summ (summary_of_status result.status) in
      let must_do_something =
        must_do_something && not (Tests.does_something test)
      in
      run_statements behavior env summ rest segments subs ~branches
        ~must_do_something
    | Split alternatives :: rest ->
      let count = List.length alternatives in
      let run_alternative i alternative =
        Printf.fprintf log "Running split branch %d of %d\n%!" (i + 1) count;
        run_statements behavior env summ alternative (rest :: segments)
          subs ~branches:(i + 1 :: branches) ~must_do_something
      in
      join_list_parallel (List.mapi run_alternative alternatives)
    | [] ->
      run_segments behavior env summ segments subs ~branches ~must_do_something
  and run_segments behavior env summ segments subs ~branches
      ~must_do_something =
    match segments with
    | segment :: segments ->
        run_statements behavior env summ segment segments subs ~branches
          ~must_do_something
    | [] ->
        (* If [subs] is empty, there are no further test actions to
           perform: we are at the end of a test path and can report
           our current summary. Otherwise we continue with each
           branch, and parallel-join the result summaries. *)
        begin match subs with
        | [] ->
          if not must_do_something then summ
          else (
            match summ with
            | Pass ->
              add_msg "does the test tree do something? => failed";
              Fail
            | Skip | Fail -> summ
          )
        | _ ->
        (* CR mshinwell/xclerc: maybe sequences of actions that "do something"
           and then have further actions that do not "do something" should be
           flagged *)
            join_list_parallel
              (List.map
                (run_tree behavior env summ ~branches ~must_do_something)
                subs)
        end
  and run_tree behavior env summ (Ast (stmts, subs)) ~branches
      ~must_do_something =
    run_statements behavior env summ stmts [] subs ~branches ~must_do_something
  in run_tree behavior env summ ast ~branches:[] ~must_do_something:true

let get_test_source_directory test_dirname =
  if (Filename.is_relative test_dirname) then
    Sys.with_chdir test_dirname Sys.getcwd
  else test_dirname

let get_test_build_directory_prefix test_dirname =
  let ocamltestdir_variable = "OCAMLTESTDIR" in
  let root =
    Sys.getenv_with_default_value ocamltestdir_variable
      (Filename.concat (Sys.getcwd ()) "_ocamltest")
  in
  if test_dirname = "." then root
  else Filename.concat root test_dirname

let tests_to_skip = ref []

let init_tests_to_skip () =
  tests_to_skip := String.words (Sys.safe_getenv "OCAMLTEST_SKIP_TESTS")

let extract_rootenv (Ast (stmts, subs)) =
  let (env, stmts) = split_env stmts in
  (env, Ast (stmts, subs))

let parse_test_file test_filename =
  let tsl_ast = tsl_parse_file_safe test_filename in
  let rootenv_statements, tsl_ast = extract_rootenv tsl_ast in
  let tsl_ast = match[@ocaml.warning "-fragile-match"] tsl_ast with
    | Ast ([], []) ->
      let default_tests = Tests.default_tests() in
      let make_tree test =
        let id = make_identifier test.Tests.test_name in
        Ast ([Test (0, id, [])], [])
      in
      Ast ([], List.map make_tree default_tests)
    | _ -> tsl_ast
  in
  rootenv_statements, tsl_ast

let test_file test_filename =
  let start = if Options.show_timings then Unix.gettimeofday () else 0.0 in
  let skip_test = List.mem test_filename !tests_to_skip in
  let rootenv_statements, tsl_ast = parse_test_file test_filename in
  let used_tests = tests_in_tree tsl_ast in
  let used_actions = actions_in_tests used_tests in
  let action_names =
    let f act names = String.Set.add (Actions.name act) names in
    Actions.ActionSet.fold f used_actions String.Set.empty in
  let test_dirname = Filename.dirname test_filename in
  let test_basename = Filename.basename test_filename in
  let test_prefix = Filename.chop_extension test_basename in
  let test_directory =
    if test_dirname="." then test_prefix
    else Filename.concat test_dirname test_prefix in
  let test_source_directory = get_test_source_directory test_dirname in
  let hookname_prefix = Filename.concat test_source_directory test_prefix in
  let test_build_directory_prefix =
    get_test_build_directory_prefix test_directory in
  let clean_test_build_directory () =
    try
      Sys.rm_rf test_build_directory_prefix
    with Sys_error _ -> ()
  in
  clean_test_build_directory ();
  Sys.make_directory test_build_directory_prefix;
  let log_filename =
    Filename.concat test_build_directory_prefix (test_prefix ^ ".log") in
  let log =
    if Options.log_to_stderr then stderr else begin
      open_out log_filename
    end in
  let summary = Sys.with_chdir test_build_directory_prefix
    (fun () ->
       let promote = string_of_bool Options.promote in
       let default_timeout = string_of_int Options.default_timeout in
       let install_hook name =
         let hook_name = Filename.make_filename hookname_prefix name in
         if Sys.file_exists hook_name then begin
           let hook = Actions_helpers.run_hook hook_name in
           Actions.set_hook name hook
         end in
       String.Set.iter install_hook action_names;

       let reference_filename = Filename.concat
           test_source_directory (test_prefix ^ ".reference") in
       let make = try Sys.getenv "MAKE" with Not_found -> "make" in
       let initial_environment = Environments.from_bindings
           [
             Builtin_variables.dev_null, "/dev/null";
             Builtin_variables.make, make;
             Builtin_variables.test_file, test_basename;
             Builtin_variables.reference, reference_filename;
             Builtin_variables.test_source_directory, test_source_directory;
             Builtin_variables.test_build_directory_prefix,
               test_build_directory_prefix;
             Builtin_variables.promote, promote;
             Builtin_variables.timeout, default_timeout;
           ] in
       let initial_status = if skip_test then Skip_all else Run in
       let rootenv =
         Environments.initialize Environments.Pre log initial_environment
       in
       let msgs = ref [] in
       let add_msg s = msgs := s :: !msgs in
       let rootenv, initial_status, initial_summary =
         let rec loop env stmts =
           match stmts with
           | [] -> (env, initial_status, Pass)
           | s :: t ->
             begin match interpret_environment_statement env s with
             | env -> loop env t
             | exception e ->
               let bt = Printexc.get_backtrace () in
               let line = s.loc.Location.loc_start.Lexing.pos_lnum in
               Printf.ksprintf add_msg "line %d %s" line
                 (report_error s.loc e bt);
               (env, Skip_all, Fail)
             end
         in
         loop rootenv rootenv_statements
       in
       let rootenv = Environments.initialize Environments.Post log rootenv in
       let summary =
         run_test_tree log add_msg initial_status rootenv initial_summary
           tsl_ast
       in
       let common_prefix = " ... testing '" ^ test_basename ^ "'" in
       let msgs = List.rev !msgs in
       if summary = Skip && msgs <> [] then
         List.iter (Printf.printf "%s with %s\n%!" common_prefix) msgs
       else begin
         Printf.printf "%s => %s%s\n%!" common_prefix
           (string_of_summary summary)
           (if Options.show_timings && summary = Pass then
              let wall_clock_duration = Unix.gettimeofday () -. start in
              Printf.sprintf " (wall clock: %.02fs)" wall_clock_duration
            else "");
         if summary = Fail then
           List.iter (Printf.printf "%s with %s\n%!" common_prefix) msgs
       end;
       Actions.clear_all_hooks();
       summary
    ) in
  if not Options.log_to_stderr then close_out log;
  begin match summary with
  | Fail ->
      if not Options.log_to_stderr then
        Sys.dump_file stderr ~prefix:"> " log_filename
  | Pass | Skip ->
      if not Options.keep_test_dir_on_success then
        clean_test_build_directory ()
  end

let plan_incremental files =
  let fail_unsupported filename format =
    Printf.ksprintf
      (fun message ->
        Printf.eprintf "Incremental testing does not support %s in %s.\n%!"
          message filename;
        exit 2)
      format
  in
  let action_hook filename action =
    let basename = Filename.basename filename in
    let prefix = Filename.chop_extension basename in
    let prefix = Filename.concat (Filename.dirname filename) prefix in
    Filename.make_filename prefix (Actions.name action)
  in
  let add_action filename action requirements =
    let hook = action_hook filename action in
    if Sys.file_exists hook then
      fail_unsupported filename "action hook %S" hook;
    match Actions.incremental_requirement action with
    | Actions.No_artifact -> requirements
    | Actions.Requirement requirement ->
        String.Set.add requirement requirements
    | Actions.Unsupported ->
        fail_unsupported filename "action %S" (Actions.name action)
  in
  let generated_source filename =
    Filename.check_suffix filename ".mll" ||
    Filename.check_suffix filename ".mly"
  in
  let value_uses_generator value =
    List.exists generated_source (String.words value.node)
  in
  let environment_uses_generator statement =
    match statement.node with
    | Assignment (_, _, value) | Append (_, value) ->
        value_uses_generator value
    | Include _ | Unset _ -> false
  in
  let rec item_uses_generator = function
    | Environment_statement statement ->
        environment_uses_generator statement
    | Test (_, _, modifiers) ->
        List.exists value_uses_generator modifiers
    | Split alternatives ->
        List.exists (List.exists item_uses_generator) alternatives
  in
  let rec tree_uses_generator (Ast (statements, subtrees)) =
    List.exists item_uses_generator statements
    || List.exists tree_uses_generator subtrees
  in
  let environment_uses_expanded_generator env =
    List.exists
      (fun variable ->
        match Environments.lookup variable env with
        | None -> false
        | Some value -> List.exists generated_source (String.words value))
      Ocaml_variables.[all_modules; modules; plugins]
  in
  let apply_planning_statement filename env statement =
    let env =
      try interpret_environment_statement env statement with
      | exn ->
          fail_unsupported filename "environment expansion (%s)"
            (Printexc.to_string exn)
    in
    if environment_uses_expanded_generator env then
      fail_unsupported filename "generated lexer or parser sources";
    env
  in
  let rec plan_items filename env = function
    | [] -> [env]
    | Environment_statement statement :: rest ->
        let env = apply_planning_statement filename env statement in
        plan_items filename env rest
    | Test (_, _, modifiers) :: rest ->
        let env =
          List.fold_left
            (fun env modifier ->
              try apply_modifiers env modifier with
              | exn ->
                  fail_unsupported filename "environment modifier %S (%s)"
                    modifier.node (Printexc.to_string exn))
            env modifiers
        in
        if environment_uses_expanded_generator env then
          fail_unsupported filename "generated lexer or parser sources";
        plan_items filename env rest
    | Split alternatives :: rest ->
        List.concat_map
          (fun alternative -> plan_items filename env (alternative @ rest))
          alternatives
  in
  let rec plan_tree filename env (Ast (statements, subtrees)) =
    let environments = plan_items filename env statements in
    List.iter
      (fun env ->
        List.iter (fun subtree -> plan_tree filename env subtree) subtrees)
      environments
  in
  let compilerlibs_requirement = function
    | "ocamlcommon" -> Some "compilerlibs.ocamlcommon"
    | "ocamlbytecomp" -> Some "compilerlibs.ocamlbytecomp"
    | "ocamloptcomp" -> Some "compilerlibs.ocamloptcomp"
    | "ocamltoplevel" -> Some "compilerlibs.ocamltoplevel"
    | "ocamlmiddleend" ->
        failwith "the ocamlmiddleend modifier has no Dune install artifact"
    | _ -> None
  in
  let add_modifier_requirement filename name requirements =
    match compilerlibs_requirement name.node with
    | Some requirement -> String.Set.add requirement requirements
    | None -> requirements
    | exception Failure message -> fail_unsupported filename "%s" message
  in
  let add_environment_requirement filename statement requirements =
    match statement.node with
    | Include name -> add_modifier_requirement filename name requirements
    | Assignment _ | Append _ | Unset _ -> requirements
  in
  let rec add_item_requirements filename requirements = function
    | Environment_statement statement ->
        add_environment_requirement filename statement requirements
    | Test (_, _, modifiers) ->
        List.fold_left
          (fun requirements name ->
            add_modifier_requirement filename name requirements)
          requirements modifiers
    | Split alternatives ->
        List.fold_left
          (List.fold_left (add_item_requirements filename))
          requirements alternatives
  in
  let rec add_tree_requirements filename requirements
      (Ast (statements, subtrees)) =
    let requirements =
      List.fold_left (add_item_requirements filename) requirements statements
    in
    List.fold_left (add_tree_requirements filename) requirements subtrees
  in
  let compiler_requirement action =
    match Actions.incremental_requirement action with
    | Actions.Requirement
        ("ocamlc.byte" | "ocamlc.opt" | "ocamlopt.byte" | "ocamlopt.opt") ->
        true
    | Actions.No_artifact | Actions.Requirement _ | Actions.Unsupported ->
        false
  in
  let add_file requirements filename =
    let rootenv, tsl_ast = parse_test_file filename in
    let tests =
      try tests_in_tree_strict tsl_ast with
      | No_such_test_or_action name ->
          Printf.eprintf
            "Incremental testing cannot resolve test or action %S in %s.\n%!"
            name filename;
          exit 2
    in
    let actions = actions_in_tests tests in
    let root_environment =
      List.fold_left
        (apply_planning_statement filename)
        Environments.empty rootenv
    in
    plan_tree filename root_environment tsl_ast;
    if Actions.ActionSet.exists compiler_requirement actions &&
       (generated_source filename ||
        List.exists environment_uses_generator rootenv ||
        tree_uses_generator tsl_ast)
    then fail_unsupported filename "generated lexer or parser sources";
    let requirements =
      List.fold_left
        (fun requirements statement ->
          add_environment_requirement filename statement requirements)
        requirements rootenv
    in
    let requirements = add_tree_requirements filename requirements tsl_ast in
    Actions.ActionSet.fold (add_action filename) actions requirements
  in
  let requirements = List.fold_left add_file String.Set.empty files in
  String.Set.iter print_endline requirements

let is_test filename =
  let input_channel = open_in filename in
  let lexbuf = Lexing.from_channel input_channel in
  Fun.protect ~finally:(fun () -> close_in input_channel) begin fun () ->
    Tsl_lexer.is_test lexbuf
  end

let ignored s =
  s = "" || s.[0] = '_' || s.[0] = '.'

let sort_strings = List.sort String.compare

let find_test_dirs dir =
  let res = ref [] in
  let rec loop dir =
    let contains_tests = ref false in
    Array.iter (fun s ->
        if ignored s then ()
        else begin
          let s = dir ^ "/" ^ s in
          if Sys.is_directory s then loop s
          else if not !contains_tests && is_test s then contains_tests := true
        end
      ) (Sys.readdir dir);
    if !contains_tests then res := dir :: !res
  in
  loop dir;
  sort_strings !res

let list_tests dir =
  let res = ref [] in
  if Sys.is_directory dir then begin
    Array.iter (fun s ->
        if ignored s then ()
        else begin
          let s' = dir ^ "/" ^ s in
          if Sys.is_directory s' || not (is_test s') then ()
          else res := s :: !res
        end
      ) (Sys.readdir dir)
  end;
  sort_strings !res

let () =
  Actions.init ();
  Builtin_actions.init ();
  Builtin_variables.init ();
  Ocaml_actions.init ();
  Ocaml_modifiers.init ();
  Ocaml_tests.init ();
  Ocaml_variables.init ();
  Debugger_actions.init ();
  Debugger_variables.init ();
  Strace.init ();
  init_tests_to_skip()

let () =
  let failed = ref false in
  let work_done = ref false in
  let list_tests dir =
    match list_tests dir with
    | [] -> failed := true
    | res -> List.iter print_endline res
  in
  let find_test_dirs dir = List.iter print_endline (find_test_dirs dir) in
  let doit f x = work_done := true; f x in
  if Options.plan_incremental then begin
    if Options.translate || Options.find_test_dirs <> [] ||
       Options.list_tests <> [] then begin
      Printf.eprintf
        "-plan-incremental cannot be combined with listing or translation.\n%!";
      exit 2
    end;
    if Options.files_to_test = [] then begin
      Printf.eprintf "-plan-incremental requires at least one test file.\n%!";
      exit 2
    end;
    work_done := true;
    plan_incremental Options.files_to_test
  end else begin
    List.iter (doit find_test_dirs) Options.find_test_dirs;
    List.iter (doit list_tests) Options.list_tests;
    let do_file =
      if Options.translate then
        Translate.file ~style:Options.style ~compact:Options.compact
      else
        test_file
    in
    List.iter (doit do_file) Options.files_to_test
  end;
  if not !work_done then print_usage();
  if !failed || not !work_done then exit 1
