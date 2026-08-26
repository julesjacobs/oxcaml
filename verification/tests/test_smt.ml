open Vox_smt

let query ?(symbols = []) ?(functions = []) ?(facts = []) term =
  { symbols; functions; facts; goal = { label = "goal"; term } }

let app op args = App (op, args)

let integer n = Integer (Int64.of_int n)

let serialize q = to_smtlib ~int_width:63 ~timeout_ms:5000 q

let sort_error q =
  match serialize q with
  | _ -> failwith "Expected sort error"
  | exception Sort_error _ -> ()

let () =
  let x = Symbol.create ~label:"x" Bv63 in
  let b = Symbol.create ~label:"x" Bool in
  let f =
    Function.create ~label:"v0) (assert false)" ~arguments:[Bv63] ~result:Bv63
  in
  let call = Call (f, [integer 0]) in
  sort_error (query (app Eq [call; call]));
  sort_error (query ~functions:[f; f] (Boolean true));
  sort_error (query ~functions:[f] (app Eq [Call (f, []); call]));
  sort_error (query ~functions:[f] (app Eq [Call (f, [Boolean true]); call]));
  assert (
    serialize (query ~functions:[f] (app Eq [call; call]))
    = "(set-option :print-success false)\n\
       (set-option :produce-models true)\n\
       (set-option :timeout 5000)\n\
       (set-logic QF_UFBV)\n\
       (declare-fun f0 ((_ BitVec 63)) (_ BitVec 63))\n\
       (assert (not (= (f0 (_ bv0 63)) (f0 (_ bv0 63)))))\n\
       (check-sat)\n");
  let bad_arity =
    [Add; Sub; Mul; Neg; Eq; Ne; Lt; Le; Gt; Ge; Not; And; Or; Implies; Ite]
  in
  List.iter (fun op -> sort_error (query (app op []))) bad_arity;
  List.iter sort_error
    [ query (Var x);
      query ~symbols:[x; x] (Boolean true);
      query (integer 0);
      query ~facts:[{ label = "bad fact"; term = integer 0 }] (Boolean true);
      query (app Eq [Boolean false; integer 0]);
      query (app Lt [Boolean false; Boolean true]);
      query (app And [integer 0; integer 1]);
      query (app Eq [Integer Int64.min_int; integer 0]);
      query (app Eq [Integer Int64.max_int; integer 0]);
      query (app Ite [integer 0; Boolean false; Boolean true]);
      query (app Ite [Boolean true; integer 0; Boolean true]) ];
  (match to_smtlib ~int_width:31 ~timeout_ms:1 (query (Boolean true)) with
  | _ -> failwith "Expected unsupported target"
  | exception Unsupported_target 31 -> ());
  (match
     serialize
       (query
          ~facts:[{ label = "source fact"; term = integer 0 }]
          (Boolean true))
   with
  | _ -> failwith "Expected labelled sort error"
  | exception Sort_error message ->
    assert (String.starts_with ~prefix:"source fact:" message));
  let collision_query () =
    let a = Symbol.create ~label:"v0) (assert false) ;" Bv63 in
    let b = Symbol.create ~label:"v0) (assert false) ;" Bv63 in
    query ~symbols:[a; b] (app Eq [Var a; Var b])
  in
  let serialized = serialize (collision_query ()) in
  assert (serialized = serialize (collision_query ()));
  assert (
    serialized
    = "(set-option :print-success false)\n\
       (set-option :produce-models true)\n\
       (set-option :timeout 5000)\n\
       (set-logic QF_BV)\n\
       (declare-fun v0 () (_ BitVec 63))\n\
       (declare-fun v1 () (_ BitVec 63))\n\
       (assert (not (= v0 v1)))\n\
       (check-sat)\n");
  ignore
    (serialize
       (query ~symbols:[x; b]
          (app Eq [Var x; app Ite [Var b; integer 0; integer (-1)]])))

let fake =
  if Filename.is_relative Sys.argv.(1)
  then Filename.concat (Sys.getcwd ()) Sys.argv.(1)
  else Sys.argv.(1)

let pid_file = Filename.temp_file "vox-smt-pid" ".txt"

let reaped () =
  let input = open_in pid_file in
  let pid = int_of_string (input_line input) in
  close_in input;
  (match Unix.waitpid [Unix.WNOHANG] pid with
  | _ -> failwith "Solver was not reaped"
  | exception Unix.Unix_error (Unix.ECHILD, _, _) -> ());
  match Unix.kill pid 0 with
  | () -> failwith "Solver is still running"
  | exception Unix.Unix_error (Unix.ESRCH, _, _) -> ()

let config timeout_ms = { Vox_smt_solver.executable = fake; timeout_ms }

let run ?(timeout_ms = 2000) ?dump mode q =
  Unix.putenv "VOX_FAKE_MODE" mode;
  let r =
    Vox_smt_solver.check ~config:(config timeout_ms) ?dump ~int_width:63 q
  in
  reaped ();
  assert (String.starts_with ~prefix:"fake solver started\n" r.stderr);
  r

let validity r = r.Vox_smt_solver.validity

let is_failure = function Failure _ -> true | _ -> false

let () =
  Fun.protect
    ~finally:(fun () -> Sys.remove pid_file)
    (fun () ->
      Unix.putenv "VOX_FAKE_PID" pid_file;
      let q = query (Boolean true) in
      let dump = Buffer.create 128 in
      assert (
        validity (run ~dump:(fun s -> Buffer.add_string dump s) "unsat" q)
        = Valid);
      assert (
        Buffer.contents dump
        = to_smtlib ~int_width:63 ~timeout_ms:2000 q ^ "(exit)\n");
      let x = Symbol.create ~label:"x" Bv63 in
      let sat = query ~symbols:[x] (app Eq [Var x; integer 0]) in
      List.iter
        (fun mode ->
          assert (validity (run mode sat) = Invalid (Some [x, Int_value (-1L)])))
        ["sat"; "decimal-model"];
      List.iter
        (fun mode -> assert (validity (run mode sat) = Invalid None))
        ["model-error"; "unparsed-model"];
      assert (validity (run "unknown" q) = Unknown (Some "incomplete"));
      assert (validity (run "solver-timeout" q) = Timeout);
      List.iter
        (fun mode ->
          if not (is_failure (validity (run mode sat)))
          then failwith ("Expected protocol failure: " ^ mode))
        [ "bad-status";
          "bad-model";
          "wrong-model-shape";
          "deep-model";
          "flat-model";
          "unsat-junk";
          "duplicate-status";
          "early-exit";
          "stdout-flood";
          "signal" ];
      let crash = run "crash" q in
      assert (is_failure crash.validity);
      assert (crash.stderr = "fake solver started\nsolver crashed\n");
      let flood = run "stderr-flood" q in
      assert (flood.validity = Valid && String.length flood.stderr = 100020);
      List.iter
        (fun mode ->
          let start = Unix.gettimeofday () in
          assert (validity (run ~timeout_ms:100 mode q) = Timeout);
          assert (Unix.gettimeofday () -. start < 2.))
        ["startup-hang"; "hang"; "unsat-hang"];
      let large =
        query
          ~symbols:(List.init 10000 (fun _ -> Symbol.create ~label:"x" Bv63))
          (Boolean true)
      in
      assert (validity (run ~timeout_ms:100 "startup-hang" large) = Timeout);
      assert (is_failure (validity (run "early-exit" large)));
      Unix.putenv "VOX_FAKE_MODE" "hang";
      let start = Unix.gettimeofday () in
      (match
         Vox_smt_solver.check ~config:(config 2000) ~int_width:63
           ~cancelled:(fun () -> Unix.gettimeofday () -. start > 0.1)
           q
       with
      | _ -> failwith "Expected cancellation"
      | exception Vox_smt_solver.Cancelled -> reaped ());
      let start = Unix.gettimeofday () in
      (match
         Vox_smt_solver.check ~config:(config 2000) ~int_width:63
           ~cancelled:(fun () ->
             if Unix.gettimeofday () -. start > 0.1 then raise Sys.Break;
             false)
           q
       with
      | _ -> failwith "Expected Sys.Break"
      | exception Sys.Break -> reaped ());
      let interrupted = ref false in
      let previous =
        Sys.signal Sys.sigint (Sys.Signal_handle (fun _ -> interrupted := true))
      in
      Fun.protect
        ~finally:(fun () -> Sys.set_signal Sys.sigint previous)
        (fun () ->
          Unix.putenv "VOX_FAKE_MODE" "interrupt";
          match
            Vox_smt_solver.check ~config:(config 2000) ~int_width:63
              ~cancelled:(fun () -> !interrupted)
              q
          with
          | _ -> failwith "Expected interrupt"
          | exception Vox_smt_solver.Cancelled -> reaped ());
      (match
         run
           ~dump:(fun s ->
             if String.starts_with ~prefix:"(exit)" s then raise Exit)
           "unsat" q
       with
      | _ -> failwith "Expected dump exception"
      | exception Exit -> reaped ());
      (match
         run
           ~dump:(fun s ->
             if String.starts_with ~prefix:"(exit)" s
             then raise (Unix.Unix_error (Unix.EPIPE, "callback", "")))
           "unsat" q
       with
      | _ -> failwith "Expected callback Unix error"
      | exception Unix.Unix_error (Unix.EPIPE, "callback", "") -> reaped ());
      let available_fd () =
        let fd = Unix.dup Unix.stdin in
        Unix.close fd;
        fd
      in
      let before = available_fd () in
      for _ = 1 to 20 do
        assert (validity (run "unsat" q) = Valid)
      done;
      assert (before = available_fd ());
      let executable = Filename.temp_file "vox solver ;" ".exe" in
      Sys.remove executable;
      Unix.symlink fake executable;
      Fun.protect
        ~finally:(fun () -> Sys.remove executable)
        (fun () ->
          let config = { Vox_smt_solver.executable; timeout_ms = 2000 } in
          assert (
            validity (Vox_smt_solver.check ~config ~int_width:63 q) = Valid);
          reaped ());
      let missing =
        { Vox_smt_solver.executable = pid_file ^ ".missing"; timeout_ms = 100 }
      in
      assert (
        is_failure
          (validity (Vox_smt_solver.check ~config:missing ~int_width:63 q)));
      (match
         Vox_smt_solver.check ~config:missing ~int_width:63 (query (integer 0))
       with
      | _ -> failwith "Sort checking must precede process startup"
      | exception Sort_error _ -> ());
      print_endline "SMT interface tests passed")
