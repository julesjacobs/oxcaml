open Vox_smt

let query ?(datatypes = []) ?(symbols = []) ?(functions = []) ?(facts = []) term
    =
  { datatypes; symbols; functions; facts; goal = { label = "goal"; term } }

let app op args = App (op, args)

let integer n = Integer (Int64.of_int n)

let serialize q = to_smtlib ~int_width:63 ~timeout_ms:5000 q

let sort_error q =
  match serialize q with
  | _ -> failwith "Expected sort error"
  | exception Sort_error _ -> ()

let () =
  let opaque = Symbol.create ~label:"map" (Opaque 1) in
  let x = Symbol.create ~label:"x" Int63 in
  let data = Datatype.create ~label:"data" in
  let record = Symbol.create ~label:"record" (Datatype data) in
  let b = Symbol.create ~label:"b" Bool in
  let n = Symbol.create ~label:"n" Int in
  let symbols = [opaque; x; record; b; n] in
  let interpret symbols text =
    Vox_smt_response.interpret_response ~resources:(ref None) symbols "sat" text
  in
  assert (
    interpret symbols
      "((v0 opaque!val!0) (v1 (- 1)) (v2 (C 42)) (v3 true) (v4 123))"
    = Invalid
        (Some [x, Int_value (-1L); b, Bool_value true; n, Bigint_value "123"]));
  assert (
    interpret [opaque; record] "((v0 opaque!val!0) (v1 (C 42)))"
    = Invalid (Some []));
  List.iter
    (fun text -> assert (interpret symbols text = Invalid None))
    [ "((v1 opaque!val!0) (v1 0) (v2 (C 42)) (v3 true) (v4 123))";
      "((v0 opaque!val!0) (v1 0) (v0 (C 42)) (v3 true) (v4 123))";
      "((v0 opaque!val!0) (v1 0) (v2 (C 42)) (v3 true))";
      "((v0 opaque!val!0) (v1 0) (v2 (C 42)) (v3 true) (v4 123) (v5 0))";
      "((v0 opaque!val!0) (v1 invalid) (v2 (C 42)) (v3 true) (v4 123))";
      "((v0 opaque!val!0) (v1 4611686018427387904) (v2 (C 42)) (v3 true) (v4 \
       123))";
      "((v0 opaque!val!0) (v1 0) (v2 (C 42)) (v3 invalid) (v4 123))";
      "((v0 opaque!val!0) (v1 0) (v2 (C 42)) (v3 true) (v4 invalid))" ]

let () =
  let x = Symbol.create ~label:"x" Int63 in
  let b = Symbol.create ~label:"b" Bool in
  let f = Function.create ~label:"f" ~arguments:[Int63] ~result:Bool in
  let n = integer 0 and p = Boolean true in
  List.iter
    (fun (term, expected) ->
      assert (term_sort term = expected);
      let witness =
        match expected with
        | Bool -> p
        | Int63 -> n
        | Int -> Big_integer "0"
        | Opaque _ -> assert false
        | Datatype _ -> assert false
      in
      check ~int_width:63
        (query ~symbols:[x; b] ~functions:[f] (app Eq [term; witness])))
    [ p, Bool;
      n, Int63;
      Var x, Int63;
      Var b, Bool;
      Call (f, [n]), Bool;
      app Add [n; n], Int63;
      app Sub [n; n], Int63;
      app Mul [n; n], Int63;
      app Div [n; n], Int63;
      app Rem [n; n], Int63;
      app Neg [n], Int63;
      app Bit_and [n; n], Int63;
      app Bit_or [n; n], Int63;
      app Bit_xor [n; n], Int63;
      app Shift_right_logical [n; n], Int63;
      app Eq [n; n], Bool;
      app Ne [p; p], Bool;
      app Lt [n; n], Bool;
      app Le [n; n], Bool;
      app Gt [n; n], Bool;
      app Ge [n; n], Bool;
      app Not [p], Bool;
      app And [p; p], Bool;
      app Or [p; p], Bool;
      app Implies [p; p], Bool;
      app Ite [p; app Ite [p; n; n]; n], Int63;
      app Ite [p; p; p], Bool ];
  sort_error (query (app Eq [Var x; n]));
  sort_error (query (Call (f, [n])))

let () =
  let z = Big_integer "123456789012345678901234567890" in
  List.iter
    (fun op -> check ~int_width:63 (query (app Eq [app op [z; z]; z])))
    [Int_add; Int_sub; Int_mul; Int_div; Int_mod];
  List.iter
    (fun op -> check ~int_width:63 (query (app op [z; z])))
    [Int_lt; Int_le; Int_gt; Int_ge];
  check ~int_width:63 (query (app Eq [app Int_neg [z]; z]));
  check ~int_width:63
    (query (app Eq [app Int_of_int63 [integer (-1)]; Big_integer "-1"]));
  List.iter
    (fun text -> sort_error (query (app Eq [Big_integer text; z])))
    [""; "-"; "+1"; "00"; "-0"; "1_000"; "1) (assert false)"];
  sort_error (query (app Eq [app Int_add [z; integer 1]; z]));
  sort_error (query (app Eq [app Add [z; z]; integer 0]));
  sort_error (query (app Eq [app Int_of_int63 [z]; z]));
  assert (
    List.mem "(set-logic ALL)"
      (String.split_on_char '\n' (serialize (query (app Eq [z; z])))))

let () =
  let x = Symbol.create ~label:"x" Int63 in
  let b = Symbol.create ~label:"x" Bool in
  let f =
    Function.create ~label:"v0) (assert false)" ~arguments:[Int63] ~result:Int63
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
       (set-option :rlimit 0)\n\
       (set-logic QF_UFLIA)\n\
       (declare-fun f0 (Int) Int)\n\
       (assert (and (<= (- 4611686018427387904) (f0 0)) (<= (f0 0) \
       4611686018427387903)))\n\
       (assert (not (= (f0 0) (f0 0))))\n\
       (check-sat)\n");
  let bad_arity =
    [ Add;
      Sub;
      Mul;
      Div;
      Rem;
      Bit_and;
      Bit_or;
      Bit_xor;
      Shift_right_logical;
      Neg;
      Eq;
      Ne;
      Lt;
      Le;
      Gt;
      Ge;
      Not;
      And;
      Or;
      Implies;
      Ite ]
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
    let a = Symbol.create ~label:"v0) (assert false) ;" Int63 in
    let b = Symbol.create ~label:"v0) (assert false) ;" Int63 in
    query ~symbols:[a; b] (app Eq [Var a; Var b])
  in
  let serialized = serialize (collision_query ()) in
  assert (serialized = serialize (collision_query ()));
  assert (
    serialized
    = "(set-option :print-success false)\n\
       (set-option :produce-models true)\n\
       (set-option :timeout 5000)\n\
       (set-option :rlimit 0)\n\
       (set-logic QF_LIA)\n\
       (declare-fun v0 () Int)\n\
       (declare-fun v1 () Int)\n\
       (assert (and (<= (- 4611686018427387904) v0) (<= v0 \
       4611686018427387903)))\n\
       (assert (and (<= (- 4611686018427387904) v1) (<= v1 \
       4611686018427387903)))\n\
       (assert (not (= v0 v1)))\n\
       (check-sat)\n");
  ignore
    (serialize
       (query ~symbols:[x; b]
          (app Eq [Var x; app Ite [Var b; integer 0; integer (-1)]])))

let () =
  let x = Symbol.create ~label:"x" Int63 in
  let product = app Mul [Var x; integer 2] in
  let serialized =
    serialize
      (query ~symbols:[x]
         ~facts:[{ label = "same product"; term = app Eq [product; product] }]
         (app Eq [product; product]))
  in
  assert (
    serialized
    = "(set-option :print-success false)\n\
       (set-option :produce-models true)\n\
       (set-option :timeout 5000)\n\
       (set-option :rlimit 0)\n\
       (set-logic QF_UFLIA)\n\
       (declare-fun int63_mul (Int Int) Int)\n\
       (declare-fun v0 () Int)\n\
       (assert (and (<= (- 4611686018427387904) v0) (<= v0 \
       4611686018427387903)))\n\
       (assert (and (<= (- 4611686018427387904) (int63_mul v0 2)) (<= \
       (int63_mul v0 2) 4611686018427387903)))\n\
       (assert (= (int63_mul v0 2) (- (mod (+ (* 2 v0) 4611686018427387904) \
       9223372036854775808) 4611686018427387904)))\n\
       (assert (= (int63_mul v0 2) (int63_mul v0 2)))\n\
       (assert (not (= (int63_mul v0 2) (int63_mul v0 2))))\n\
       (check-sat)\n")

let () =
  let a = Symbol.create ~label:"a" (Opaque 41) in
  let b = Symbol.create ~label:"b" (Opaque 41) in
  let other = Symbol.create ~label:"other" (Opaque 42) in
  assert (
    serialize (query ~symbols:[a; b] (app Eq [Var a; Var b]))
    = "(set-option :print-success false)\n\
       (set-option :produce-models true)\n\
       (set-option :timeout 5000)\n\
       (set-option :rlimit 0)\n\
       (set-logic ALL)\n\
       (declare-sort s0 0)\n\
       (declare-fun v0 () s0)\n\
       (declare-fun v1 () s0)\n\
       (assert (not (= v0 v1)))\n\
       (check-sat)\n");
  sort_error (query ~symbols:[a; other] (app Eq [Var a; Var other]))

let () =
  let tree = Datatype.create ~label:"tree" in
  let leaf = Constructor.create ~datatype:tree ~label:"Leaf" ["value", Int63] in
  let node =
    Constructor.create ~datatype:tree ~label:"Node"
      ["left", Datatype tree; "right", Datatype tree]
  in
  let declaration = { datatype = tree; constructors = [leaf; node] } in
  let one = Construct (leaf, [integer 1]) in
  let two = Construct (leaf, [integer 2]) in
  let pair = Construct (node, [one; two]) in
  check ~int_width:63
    (query ~datatypes:[declaration]
       (app And [Is (node, pair); app Eq [Select (node, 0, pair); one]]));
  sort_error (query (app Eq [one; one]));
  sort_error
    (query ~datatypes:[declaration] (app Eq [Construct (leaf, []); one]));
  (match term_sort (Select (leaf, -1, one)) with
  | _ -> failwith "Expected sort error"
  | exception Sort_error _ -> ());
  sort_error (query ~datatypes:[declaration] (Select (leaf, -1, one)));
  let actual =
    serialize
      (query ~datatypes:[declaration]
         (app Eq [Select (leaf, 0, one); integer 1]))
  in
  let expected =
    "(set-option :print-success false)\n\
     (set-option :produce-models true)\n\
     (set-option :timeout 5000)\n\
     (set-option :rlimit 0)\n\
     (set-logic ALL)\n\
     (declare-datatypes ((d0 0)) (((c0 (p0 Int)) (c1 (p1 d0) (p2 d0)))))\n\
     (assert (and (<= (- 4611686018427387904) (p0 (c0 1))) (<= (p0 (c0 1)) \
     4611686018427387903)))\n\
     (assert (not (= (p0 (c0 1)) 1)))\n\
     (check-sat)\n"
  in
  if actual <> expected then failwith actual

let () =
  let loop = Datatype.create ~label:"loop" in
  let loop_constructor =
    Constructor.create ~datatype:loop ~label:"Loop" ["rest", Datatype loop]
  in
  sort_error
    (query
       ~datatypes:[{ datatype = loop; constructors = [loop_constructor] }]
       (Boolean true))

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

let run ?(timeout_ms = 2000) ?dump ?assumptions mode q =
  Unix.putenv "VOX_FAKE_MODE" mode;
  let r =
    Vox_smt_solver.check ~config:(config timeout_ms) ?dump ?assumptions
      ~int_width:63 q
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
        = "(reset)\n"
          ^ String.concat "\n"
              (List.map
                 (fun line ->
                   if String.starts_with ~prefix:"(set-logic " line
                   then "(set-logic ALL)"
                   else line)
                 (String.split_on_char '\n'
                    (to_smtlib ~int_width:63 ~timeout_ms:2000 q)))
          ^ "(get-info :rlimit)\n(echo \"vox-query-done\")\n");
      let p = Symbol.create ~label:"assumption" Bool in
      let assumption_query = query ~symbols:[p] (Var p) in
      let core_result =
        run ~assumptions:[p] "assumption-core" assumption_query
      in
      assert (core_result.validity = Valid);
      assert (core_result.core = Some [p]);
      assert (
        Vox_smt_response.followup ~assumptions:[p] [p] "unsat"
        = "(get-unsat-assumptions)\n");
      let unsat = run "unsat" q in
      assert (unsat.resources = Some 42);
      assert (validity (run "quoted-echo" q) = Valid);
      assert ((run "no-resources" q).resources = None);
      let x = Symbol.create ~label:"x" Int63 in
      let sat = query ~symbols:[x] (app Eq [Var x; integer 0]) in
      List.iter
        (fun mode ->
          assert (validity (run mode sat) = Invalid (Some [x, Int_value (-1L)])))
        ["sat"; "decimal-model"];
      List.iter
        (fun mode -> assert (validity (run mode sat) = Invalid None))
        [ "model-error";
          "positive-out-of-range";
          "negative-out-of-range";
          "unparsed-model" ];
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
          ~symbols:(List.init 10000 (fun _ -> Symbol.create ~label:"x" Int63))
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
             if String.starts_with ~prefix:"(get-info :rlimit)" s
             then raise Exit)
           "unsat" q
       with
      | _ -> failwith "Expected dump exception"
      | exception Exit -> reaped ());
      (match
         run
           ~dump:(fun s ->
             if String.starts_with ~prefix:"(get-info :rlimit)" s
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
      let read_pid () =
        let input = open_in pid_file in
        Fun.protect
          ~finally:(fun () -> close_in input)
          (fun () -> int_of_string (input_line input))
      in
      List.iter
        (fun mode ->
          let with_session = match mode with
            | `Reset -> Vox_smt_solver.with_session
            | `Incremental -> Vox_smt_solver.with_incremental_session
            | `Automatic -> Vox_smt_solver.with_automatic_session in
          Unix.putenv "VOX_FAKE_MODE" "unsat";
          with_session ~config:(config 2000) ~int_width:63 (fun check ->
              assert ((check q).validity = Valid);
              let pid = read_pid () in
              let second = check q in
              assert (second.validity = Valid && second.resources = Some 42);
              assert (read_pid () = pid));
          reaped ();
          List.iter
            (fun mode ->
              Unix.putenv "VOX_FAKE_MODE" mode;
              with_session ~config:(config 100) ~int_width:63 (fun check ->
                  let failed = check q in
                  assert (
                    failed.validity = Timeout || is_failure failed.validity);
                  reaped ();
                  Unix.putenv "VOX_FAKE_MODE" "unsat";
                  assert ((check q).validity = Valid));
              reaped ())
            ["unsat-hang"; "unsat-junk"];
          let cancelled = ref false in
          with_session ~config:(config 2000) ~int_width:63
            ~cancelled:(fun () -> !cancelled)
            (fun check ->
              assert ((check q).validity = Valid);
              cancelled := true;
              (match check q with
              | _ -> failwith "Expected cancellation of an idle session"
              | exception Vox_smt_solver.Cancelled -> reaped ());
              cancelled := false;
              assert ((check q).validity = Valid));
          reaped ())
        [`Reset; `Incremental; `Automatic];
      Unix.putenv "VOX_FAKE_MODE" "unsat";
      let escaped = ref None in
      Vox_smt_solver.with_incremental_session ~config:(config 2000)
        ~int_width:63 (fun check ->
          escaped := Some check;
          assert ((check q : Vox_smt_solver.result).validity = Valid);
          let first = read_pid () in
          for _ = 2 to 256 do
            let result = check q in
            assert (result.validity = Valid && result.resources = Some 42);
            assert (read_pid () = first)
          done;
          let result = check q in
          assert (result.validity = Valid && result.resources = Some 42);
          assert (read_pid () <> first));
      reaped ();
      Option.iter
        (fun check ->
          match check ?request_model:None ?resource_limit:None
              ?assumptions:None q with
          | exception Invalid_argument _ -> ()
          | _ -> failwith "Escaped incremental check remained usable")
        !escaped;
      List.iter
        (fun (mode, exhausted) ->
          Unix.putenv "VOX_FAKE_MODE" mode;
          Vox_smt_solver.with_incremental_session ~config:(config 2000)
            ~int_width:63 (fun check ->
              let result = check ~resource_limit:1 q in
              if exhausted
              then assert (result.validity = Unknown (Some "resource limit"))
              else assert (is_failure result.validity);
              reaped ();
              Unix.putenv "VOX_FAKE_MODE" "unsat";
              assert ((check q).validity = Valid));
          reaped ())
        ["setup-resource", true; "setup-error", false];
      Unix.putenv "VOX_FAKE_MODE" "second-bad-model";
      Vox_smt_solver.with_incremental_session ~config:(config 2000)
        ~int_width:63 (fun check ->
          assert ((check sat).validity = Valid);
          let result = check sat in
          assert (is_failure result.validity && result.resources = Some 42));
      reaped ();
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

let () =
  let calls = ref 0 in
  let q = query (app And [Boolean true; Boolean true]) in
  let poll () =
    incr calls;
    if !calls = 3 then raise Exit
  in
  match to_smtlib ~poll ~int_width:63 ~timeout_ms:5000 q with
  | _ -> failwith "Expected construction cancellation"
  | exception Exit -> assert (!calls = 3)

let () =
  let calls = ref 0 in
  let cancelled () =
    incr calls;
    if !calls = 3 then raise Exit;
    false
  in
  let config =
    { Vox_smt_solver.executable = "/no-such-vox-solver"; timeout_ms = 5000 }
  in
  match
    Vox_smt_solver.check ~config ~cancelled ~int_width:63
      (query (app And [Boolean true; Boolean true]))
  with
  | _ -> failwith "Expected cancellation before solver startup"
  | exception Exit -> assert (!calls = 3)

let () =
  let x = Symbol.create ~label:"x" Int63 in
  let q =
    query ~symbols:[x]
      ~facts:[{ label = "branch"; term = app Lt [Var x; integer 0] }]
      (app Ge [Var x; integer 0])
  in
  assert (
    explain_invalid q (Some [x, Int_value (-1L)])
    = "Goal (goal): (x >= 0)\n\
       Assumption (branch): (x < 0)\n\
       Model: x = -1\n\
       The solver found a model satisfying the assumptions and falsifying the \
       goal.");
  let f = Function.create ~label:"clamp" ~arguments:[Int63] ~result:Int63 in
  let q = query ~functions:[f] (app Ge [Call (f, [integer 0]); integer 0]) in
  assert (
    explain_invalid q None
    = "Goal (goal): (clamp(0) >= 0)\n\
       The solver did not return printable model values.\n\
       Opaque functions: clamp\n\
       A model of opaque calls may indicate missing facts, not a runtime \
       counterexample.\n\
       The solver found a model satisfying the assumptions and falsifying the \
       goal.")

let () =
  let left = Symbol.create ~label:"value" Int63 in
  let right = Symbol.create ~label:"value" Int63 in
  let q = query ~symbols:[left; right] (app Eq [Var left; Var right]) in
  assert (
    explain_invalid q (Some [left, Int_value 0L; right, Int_value 1L])
    = "Goal (goal): (value[v0] = value[v1])\n\
       Model: value[v0] = 0\n\
       Model: value[v1] = 1\n\
       The solver found a model satisfying the assumptions and falsifying the \
       goal.")

let () =
  let module Codec = Vox_smt_query_codec in
  let machine = Symbol.create ~label:"machine\nvalue" Int63 in
  let integer = Symbol.create ~label:"integer" Int in
  let boolean = Symbol.create ~label:"boolean" Bool in
  let opaque = Symbol.create ~label:"opaque" (Opaque (-7)) in
  let datatype = Datatype.create ~label:"node" in
  let leaf = Constructor.create ~datatype ~label:"leaf" [] in
  let node =
    Constructor.create ~datatype ~label:"node"
      ["value", Int63; "next", Datatype datatype]
  in
  let data = Symbol.create ~label:"data" (Datatype datatype) in
  let f =
    Function.create ~label:"function" ~arguments:[Int; Opaque (-7)] ~result:Int
  in
  let m = Var machine and i = Var integer and b = Var boolean in
  let term = function
    | ( Neg | Bit_and | Bit_or | Bit_xor | Add | Sub | Mul | Div | Rem
      | Shift_right_logical | Shift_left | Shift_right_arithmetic ) as op ->
      App (op, if op = Neg then [m] else [m; Integer 1L])
    | (Eq | Ne | Lt | Le | Gt | Ge) as op -> App (op, [m; Integer 1L])
    | Not -> App (Not, [b])
    | (And | Or | Implies) as op -> App (op, [b; Boolean false])
    | Ite -> App (Ite, [b; i; Big_integer "92233720368547758080"])
    | ( Int_add | Int_sub | Int_mul | Int_div | Int_mod | Int_lt | Int_le
      | Int_gt | Int_ge ) as op ->
      App (op, [i; Big_integer "2"])
    | Int_neg -> App (Int_neg, [i])
    | Int_of_int63 -> App (Int_of_int63, [m])
    | Int63_of_int -> App (Int63_of_int, [i])
  in
  let shared =
    App
      ( Eq,
        [ Construct (node, [m; Construct (leaf, [])]);
          Construct (node, [m; Construct (leaf, [])]) ] )
  in
  let terms =
    shared :: shared
    :: Call (f, [i; Var opaque])
    :: Is (node, Var data)
    :: Select (node, 0, Var data)
    :: List.map term (Array.to_list Codec.ops)
  in
  let query =
    { datatypes = [{ datatype; constructors = [leaf; node] }];
      symbols = [machine; integer; boolean; opaque; data];
      functions = [f];
      facts =
        List.map
          (fun term -> { label = "a\000fact"; term = App (Eq, [term; term]) })
          terms;
      goal = { label = "goal"; term = b }
    }
  in
  let call =
    { Codec.int_width = 63;
      timeout_ms = 5000;
      resource_limit = Some 123;
      assumptions = Some [boolean];
      request_model = true;
      query
    }
  in
  let file = Filename.temp_file "vox-codec" ".vapi" in
  Fun.protect
    ~finally:(fun () -> Sys.remove file)
    (fun () ->
      Out_channel.with_open_bin file (fun out ->
          Codec.write_call out call;
          Codec.write_call out
            { call with assumptions = None; resource_limit = None;
              request_model = false });
      In_channel.with_open_bin file (fun input ->
          let read () =
            match Codec.read_call input with
            | Some call -> call
            | None -> failwith "Missing call"
          in
          let decoded = read () in
          let text (call : Codec.call) =
            to_smtlib ~int_width:call.Codec.int_width
              ~timeout_ms:call.timeout_ms ?resource_limit:call.resource_limit
              ?assumptions:call.assumptions call.query
          in
          assert (text call = text decoded && decoded.request_model);
          (match decoded.query.facts with
          | { term = App (Eq, [first; _]); _ }
            :: { term = App (Eq, [second; _]); _ }
            :: _ ->
            assert (first == second)
          | _ -> failwith "Missing shared terms");
          let second = read () in
          assert (second.assumptions = None && second.resource_limit = None);
          assert (not second.request_model);
          assert (Codec.read_call input = None));
      let malformed text =
        Out_channel.with_open_bin file (fun output -> output_string output text);
        match In_channel.with_open_bin file Codec.read_call with
        | _ -> failwith "Malformed query capture accepted"
        | exception (Invalid_argument _ | End_of_file | Failure _) -> ()
      in
      let header = "vox-api-query-v1\n63\n5000\n-1\n" in
      List.iter malformed
        [ "unknown-codec\n";
          "vox-api-query-v1\n-1\n";
          "vox-api-query-v1\n63\n5000\n0\n";
          header ^ "0\n0\n0\n-1\n1\n3\n0\n";
          header ^ "0\n0\n0\n-1\n1\n4\n18\n1\n0\n";
          header ^ "0\n0\n0\n-1\n1\n4\n999\n";
          header ^ "0\n0\n0\n-1\n1\n0\n2\n";
          header ^ "0\n1\n1\nx999\n" ];
      Out_channel.with_open_bin file (fun output ->
          Codec.write_call output call);
      let complete = In_channel.with_open_bin file In_channel.input_all in
      malformed (String.sub complete 0 (String.length complete / 2)));
  print_endline "SMT query codec tests passed"

let () =
  let module Codec = Vox_smt_query_codec in
  let file = Filename.temp_file "vox-local-codec" ".vapi" in
  Fun.protect
    ~finally:(fun () -> Sys.remove file)
    (fun () ->
      let symbol = Symbol.create ~label:"b" Bool in
      let datatype = Datatype.create ~label:"data" in
      let constructor = Constructor.create ~datatype ~label:"data" [] in
      let function_ =
        Function.create ~label:"f" ~arguments:[Datatype datatype] ~result:Bool
      in
      let leaf = Var symbol in
      let args = [leaf; Boolean true] in
      let make args = App (And, Sys.opaque_identity args) in
      let first = make args and second = make args in
      assert (first != second);
      let fact = { label = "shared"; term = first } in
      let q =
        query ~symbols:[symbol] ~functions:[function_]
          ~datatypes:[{ datatype; constructors = [constructor] }]
          ~facts:[fact; fact; { label = "distinct"; term = second }]
          first
      in
      let call =
        { Codec.int_width = 63;
          timeout_ms = 5000;
          resource_limit = Some 10;
          assumptions = Some [symbol];
          request_model = true;
          query = q
        }
      in
      let read () =
        In_channel.with_open_bin file (fun input ->
            match Codec.read_call input with
            | Some call ->
              assert (Codec.read_call input = None);
              call
            | None -> failwith "Missing local query")
      in
      Out_channel.with_open_bin file (fun output ->
          Codec.write_call output call);
      let decoded = read () in
      let decoded_again = read () in
      let symbols =
        q.symbols @ decoded.query.symbols @ decoded_again.query.symbols
        @ [Symbol.create ~label:"fresh" Bool]
      in
      let fresh = Datatype.create ~label:"fresh" in
      let datatypes =
        q.datatypes @ decoded.query.datatypes @ decoded_again.query.datatypes
        @ [ { datatype = fresh;
              constructors =
                [Constructor.create ~datatype:fresh ~label:"fresh" []]
            } ]
      in
      let functions =
        q.functions @ decoded.query.functions @ decoded_again.query.functions
        @ [Function.create ~label:"fresh" ~arguments:[] ~result:Bool]
      in
      check ~int_width:63 (query ~datatypes ~symbols ~functions (Boolean true));
      assert (
        to_smtlib ~int_width:63 ~timeout_ms:5000 q
        = to_smtlib ~int_width:63 ~timeout_ms:5000 decoded.query);
      (match decoded.query.facts with
      | [a; b; c] -> (
        assert (a == b);
        assert (a.term == decoded.query.goal.term);
        assert (a.term != c.term && a.term = c.term);
        match a.term, c.term with
        | App (_, a), App (_, b) -> assert (a == b)
        | _ -> failwith "Changed term shape")
      | _ -> failwith "Changed fact list");
      let boolean () = Boolean (Sys.opaque_identity true) in
      let a = boolean () and b = boolean () in
      assert (a != b);
      let bool_facts =
        List.map (fun term -> { label = "boolean"; term }) [a; a; b]
      in
      Out_channel.with_open_bin file (fun output ->
          Codec.write_call output
            { call with query = query ~facts:bool_facts a });
      (match (read ()).query.facts with
      | [a; b; c] ->
        assert (a.term == b.term);
        assert (a.term != c.term && a.term = c.term)
      | _ -> failwith "Changed Boolean sharing");
      let opaque = Symbol.create ~label:"opaque" (Opaque 1) in
      let constants =
        { call with
          resource_limit = Some 1;
          assumptions = None;
          query = query ~symbols:[opaque] (Boolean true)
        }
      in
      Out_channel.with_open_bin file (fun output ->
          Codec.write_call output constants);
      let decoded = read () in
      assert (decoded.resource_limit = Some 1);
      assert (decoded.query.goal.term = Boolean true);
      (match decoded.query.symbols with
      | [symbol] -> assert (Symbol.sort symbol = Opaque 1)
      | _ -> failwith "Changed constant query");
      let graph =
        let open Codec.Local_graph in
        { root = Reference 0;
          nodes =
            [| Block
                 ( 0,
                   [| Immediate 63;
                      Immediate 5000;
                      Reference 1;
                      Immediate 0;
                      Reference 2
                   |] );
               Block (0, [| Immediate 1 |]);
               Block
                 ( 0,
                   [| Immediate 0;
                      Reference 3;
                      Immediate 0;
                      Immediate 0;
                      Reference 7
                   |] );
               Block (0, [| Reference 4; Immediate 0 |]);
               Block (0, [| Immediate 0; Reference 5; Reference 1 |]);
               Text "opaque";
               Text "goal";
               Block (0, [| Reference 6; Reference 1 |])
            |]
        }
      in
      let decoded = Codec.Local_query.decode graph in
      assert (decoded.resource_limit = Some 1);
      assert (decoded.query.goal.term = Boolean true);
      (match decoded.query.symbols with
      | [symbol] -> assert (Symbol.sort symbol = Opaque 1)
      | _ -> failwith "Changed shared constant query");
      let count = 100000 in
      let facts =
        List.init count (fun _ -> { label = "equal"; term = make args })
      in
      Out_channel.with_open_bin file (fun output ->
          Codec.write_call output { call with query = { q with facts } });
      let decoded = read () in
      assert (List.length decoded.query.facts = count);
      (match decoded.query.facts with
      | a :: b :: _ -> assert (a.term != b.term && a.term = b.term)
      | _ -> failwith "Missing repeated terms");
      let root = ref (Boolean true) in
      for _ = 1 to count do
        root := App (And, [!root; !root])
      done;
      Out_channel.with_open_bin file (fun output ->
          Codec.write_call output
            { call with assumptions = None; query = query !root });
      let decoded = read () in
      let rec depth n term =
        if n = 0
        then assert (term = Boolean true)
        else
          match term with
          | App (And, [a; b]) ->
            assert (a == b);
            depth (n - 1) a
          | _ -> failwith "Changed deep DAG"
      in
      depth count decoded.query.goal.term;
      Out_channel.with_open_bin file (fun output ->
          Codec.write_portable_call output call);
      assert (
        to_smtlib ~int_width:63 ~timeout_ms:5000 q
        = to_smtlib ~int_width:63 ~timeout_ms:5000 (read ()).query);
      let malformed write =
        Out_channel.with_open_bin file write;
        match read () with
        | _ -> failwith "Malformed local capture accepted"
        | exception (Invalid_argument _ | End_of_file | Failure _) -> ()
      in
      let header output =
        output_string output "vox-api-query-local-v2\n";
        output_string output Sys.ocaml_version;
        output_char output '\n'
      in
      let wrong value =
        malformed (fun output ->
            header output;
            Marshal.to_channel output value [])
      in
      wrong (63, 5000, None, None, ());
      wrong 1.;
      wrong 1n;
      malformed (fun output ->
          output_string output "vox-api-query-local-v2\nbad-version\n");
      let raw payload objects =
        malformed (fun output ->
            header output;
            output_binary_int output 0x8495a6be;
            output_binary_int output (String.length payload);
            output_binary_int output objects;
            output_binary_int output 0;
            output_binary_int output 0;
            output_string output payload)
      in
      raw "\x04\x01" 0;
      raw "\x90\x04\x01" 1;
      raw "\x19_j\000\000" 1;
      raw "\x40\x40" 0;
      raw "\x40" 100;
      raw "\x10" 0;
      Out_channel.with_open_bin file (fun output ->
          Codec.write_call output call);
      let complete = In_channel.with_open_bin file In_channel.input_all in
      malformed (fun output ->
          output_string output
            (String.sub complete 0 (String.length complete - 1))));
  print_endline "SMT local graph codec tests passed"

let () =
  let module Solver = Vox_smt_solver in
  let config = { Solver.executable = "ocaml"; timeout_ms = 5000 } in
  assert (Solver.version ~executable:"ocaml" = Some "Vox SMT 0.1");
  let p = Symbol.create ~label:"embedded premise" Bool in
  let q = Symbol.create ~label:"embedded conclusion" Bool in
  let query =
    { datatypes = []; symbols = [p; q]; functions = [];
      facts = [{ label = "implication";
                 term = App (Implies, [Var p; Var q]) }];
      goal = { label = "conclusion"; term = Var q } } in
  let check ?assumptions query =
    Solver.check ~config ?assumptions ~int_width:63 query in
  let proved = check ~assumptions:[p] query in
  assert (proved.validity = Valid && proved.core = Some [p]);
  (match (check query).validity with
  | Invalid (Some model) ->
    assert (List.assoc p model = Bool_value false);
    assert (List.assoc q model = Bool_value false)
  | _ -> failwith "Embedded countermodel missing");
  let limited = Solver.check ~config ~resource_limit:1 ~int_width:63 query in
  assert (limited.validity = Unknown (Some "resource limit"));
  assert (limited.resources = Some 1);
  let number = Symbol.create ~label:"number" Int63 in
  let negative = App (Lt, [Var number; Integer 0L]) in
  let complement =
    { query with symbols = [p; q; number];
      facts = [
        { label = "complement";
          term = App (Eq, [Var p; App (Not, [Var q])]) };
        { label = "negative"; term = App (Eq, [Var q; negative]) };
        { label = "bits";
          term = App (Eq,
            [App (Bit_and, [Var number; Integer 0L]); Integer 0L]) }];
      goal = { label = "false"; term = Boolean false } } in
  (match (check complement).validity with
  | Invalid (Some model) ->
    let value = match List.assoc number model with
      | Int_value value -> value
      | _ -> failwith "Embedded machine integer model missing" in
    assert (List.assoc q model = Bool_value (value < 0L));
    assert (List.assoc p model = Bool_value (value >= 0L))
  | _ -> failwith "Embedded complemented bitvector model missing");
  let cancel = ref true in
  Solver.with_session ~config ~cancelled:(fun () -> !cancel) ~int_width:63
    (fun check ->
      (match check ~assumptions:[p] query with
      | _ -> failwith "Embedded cancellation was ignored"
      | exception Solver.Cancelled -> ());
      cancel := false;
      assert ((check ~assumptions:[p] query).validity = Valid));
  let exception Callback in
  (match Solver.check ~config ~cancelled:(fun () -> raise Callback)
      ~int_width:63 query with
  | _ -> failwith "Embedded callback exception was swallowed"
  | exception Callback -> ());
  let dumped = Buffer.create 128 in
  let proved = Solver.check ~config ~assumptions:[p] ~int_width:63
      ~dump:(fun text -> Buffer.add_string dumped text) query in
  assert (proved.validity = Valid && Buffer.length dumped > 0);
  print_endline "Embedded solver integration tests passed"

let () =
  let rec shared depth =
    if depth = 0 then Boolean true
    else
      let child = shared (depth - 1) in
      app And [child; child]
  in
  let q = query (shared 30) in
  let polls = ref 0 in
  let poll () =
    incr polls;
    if !polls > 128 then failwith "Repeated shared-term validation"
  in
  check ~poll ~int_width:63 q;
  assert (!polls <= 128);
  let result =
    Vox_smt_embedded.check ~clock:(fun () -> 0.) ~resource_limit:100000
      ~timeout_ms:1000 ~int_width:63 q
  in
  assert (result.validity = Valid);
  let symbols = List.init 64 (fun _ -> Symbol.create ~label:"s" Bool) in
  let rec nested depth term =
    if depth = 0 then term else app Not [nested (depth - 1) term]
  in
  let facts = List.map (fun symbol ->
    { label = "fact"; term = nested 16 (Var symbol) }) symbols in
  let result =
    Vox_smt_embedded.check ~clock:(fun () -> 0.) ~resource_limit:1000000
      ~timeout_ms:1000 ~int_width:63
      (query ~symbols ~facts (Boolean true))
  in
  assert (result.validity = Valid);
  let first = shared 30 and second = shared 30 in
  let polls = ref 0 in
  let poll () =
    incr polls;
    if !polls > 4096 then failwith "Repeated shared-term comparison"
  in
  check ~poll ~int_width:63
    (query ~facts:[{ label = "first"; term = first }] second);
  let shared_int = app Add [integer 0; integer 1] in
  let facts = [{ label = "integer equality";
                 term = app Eq [shared_int; shared_int] }] in
  sort_error (query ~facts (app And [shared_int; Boolean true]));
  let exception Interrupted in
  let interrupted f =
    let polls = ref 0 in
    let poll () =
      incr polls;
      if !polls = 32 then raise Interrupted
    in
    match f poll with
    | _ -> failwith "Declaration validation did not poll"
    | exception Interrupted -> assert (!polls = 32)
  in
  let symbols = List.init 1000 (fun _ -> Symbol.create ~label:"s" Bool) in
  let functions =
    List.init 1000 (fun _ ->
      Function.create ~label:"f" ~arguments:[] ~result:Int)
  in
  let undeclared = Symbol.create ~label:"undeclared" Bool in
  let digits = String.make 1000 '1' in
  interrupted (fun poll -> decimal_integer ~poll digits);
  interrupted (fun poll ->
    check ~poll ~int_width:63
      (query (app Eq [Big_integer digits; Big_integer "0"])));
  List.iter
    (fun q -> interrupted (fun poll -> check ~poll ~int_width:63 q))
    [ query ~symbols (Var undeclared);
      query ~functions (Var undeclared) ];
  let datatype = Datatype.create ~label:"many constructors" in
  let constructors =
    List.init 1000 (fun _ ->
      Constructor.create ~datatype ~label:"c" [])
  in
  interrupted (fun poll ->
    check ~poll ~int_width:63
      (query ~datatypes:[{ datatype; constructors }] (Var undeclared)));
  let fields = List.init 1000 (fun _ -> "field", Bool) in
  let constructor = Constructor.create ~datatype ~label:"c" fields in
  interrupted (fun poll ->
    check ~poll ~int_width:63
      (query ~datatypes:[{ datatype; constructors = [constructor] }]
         (Var undeclared)));
  let datatypes = Array.init 64 (fun _ -> Datatype.create ~label:"chain") in
  let declarations =
    Array.to_list
      (Array.mapi
         (fun index datatype ->
           let fields =
             if index + 1 = Array.length datatypes then []
             else ["next", Datatype datatypes.(index + 1)]
           in
           { datatype;
             constructors = [Constructor.create ~datatype ~label:"c" fields]
           })
         datatypes)
  in
  assert (datatypes_well_founded declarations);
  interrupted (fun poll -> datatypes_well_founded ~poll declarations);
  interrupted (fun poll ->
    check ~poll ~int_width:63
      (query ~datatypes:declarations (Var undeclared)));
  print_endline "SMT typed validation sharing and polling tests passed"

let () =
  let rec shared depth term =
    if depth = 0 then term
    else let child = shared (depth - 1) term in app And [child; child]
  in
  let polls = ref 0 in
  let poll () =
    incr polls;
    if !polls > 128 then failwith "Repeated shared-term abstraction"
  in
  assert (abstract_bitwise ~poll (query (shared 30 (Boolean true))) = None);
  let symbol = Symbol.create ~label:"bits" Int63 in
  let unchanged = app Eq [Var symbol; integer 0] in
  List.iter
    (fun op ->
      let bitwise () = app op [Var symbol; integer 7] in
      let make left right =
        query ~symbols:[symbol]
          ~facts:[{ label = "unchanged"; term = unchanged }]
          (app Eq [app Add [left; right]; integer 0])
      in
      let child = bitwise () in
      match
        abstract_bitwise (make child child),
        abstract_bitwise (make (bitwise ()) (bitwise ()))
      with
      | Some shared, Some copied ->
        assert ((List.hd shared.facts).term == unchanged);
        (match shared.goal.term with
        | App (Eq, [App (Add, [left; right]); _]) ->
          assert (left == right)
        | _ -> failwith "Unexpected bitwise abstraction shape");
        let serialize = to_smtlib ~int_width:63 ~timeout_ms:1000 in
        assert (serialize shared = serialize copied)
      | _ -> failwith "Missing bitwise abstraction")
    [Bit_and; Bit_or; Bit_xor; Shift_right_logical];
  List.iter
    (fun op ->
      assert
        (abstract_bitwise
           (query ~symbols:[symbol]
              (app Eq [app op [Var symbol; integer 1]; integer 0]))
         = None))
    [Shift_left; Shift_right_arithmetic];
  let exception Interrupted in
  let interrupted q =
    let polls = ref 0 in
    match abstract_bitwise
            ~poll:(fun () -> incr polls; if !polls = 32 then raise Interrupted)
            q with
    | exception Interrupted -> assert (!polls = 32)
    | _ -> failwith "Bitwise abstraction did not poll"
  in
  interrupted (query (shared 30 (Boolean true)));
  let bitwise = app Bit_and [Var symbol; integer 7] in
  interrupted
    (query ~symbols:[symbol]
       (app And [app Eq [bitwise; integer 0]; shared 30 unchanged]));
  print_endline "SMT bitwise abstraction traversal tests passed"

let () =
  let x = Symbol.create ~label:"x" Int63 in
  List.iter (fun op ->
    let term = App (op, [Var x; Integer 1L]) in
    let q = query ~symbols:[x] (App (Eq, [term; Integer 0L])) in
    assert ((check_features ~int_width:63 q).bitvector_encoding);
    assert (Option.is_some (abstract_bitwise q)
            = List.mem op [Bit_and; Bit_or; Bit_xor; Shift_right_logical]))
    [Bit_and; Bit_or; Bit_xor; Shift_right_logical; Shift_left;
     Shift_right_arithmetic];
  List.iter (fun op ->
    let q = query ~symbols:[x]
        (App (Eq, [App (op, [Var x; Integer 1L]); Integer 0L])) in
    assert (not (check_features ~int_width:63 q).bitvector_encoding))
    [Add; Sub; Mul; Div; Rem];
  let shifted = App (Shift_left, [Var x; Integer 1L]) in
  let q = query ~symbols:[x]
      ~facts:[{label="shift";term=App (Eq, [shifted; Integer 0L])}]
      (Boolean true) in
  assert ((check_features ~int_width:63 q).bitvector_encoding);
  (match check_features ~int_width:63
           (query (App (Eq, [App (Bit_and, [Boolean true; Integer 1L]);
                             Integer 0L]))) with
   | exception Sort_error _ -> ()
   | _ -> failwith "Features bypassed sort checking");
  let rec shared depth term =
    if depth = 0 then term
    else let child = shared (depth - 1) term in App (And, [child; child]) in
  let q = query ~symbols:[x]
      (shared 30 (App (Eq, [shifted; Integer 0L]))) in
  let count f =
    let polls = ref 0 in
    let poll () =
      incr polls;
      if !polls > 128 then failwith "Repeated feature traversal" in
    f poll;
    !polls in
  let features_polls = count (fun poll ->
    assert ((check_features ~poll ~int_width:63 q).bitvector_encoding)) in
  let check_polls = count (fun poll -> check ~poll ~int_width:63 q) in
  assert (features_polls = check_polls);
  let exception Interrupted in
  (match check_features ~poll:(fun () -> raise Interrupted) ~int_width:63 q with
   | exception Interrupted -> ()
   | _ -> failwith "Feature checking did not poll");
  let undeclared = Symbol.create ~label:"undeclared" Bool in
  (match Vox_smt_embedded.check ~clock:(fun () -> 0.) ~timeout_ms:1000
           ~int_width:63 ~assumptions:[undeclared] (query (Boolean true)) with
   | exception Invalid_argument _ -> ()
   | _ -> failwith "Undeclared assumption was accepted");
  print_endline "SMT encoding feature validation tests passed"

let () =
  List.iter
    (fun op ->
      let count = 513 in
      let symbols = List.init count (fun _ -> Symbol.create ~label:"p" Bool) in
      let goal =
        List.fold_left (fun tail p -> app op [tail; Var p])
          (Boolean (op = And)) symbols
      in
      let facts =
        List.map
          (fun p ->
            { label = "fact";
              term = if op = And then Var p else app Not [Var p]
            })
          symbols
      in
      let q =
        query ~symbols ~facts (if op = And then goal else app Not [goal])
      in
      let check resource_limit q =
        Vox_smt_embedded.check ~clock:(fun () -> 0.) ~resource_limit
          ~timeout_ms:1000 ~int_width:63 q
      in
      assert ((check 40000 q).validity = Valid);
      let q = { q with facts = List.rev q.facts } in
      assert ((check 40000 q).validity = Valid);
      let q = { q with facts = List.tl q.facts } in
      match (check 80000 q).validity with
      | Invalid (Some model) ->
        assert (List.length model = count);
        List.iteri
          (fun index symbol ->
            let expected = if index + 1 = count then op <> And else op = And in
            assert (List.assoc symbol model = Bool_value expected))
          symbols
      | _ -> failwith "Missing nested Boolean countermodel")
    [And; Or];
  print_endline "SMT nested Boolean encoding tests passed"

let () =
  let p = Symbol.create ~label:"p" Bool in
  let q = Symbol.create ~label:"q" Bool in
  let n = Symbol.create ~label:"n" Int63 in
  let facts =
    List.init 513 (fun _ -> { label = "padding"; term = Boolean true })
    @
    List.map (fun term -> { label = "fact"; term })
      [app Implies [Var p; Var q];
       app Eq [Var n; integer 7];
       app Eq [app Mul [Var n; integer 2]; integer 14]]
  in
  List.iter
    (fun goal ->
      let query = query ~symbols:[p; q; n] ~facts goal in
      let check ?resource_limit ?cancelled () =
        Vox_smt_embedded.check ?resource_limit ?cancelled
          ~clock:(fun () -> 0.) ~timeout_ms:5000 ~int_width:63
          ~assumptions:[p] query
      in
      let validate (result : Vox_smt_embedded.result) =
        match goal, result.validity with
        | Var _, Valid -> assert (result.core = Some [p])
        | Boolean false, Invalid (Some model) ->
          assert (List.assoc p model = Bool_value true);
          assert (List.assoc q model = Bool_value true);
          assert (List.assoc n model = Int_value 7L)
        | _ -> failwith "Interrupted embedded check changed its result"
      in
      let callbacks = ref 0 in
      let result = check ~cancelled:(fun () -> incr callbacks; false) () in
      validate result;
      let used = Option.get result.resources in
      for budget = 1 to used do
        let result = check ~resource_limit:budget () in
        assert (Option.get result.resources <= budget);
        match result.validity with
        | Unknown (Some "resource limit") -> ()
        | _ -> validate result
      done;
      List.iter
        (fun raises ->
          for stop = 1 to !callbacks do
            let calls = ref 0 in
            let exception Callback in
            (match check ~cancelled:(fun () ->
               incr calls;
               if !calls = stop then
                 if raises then raise Callback else true
               else false) () with
            | exception Callback when raises -> ()
            | exception Vox_smt_embedded.Cancelled when not raises -> ()
            | _ -> failwith "Embedded ingestion interruption was ignored");
            validate (check ())
          done)
        [false; true];
      validate (check ()))
    [Var q; Boolean false];
  print_endline "SMT embedded operation interruption tests passed"

module Solver = Vox_smt_solver
module Embedded = Vox_smt_embedded
module Codec = Vox_smt_query_codec
let ensure condition message = if not condition then failwith message
let contains source text =
  let rec loop offset =
    offset + String.length text <= String.length source
    && (String.sub source offset (String.length text) = text || loop (offset + 1))
  in loop 0
let () =
  let x = Symbol.create ~label:"x" Int in
  let p = Symbol.create ~label:"p" Bool in
  let q = { datatypes = []; symbols = [x; p]; functions = [];
    facts = [{ label = "x"; term = App (Eq, [Var x; Big_integer "7"]) }];
    goal = { label = "goal"; term = Boolean false } } in
  let expected_model = function
    | Invalid (Some model) ->
      ensure (List.assoc x model = Bigint_value "7") "integer model";
      ensure (List.assoc p model = Bool_value true) "assumption model"
    | _ -> failwith "requested model unavailable" in
  List.iter (fun executable ->
    let config = { Solver.executable; timeout_ms = 5000 } in
    List.iter (fun mode ->
      let dumped = Buffer.create 128 in
      let with_session = match mode with
        | 0 -> Solver.with_session | 1 -> Solver.with_incremental_session
        | _ -> Solver.with_automatic_session in
      with_session ~config ~dump:(fun text -> Buffer.add_string dumped text) ~int_width:63
        (fun check ->
          List.iter (fun request_model ->
            Buffer.clear dumped;
            let result = check ?request_model:(Some request_model) ~assumptions:[p] q in
            if request_model then expected_model result.Solver.validity else
              ensure (result.validity = Invalid None) "unrequested model";
            if executable <> "ocaml" then
              ensure (contains (Buffer.contents dumped) "(get-value "
                      = request_model) "external request did not match option")
            [false; true; false; true];
          expected_model (check ~assumptions:[p] q).validity;
          let result = check ?request_model:(Some false) ~assumptions:[p]
              { q with goal = { label = "p"; term = Var p } } in
          ensure (result.validity = Valid && result.core = Some [p])
            "unrequested model suppressed assumption core";
          ensure ((check ?request_model:(Some false)
            { q with symbols = []; facts = [] }).validity = Invalid None)
            "empty unrequested model"))
      [0; 1; 2];
    expected_model (Solver.check ~config ~int_width:63 ~assumptions:[p] q).validity;
    ensure ((Solver.check ~config ~request_model:false ~int_width:63
      ~assumptions:[p] q).validity = Invalid None) "fresh unrequested model")
    ["ocaml"; "z3"];
  let run ?request_model ?resource_limit ?cancelled ?(clock = fun () -> 0.) () =
    Embedded.check ?request_model ?resource_limit ?cancelled ~clock
      ~timeout_ms:5000 ~int_width:63 ~assumptions:[p] q in
  let full = run () and skipped = run ~request_model:false () in
  expected_model full.validity;
  ensure (skipped.validity = Invalid None) "embedded skip";
  let used = Option.get skipped.resources in
  ensure (used < Option.get full.resources) "model budget was not omitted";
  for budget = 1 to used do
    let result = run ~request_model:false ~resource_limit:budget () in
    ensure (Option.get result.resources <= budget) "exceeded operation budget";
    match result.validity with
    | Unknown (Some "resource limit") | Invalid None -> ()
    | _ -> failwith "incorrect budgeted unrequested model"
  done;
  ensure ((run ~request_model:false ~resource_limit:used ()).validity
    = Invalid None) "exact complete-operation budget";
  ensure ((run ~resource_limit:used ()).validity
    = Unknown (Some "resource limit")) "model budget not charged";
  let callbacks = ref 0 in
  ignore (run ~request_model:false
    ~cancelled:(fun () -> incr callbacks; false) ());
  List.iter (fun raises ->
    for stop = 1 to !callbacks do
      let count = ref 0 in
      (match run ~request_model:false ~cancelled:(fun () ->
        incr count;
        if !count = stop && raises then raise Exit;
        !count = stop) () with
      | exception Exit when raises -> ()
      | exception Embedded.Cancelled when not raises -> ()
      | _ -> failwith "skipped model ignored cancellation");
      expected_model (run ()).validity
    done) [false; true];
  let time = ref 0. in
  ensure ((run ~request_model:false
    ~clock:(fun () -> time := !time +. 3.; !time) ()).validity = Timeout)
    "skipped model ignored deadline";
  let file = Filename.temp_file "vox-model-request" ".vapi" in
  Fun.protect ~finally:(fun () -> Sys.remove file) (fun () ->
    let call = { Codec.int_width = 63; timeout_ms = 5000;
      resource_limit = None; assumptions = Some [p]; query = q;
      request_model = false } in
    let read () = In_channel.with_open_bin file (fun input ->
      match Codec.read_call input with
      | Some call -> ensure (Codec.read_call input = None) "codec trailing"; call
      | None -> failwith "codec empty") in
    List.iter (fun write ->
      Out_channel.with_open_bin file (fun output -> write output call);
      ensure (not (read ()).request_model) "codec lost model request")
      [Codec.write_call; Codec.write_portable_call];
    Out_channel.with_open_bin file (fun output ->
      output_string output "vox-api-query-local-v2\n";
      output_string output Sys.ocaml_version;
      output_char output '\n';
      Marshal.to_channel output
        (call.int_width, call.timeout_ms, call.resource_limit,
         call.assumptions, call.query) []);
    ensure (read ()).request_model "legacy local model request";
    Out_channel.with_open_bin file (fun output ->
      output_string output "vox-api-query-v1\n63\n5000\n-1\n";
      output_string output "0\n0\n0\n-1\n1\n0\n0\n0\n4\ngoal0\n");
    ensure (read ()).request_model "legacy portable model request";
    Out_channel.with_open_bin file (fun output ->
      output_string output "vox-api-query-local-v3\n";
      output_string output Sys.ocaml_version;
      output_char output '\n';
      Marshal.to_channel output
        (call.int_width, call.timeout_ms, call.resource_limit,
         call.assumptions, call.query, 2) []);
    (match read () with
    | exception Invalid_argument _ -> ()
    | _ -> failwith "invalid request Boolean accepted");
    Out_channel.with_open_bin file (fun output ->
      output_string output "vox-api-query-v2\n63\n5000\n-1\n2\n");
    (match read () with
    | exception Invalid_argument _ -> ()
    | _ -> failwith "invalid portable request Boolean accepted"));
  Printf.printf "model requests: both backends, %d budgets, %d callbacks passed\n%!"
    used (2 * !callbacks)

let () =
  let check q =
    Vox_smt_embedded.check ~clock:(fun () -> 0.) ~timeout_ms:60000
      ~int_width:63 q
  in
  let values =
    [-4611686018427387904L; -4611686018427387903L; -2147483648L;
     -1L; 0L; 1L; 2147483647L; 4611686018427387902L;
     4611686018427387903L]
  in
  List.iter (fun value ->
    let converted = app Int_of_int63 [Integer value] in
    let expected = Big_integer (Int64.to_string value) in
    assert ((check (query (app Eq [converted; expected]))).validity = Valid);
    List.iter (fun bitwise ->
      let symbol = Symbol.create ~label:"value" Int63 in
      let term = if bitwise then app Bit_xor [Integer value; Integer 0L]
        else Integer value in
      let q = query ~symbols:[symbol]
          ~facts:[{label = "fixed"; term = app Eq [Var symbol; term]}]
          (Boolean false) in
      match (check q).validity with
      | Invalid (Some model) ->
        assert (List.assoc symbol model = Int_value value)
      | _ -> failwith "Missing exact integer boundary model") [false; true])
    values;
  List.iter (fun value ->
    match check (query (app Eq [Integer value; Integer 0L])) with
    | exception Sort_error _ -> ()
    | _ -> failwith "Out-of-range integer accepted")
    [Int64.min_int; Int64.max_int; -4611686018427387905L;
     4611686018427387904L];
  let symbols = List.init 128 (fun _ -> Symbol.create ~label:"same" Bool) in
  let facts = List.mapi (fun i symbol ->
    {label = "fixed";
     term = app Eq [Var symbol; Boolean (i mod 2 = 0)]}) symbols in
  (match (check (query ~symbols ~facts (Boolean false))).validity with
  | Invalid (Some model) ->
    List.iteri (fun i symbol ->
      assert (List.assoc symbol model = Bool_value (i mod 2 = 0))) symbols
  | _ -> failwith "Missing model for distinct symbol identities");
  print_endline "Embedded leaf boundaries and identities passed"

let () =
  let unused =
    [Symbol.create ~label:"unused-bool" Bool, Bool_value false;
     Symbol.create ~label:"unused-int" Int, Bigint_value "0";
     Symbol.create ~label:"unused-word" Int63, Int_value 0L] in
  let premise = Symbol.create ~label:"premise" Bool in
  let number = Symbol.create ~label:"number" Int in
  let word = Symbol.create ~label:"word" Int63 in
  let answer = Symbol.create ~label:"answer" Int in
  let datatype = Datatype.create ~label:"record" in
  let constructor = Constructor.create ~datatype ~label:"record"
      ["flag", Bool; "number", Int] in
  let record = Symbol.create ~label:"record" (Datatype datatype) in
  let function_ = Function.create ~label:"read" ~arguments:[Datatype datatype]
      ~result:Int in
  let equality left right =
    {label = "definition"; term = app Eq [left; right]} in
  let q = query
      ~symbols:(List.map fst unused @ [premise; number; word; answer; record])
      ~datatypes:[{datatype; constructors = [constructor]}]
      ~functions:[function_]
      ~facts:[equality (Var number) (Big_integer "7");
        equality (Var word) (Integer (-42L));
        equality (Var record)
          (Construct (constructor, [Var premise; Var number]));
        equality (Var answer) (Call (function_, [Var record]));
        equality (Call (function_, [Var record]))
          (app Int_add [Select (constructor, 1, Var record); Big_integer "3"])]
      (Boolean false) in
  let check ?(request_model = true) ?resource_limit q =
    Embedded.check ~request_model ?resource_limit ~assumptions:[premise]
      ~clock:(fun () -> 0.) ~timeout_ms:1000 ~int_width:63 q in
  let expected = unused @ [premise, Bool_value true;
      number, Bigint_value "7"; word, Int_value (-42L);
      answer, Bigint_value "10"] in
  List.iter (fun bitwise ->
    let q = if not bitwise then q else
        {q with facts = equality
            (app Bit_and [Integer 0L; Integer 0L]) (Integer 0L) :: q.facts} in
    let complete = check q in
    (match complete.validity with
    | Invalid (Some model) ->
      assert (List.length model = List.length expected);
      List.iter (fun (symbol, value) ->
        assert (List.assoc symbol model = value)) expected
    | _ -> failwith "Incomplete embedded declaration model");
    assert ((check ~request_model:false q).validity = Invalid None);
    let used = Option.get complete.resources in
    for budget = 1 to used - 1 do
      let result = check ~resource_limit:budget q in
      assert (result.validity = Unknown (Some "resource limit"));
      assert (result.core = None && result.resources = Some budget)
    done;
    assert ((check ~resource_limit:used q).validity = complete.validity);
    let proved = check {q with goal =
        {label = "assumption"; term = Var premise}} in
    assert (proved.validity = Valid && proved.core = Some [premise]))
    [false; true];
  print_endline "Embedded declaration models and budgets passed"

let () =
  let datatype = Datatype.create ~label:"ordering" in
  let constructor = Constructor.create ~datatype ~label:"ordering"
      ["number", Int63] in
  let symbol = Symbol.create ~label:"ordering" Int63 in
  let record = Symbol.create ~label:"ordering" (Datatype datatype) in
  let function_ = Function.create ~label:"ordering" ~arguments:[Int63]
      ~result:Int63 in
  let state = Random.State.make [|131; 63|] in
  let choose xs = List.nth xs (Random.State.int state (List.length xs)) in
  let rec number depth =
    if depth = 0 then choose [Integer (-1L); Integer 0L; Integer 1L; Var symbol]
    else match Random.State.int state 5 with
      | 0 -> number 0
      | 1 -> App (Add, [number (depth - 1); number (depth - 1)])
      | 2 -> App (Mul, [number (depth - 1); number (depth - 1)])
      | 3 -> Call (function_, [number (depth - 1)])
      | _ -> Select (constructor, 0, Var record) in
  let sample () =
    List.init (Random.State.int state 4) (fun _ ->
      choose [number 3; Boolean false; Boolean true; Big_integer "0";
        Construct (constructor, [number 2]); Is (constructor, Var record)]) in
  let sign n = Int.compare n 0 in
  for _ = 1 to 3000 do
    let left = sample () and right = sample () in
    assert (sign (compare_operands ~poll:(fun () -> ()) left right)
      = sign (Stdlib.compare left right))
  done;
  let rec shared depth =
    if depth = 0 then Integer 1L else
    let child = shared (depth - 1) in App (Add, [child; child]) in
  let left = [shared 200] and right = [shared 200] in
  assert (compare_operands ~poll:(fun () -> ()) left right = 0);
  let exception Interrupted in
  let calls = ref 0 in
  (match compare_operands ~poll:(fun () ->
      incr calls; if !calls = 100 then raise Interrupted) left right with
  | exception Interrupted -> assert (!calls = 100)
  | _ -> failwith "Uninterrupted shared operand comparison");
  let resource = Vox_smt_resource.create ~budget:1000 () in
  (match compare_operands ~poll:(fun () -> Vox_smt_resource.poll resource)
      left right with
  | exception Vox_smt_resource.Exhausted _ ->
    assert (Vox_smt_resource.used resource = 1000)
  | _ -> failwith "Operand comparison exceeded its resource budget");
  let text = String.make 10000 '1' in
  calls := 0;
  (match compare_operands ~poll:(fun () ->
      incr calls; if !calls = 100 then raise Interrupted)
      [Big_integer text]
      [Big_integer (String.init (String.length text) (String.get text))] with
  | exception Interrupted -> assert (!calls = 100)
  | _ -> failwith "Uninterrupted integer-text operand comparison");
  print_endline "SMT operand ordering and interruption tests passed"

let () =
  let check q = Embedded.check ~request_model:false ~clock:(fun () -> 0.)
      ~timeout_ms:10000 ~int_width:63 q in
  List.iter (fun count ->
    let symbols = List.init count (fun i ->
      Symbol.create ~label:(string_of_int i) Int63) in
    let facts = List.mapi (fun i symbol ->
      {label = "fixed"; term = app Eq [Var symbol; Integer (Int64.of_int i)]})
      symbols in
    let q = query ~symbols ~facts
        (app Eq [Var (List.hd (List.rev symbols));
          Integer (Int64.of_int (count - 1))]) in
    assert (not (check_features ~int_width:63 q).machine_multiplication);
    assert ((check q).validity = Valid)) [503; 512; 521];
  let x = Symbol.create ~label:"factor" Int63 in
  let zero = Integer 0L in
  let multiply factor = app Mul [factor; Var x] in
  let q = query ~symbols:[x]
      ~facts:[{label = "fixed"; term = app Eq [Var x; Integer 1L]};
        {label = "product"; term = app Eq [multiply zero; zero]}]
      (app Eq [multiply (app Add [zero; zero]); zero]) in
  assert ((check_features ~int_width:63 q).machine_multiplication);
  assert ((check q).validity = Valid);
  print_endline "SMT multiplication helper and streaming tests passed"
