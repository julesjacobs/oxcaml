(* Test all examples from fixpoint_solver.mli documentation *)

module IntValue = struct
  type t = int
  let equal = (=)
end

module UnitKey = struct
  type t = unit
  let equal () () = true
  let hash () = 0
end

module BoolKey = struct
  type t = bool
  let equal = (=)
  let hash = function true -> 1 | false -> 0
end

module IntKey = struct
  type t = int
  let equal = (=)
  let hash = Hashtbl.hash
end

(* Example 1: Simple convergence *)
let test_example1 () =
  Printf.printf "\n=== Example 1: Simple convergence ===\n";
  Printf.printf "f() := min(f() + 1, 10)     [initial: 0]\n\n";
  
  let module Solver = Fixpoint_solver.Make(UnitKey)(IntValue) in
  
  let priority_and_init () = (true, 0) in (* doesn't matter if HIGH or LOW for single key *)
  
  let iteration = ref 0 in
  let f_def f_rec () =
    incr iteration;
    let prev = f_rec () in
    let result = min (prev + 1) 10 in
    if !iteration <= 15 then
      Printf.printf "  Iteration %2d: f() = min(%d + 1, 10) = %d\n" !iteration prev result;
    result
  in
  
  let f = Solver.fix priority_and_init f_def in
  let result = f () in
  Printf.printf "\nFinal result: f() = %d\n" result;
  Printf.printf "Expected: f() = 10\n";
  assert (result = 10)

(* Example 2: Mutual recursion with different priorities *)
let test_example2 () =
  Printf.printf "\n=== Example 2: Mutual recursion with different priorities ===\n";
  Printf.printf "f(true) := f(false)          [initial: 0, HIGH]\n";
  Printf.printf "f(false) := f(true)          [initial: 1, LOW]\n\n";
  
  let module Solver = Fixpoint_solver.Make(BoolKey)(IntValue) in
  
  let priority_and_init b =
    match b with
    | true -> (true, 0)   (* HIGH priority, initial 0 *)
    | false -> (false, 1)  (* LOW priority, initial 1 *)
  in
  
  let iteration = ref 0 in
  let f_def f_rec b =
    incr iteration;
    let other = f_rec (not b) in
    if !iteration <= 10 then
      Printf.printf "  Iteration %2d: f(%5s) = f(%5s) = %d\n" 
        !iteration (string_of_bool b) (string_of_bool (not b)) other;
    other
  in
  
  let f = Solver.fix priority_and_init f_def in
  let result_true = f true in
  let result_false = f false in
  Printf.printf "\nFinal result: f(true) = %d, f(false) = %d\n" result_true result_false;
  Printf.printf "With HIGH reset: f(true) resets to 0 each outer iteration\n";
  Printf.printf "Expected: Both converge to 1 (LOW's initial value)\n"

(* Example 3: Same system with equal priorities *)
let test_example3 () =
  Printf.printf "\n=== Example 3: Same system with equal priorities ===\n";
  Printf.printf "f(true) := f(false)          [initial: 0, HIGH]\n";
  Printf.printf "f(false) := f(true)          [initial: 1, HIGH]\n\n";
  
  let module Solver = Fixpoint_solver.Make(BoolKey)(IntValue) in
  
  let priority_and_init b =
    match b with
    | true -> (true, 0)   (* HIGH priority, initial 0 *)
    | false -> (true, 1)  (* HIGH priority, initial 1 *)
  in
  
  let iteration = ref 0 in
  let f_def f_rec b =
    incr iteration;
    let other = f_rec (not b) in
    if !iteration <= 10 then
      Printf.printf "  Iteration %2d: f(%5s) = f(%5s) = %d\n" 
        !iteration (string_of_bool b) (string_of_bool (not b)) other;
    other
  in
  
  let f = Solver.fix priority_and_init f_def in
  let result_true = f true in
  let result_false = f false in
  Printf.printf "\nFinal result: f(true) = %d, f(false) = %d\n" result_true result_false;
  Printf.printf "With HIGH reset: Both HIGH values reset each iteration\n";
  Printf.printf "Expected: Depends on evaluation order within HIGH phase\n"

(* Example 4: Incrementing cycle *)
let test_example4 () =
  Printf.printf "\n=== Example 4: Incrementing cycle ===\n";
  Printf.printf "f(true) := min(f(false) + 1, 10)    [initial: 0]\n";
  Printf.printf "f(false) := f(true)                 [initial: 0]\n\n";
  
  let module Solver = Fixpoint_solver.Make(BoolKey)(IntValue) in
  
  let priority_and_init b = (true, 0) in (* both HIGH or both LOW *)
  
  let iteration = ref 0 in
  let f_def f_rec b =
    incr iteration;
    let result = match b with
    | true -> 
        let v = f_rec false in
        let r = min (v + 1) 10 in
        if !iteration <= 25 then
          Printf.printf "  Iteration %2d: f(true) = min(f(false) + 1, 10) = min(%d + 1, 10) = %d\n" 
            !iteration v r;
        r
    | false -> 
        let v = f_rec true in
        if !iteration <= 25 then
          Printf.printf "  Iteration %2d: f(false) = f(true) = %d\n" !iteration v;
        v
    in
    result
  in
  
  let f = Solver.fix priority_and_init f_def in
  let result_true = f true in
  let result_false = f false in
  Printf.printf "\nFinal result: f(true) = %d, f(false) = %d\n" result_true result_false;
  Printf.printf "Expected: Both stabilize at 10\n"

(* Example 5: Three-way cycle with mixed priorities *)
let test_example5 () =
  Printf.printf "\n=== Example 5: Three-way cycle with mixed priorities ===\n";
  Printf.printf "f(0) := min(f(1) + 1, 20)    [initial: 0, LOW]\n";
  Printf.printf "f(1) := f(2)                 [initial: 0, HIGH]\n";
  Printf.printf "f(2) := f(0)                 [initial: 10, LOW]\n\n";
  
  let module Solver = Fixpoint_solver.Make(IntKey)(IntValue) in
  
  let priority_and_init k =
    match k with
    | 0 -> (false, 0)   (* LOW, initial 0 *)
    | 1 -> (true, 0)    (* HIGH, initial 0 *)
    | 2 -> (false, 10)  (* LOW, initial 10 *)
    | _ -> failwith "unexpected key"
  in
  
  let iteration = ref 0 in
  let f_def f_rec k =
    incr iteration;
    let result = match k with
    | 0 -> 
        let v1 = f_rec 1 in
        let r = min (v1 + 1) 20 in
        if !iteration <= 30 then
          Printf.printf "  Iteration %2d: f(0) = min(f(1) + 1, 20) = min(%d + 1, 20) = %d\n" 
            !iteration v1 r;
        r
    | 1 -> 
        let v2 = f_rec 2 in
        if !iteration <= 30 then
          Printf.printf "  Iteration %2d: f(1) = f(2) = %d\n" !iteration v2;
        v2
    | 2 -> 
        let v0 = f_rec 0 in
        if !iteration <= 30 then
          Printf.printf "  Iteration %2d: f(2) = f(0) = %d\n" !iteration v0;
        v0
    | _ -> failwith "unexpected key"
    in
    result
  in
  
  let f = Solver.fix priority_and_init f_def in
  let result0 = f 0 in
  let result1 = f 1 in
  let result2 = f 2 in
  Printf.printf "\nFinal result: f(0) = %d, f(1) = %d, f(2) = %d\n" result0 result1 result2;
  Printf.printf "With HIGH reset: f(1) resets to 0 each outer iteration\n";
  Printf.printf "Expected: All reach fixpoint (likely all equal to some value)\n"

(* Additional test to clearly show HIGH reset behavior *)
let test_high_reset_clear () =
  Printf.printf "\n=== Clear HIGH Reset Example ===\n";
  Printf.printf "f(0) := f(1) + 1    [initial: 100, HIGH]\n";
  Printf.printf "f(1) := f(0) - 1    [initial: 50, LOW]\n";
  Printf.printf "This should show HIGH resetting to 100 each outer iteration\n\n";
  
  let module Solver = Fixpoint_solver.Make(IntKey)(IntValue) in
  
  let priority_and_init k =
    match k with
    | 0 -> (true, 100)   (* HIGH, initial 100 *)
    | 1 -> (false, 50)   (* LOW, initial 50 *)
    | _ -> failwith "unexpected key"
  in
  
  let iteration = ref 0 in
  let f_def f_rec k =
    incr iteration;
    let result = match k with
    | 0 -> 
        let v1 = f_rec 1 in
        let r = v1 + 1 in
        if !iteration <= 15 then
          Printf.printf "  Iteration %2d: f(0) = f(1) + 1 = %d + 1 = %d\n" !iteration v1 r;
        r
    | 1 -> 
        let v0 = f_rec 0 in
        let r = v0 - 1 in
        if !iteration <= 15 then
          Printf.printf "  Iteration %2d: f(1) = f(0) - 1 = %d - 1 = %d\n" !iteration v0 r;
        r
    | _ -> failwith "unexpected key"
    in
    result
  in
  
  let f = Solver.fix priority_and_init f_def in
  let result0 = f 0 in
  let result1 = f 1 in
  Printf.printf "\nFinal result: f(0) = %d, f(1) = %d\n" result0 result1;
  Printf.printf "Analysis:\n";
  Printf.printf "  1. HIGH phase: f(0) resets to 100, then f(0) = f(1) + 1 = 50 + 1 = 51\n";
  Printf.printf "  2. LOW phase: f(1) = f(0) - 1 = 51 - 1 = 50\n";
  Printf.printf "  3. Since f(1) didn't change (still 50), we're done\n";
  Printf.printf "  Expected: f(0) = 51, f(1) = 50\n"

let () =
  test_example1 ();
  test_example2 ();
  test_example3 ();
  test_example4 ();
  test_example5 ();
  test_high_reset_clear ();
  Printf.printf "\n=== All tests completed ===\n"