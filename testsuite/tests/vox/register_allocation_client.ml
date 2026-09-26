(* TEST
 has-z3;
 flags = "-extension refinement_types";
 all_modules = "register_allocation_spec.ml register_allocation.mli register_allocation.ml register_allocation_client.ml";
 { bytecode; }
*)

open Register_allocation_spec
open Register_allocation

let (explicit_adapter_preservation @ total) :
  (program : program) -> (physical : int) ->
  (args : int list) -> (fuel : fuel) ->
  {u : unit |
    match allocate program physical with
    | None -> true
    | Some {code = target_code; physical = out_physical;
            source_registers; source_inputs; input_slots} ->
      not (same_shape program.inputs args)
      || observable_equal
           (advance program.code fuel (source_initial program args))
           (advance target_code fuel
              (target_initial out_physical source_registers
                 source_inputs input_slots args))}
  @ ghost =
  fun program physical args fuel -> ghost_ (
    preserves program physical args fuel;
    match allocate program physical with
    | None -> ()
    | Some allocation -> initial_of_allocation_def allocation args)

let (verified_run @ total) :
    (program : program) -> (physical : int) -> (args : int list) ->
    (fuel : fuel) ->
    {result : state option |
      match result with
      | None -> true
      | Some target ->
        not (same_shape program.inputs args)
        || observable_equal
             (advance program.code fuel (source_initial program args)) target} =
  fun program physical args fuel ->
  let allocation = allocate program physical in
  ghost_ (preserves program physical args fuel);
  match allocation with
  | None -> let result = None in result
  | Some allocation ->
    let target = advance allocation.code fuel
      (initial_of_allocation allocation args) in
    let result = Some target in
    result

let example =
  { registers = 7; inputs = [0]; code = [
      Move (1, Reg 0, 1);
      Move (2, Imm 0, 2);
      Binary (3, Less_than, Imm 0, Reg 1, 3);
      Branch (Reg 3, 4, 10);
      Binary (4, Equal, Reg 1, Imm 2, 5);
      Branch (Reg 4, 6, 7);
      Move (5, Reg 2, 8);
      Binary (5, Add, Reg 2, Reg 1, 8);
      Binary (1, Subtract, Reg 1, Imm 1, 9);
      Move (2, Reg 5, 2);
      Move (6, Reg 2, 11);
      Return (Reg 6)
    ] }
;;

let rec fuel n =
  if n <= 0 then Z else S (fuel (n - 1))
;;

let () =
  match allocate example 3 with
  | None -> Printf.printf "allocation failed\n"
  | Some allocation ->
    Printf.printf "physical=%d slots=%d\n"
      allocation.physical (List.length allocation.input_slots);
    List.iter (fun input ->
      let source = advance example.code (fuel 80) (source_initial example [input]) in
      let checked = verified_run example 3 [input] (fuel 80) in
      let target = match checked with
        | Some target -> target
        | None -> failwith "allocation failed" in
      Printf.printf "input=%d equal=%b result=%s\n" input (source = target)
        (match target with Done word -> string_of_int word | Running _ -> "running" | Stuck -> "stuck"))
      [0; 1; 2; 3; 4]
;;

let () =
  let dead_write = {
    registers = 2; inputs = [1];
    code = [Move (0, Imm 99, 1); Return (Reg 1)]
  } in
  let two_inputs = {
    registers = 2; inputs = [0; 1];
    code = [Binary (0, Add, Reg 0, Reg 1, 1); Return (Reg 0)]
  } in
  let duplicate_input = {
    registers = 1; inputs = [0; 0]; code = [Return (Reg 0)]
  } in
  let unused_input = {
    registers = 2; inputs = [0; 1]; code = [Return (Reg 0)]
  } in
  let forever = {registers = 1; inputs = []; code = [Jump 0]} in
  let dead_write_one = allocate dead_write 1 in
  let two_inputs_one = allocate two_inputs 1 in
  assert (dead_write_one = None && two_inputs_one = None);
  (match allocate dead_write 2 with
   | None -> assert false
   | Some allocation ->
     assert (advance allocation.code (fuel 2) (initial_of_allocation allocation [7]) = Done 7));
  (match allocate two_inputs 2 with
   | None -> assert false
   | Some allocation ->
     assert (advance allocation.code (fuel 2) (initial_of_allocation allocation [3; 4]) = Done 7));
  (match allocate duplicate_input 1 with
   | None -> assert false
   | Some allocation ->
     assert (advance allocation.code (fuel 1)
       (initial_of_allocation allocation [3; 5]) = Done 5));
  (match allocate unused_input 1 with
   | None -> assert false
   | Some allocation ->
     assert (advance allocation.code (fuel 1)
       (initial_of_allocation allocation [3; 99]) = Done 3));
  (match allocate forever 1 with
   | None -> assert false
   | Some allocation ->
     assert (observable_equal
       (advance forever.code (fuel 10) (source_initial forever []))
       (advance allocation.code (fuel 10) (initial_of_allocation allocation []))));
  let full_inputs = List.init 32 Fun.id in
  let maximum = {
    registers = 32; inputs = full_inputs;
    code = List.init 31 (fun i -> Binary (0, Add, Reg 0, Reg (i + 1), i + 1))
      @ List.init 32 (fun i -> Jump (i + 32)) @ [Return (Reg 0)]
  } in
  assert (allocate maximum 31 = None);
  assert (allocate maximum 33 = None);
  assert (allocate {maximum with code = Jump 0 :: maximum.code} 32 = None);
  assert (allocate {maximum with registers = 33} 32 = None);
  assert (allocate {registers = 1; inputs = []; code = [Return (Reg 0)]} 1
    = None);
  let result = verified_run maximum 32 full_inputs (fuel 64) in
  assert (result = Some (Done 496));
  Printf.printf "boundaries=passed\n"
;;
