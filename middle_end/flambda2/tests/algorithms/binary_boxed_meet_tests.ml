open Flambda2_bound_identifiers
open Flambda2_numbers
module Or_bottom = Flambda2_lattices.Or_bottom
module T = Flambda2_types
module TE = T.Typing_env
module Mode = Alloc_mode.For_types
module Ints = Numeric_types.Int64.Set

let boxed values mode =
  T.box_int64 (T.these_naked_int64s (Ints.of_list values)) mode

let () =
  Clflags.flambda_invariant_checks := Clflags.Heavy_checks;
  let cu = Compilation_unit.of_string "Binary_boxed_meet_tests" in
  Env.set_current_unit
    (Unit_info.make_dummy ~input_name:"Binary_boxed_meet_tests" cu);
  let env = TE.create ~machine_width:Sixty_four ~resolver:(fun _ -> None) in
  let inputs = [[1L]; [1L; 2L]; [2L; 3L]; [4L]] in
  let modes = [Mode.heap; Mode.unknown ()] in
  List.iter
    (fun left_values ->
      List.iter
        (fun right_values ->
          List.iter
            (fun left_mode ->
              List.iter
                (fun right_mode ->
                  let left = boxed left_values left_mode in
                  let right = boxed right_values right_mode in
                  let intersection =
                    List.filter (fun n -> List.mem n right_values) left_values
                  in
                  match T.meet env left right with
                  | Or_bottom.Bottom -> assert (intersection = [])
                  | Or_bottom.Ok (result, result_env) ->
                    assert (intersection <> []);
                    let expected_mode =
                      if
                        Mode.equal left_mode Mode.heap
                        || Mode.equal right_mode Mode.heap
                      then Mode.heap
                      else Mode.unknown ()
                    in
                    let expected = boxed intersection expected_mode in
                    assert (
                      T.Equal_types_for_debug.equal_type result_env result
                        expected);
                    if
                      intersection = left_values
                      && Mode.equal expected_mode left_mode
                    then assert (result == left)
                    else if
                      intersection = right_values
                      && Mode.equal expected_mode right_mode
                    then assert (result == right))
                modes)
            modes)
        inputs)
    inputs
