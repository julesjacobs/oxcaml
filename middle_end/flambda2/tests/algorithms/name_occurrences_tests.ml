open Flambda2_identifiers
open Flambda2_kinds
open Flambda2_nominal
module N = Name_occurrences

let compilation_unit =
  Compilation_unit.create Compilation_unit.Prefix.empty
    (Compilation_unit.Name.of_string "Name_occurrences_tests")

let () =
  Current_unit.set
    (Unit_info.make_dummy ~input_name:"name_occurrences_tests.ml"
       compilation_unit)

let () =
  let variable = Variable.create "old" Flambda_kind.value in
  let fresh = Variable.create "fresh" Flambda_kind.value in
  let continuation = Continuation.create () in
  let target = Continuation.create () in
  let function_slot =
    Function_slot.create compilation_unit ~name:"function" ~size:2
  in
  let value_slot =
    Value_slot.create compilation_unit ~name:"value" ~is_always_immediate:false
      Flambda_kind.value
  in
  let adders =
    [| (fun mode t -> N.add_variable t variable mode);
       (fun _ t -> N.add_continuation t continuation ~has_traps:true);
       (fun mode t -> N.add_function_slot_in_projection t function_slot mode);
       (fun mode t -> N.add_function_slot_in_declaration t function_slot mode);
       (fun mode t -> N.add_value_slot_in_projection t value_slot mode);
       (fun mode t -> N.add_value_slot_in_declaration t value_slot mode)
    |]
  in
  let sample mode count mask =
    let result = ref N.empty in
    Array.iteri
      (fun bit add ->
        if mask land (1 lsl bit) <> 0
        then
          for _ = 1 to count do
            result := add mode !result
          done)
      adders;
    !result
  in
  let left = Array.init 64 (sample Name_mode.normal 1) in
  let right = Array.init 64 (sample Name_mode.phantom 3) in
  Array.iteri
    (fun i l ->
      Array.iteri
        (fun j r -> assert (N.subset_domain l r = (i land j = i)))
        right)
    left;
  let renaming =
    Renaming.add_continuation
      (Renaming.add_variable Renaming.empty variable fresh)
      continuation target
  in
  List.iter
    (fun mode ->
      for mask = 0 to 63 do
        let before = sample mode 3 mask in
        let after = N.apply_renaming before renaming in
        assert (
          N.equal
            (N.restrict_to_value_slots_and_function_slots before)
            (N.restrict_to_value_slots_and_function_slots after));
        assert (N.mem_var after fresh = (mask land 1 <> 0));
        assert (not (N.mem_var after variable));
        assert (N.mem_continuation after target = (mask land 2 <> 0));
        assert (not (N.mem_continuation after continuation));
        assert (
          N.continuation_is_applied_with_traps after target = (mask land 2 <> 0))
      done)
    [Name_mode.normal; Name_mode.in_types; Name_mode.phantom]
