module B = Wasm_u32
module D = Hm_declarative
module R = Hmc_runtime_closures
module Descriptor = Hmc_runtime_descriptor
module Load = Hmc_wasm_descriptor_load
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module L = Wasm_locals
module I = Wasm_instruction
type fixture = {memory : B.bytes; descriptor : R.descriptor; code : Wasm_code.t; prefix : I.t list}
let rec fill n = if n = 0 then B.End else B.Byte (173, fill (n - 1))
let rec locals = function [] -> S.Empty | n :: rest -> S.Push (S.I32 n, locals rest)
let fixture (base : B.u32) (descriptor : R.descriptor) =
  if base > 7 then failwith "descriptor base" else
  let before = fill 512 in
  match Hmc_linear_bytes.drop before 512 with
  | None -> failwith "descriptor memory"
  | Some _ ->
    ghost_ (Hmc_linear_bounds.covers_def before 512; Descriptor.slots_def ();
      Hmc_u32_index.represents_def (D.S (D.S D.Z)) 2;
      Hmc_u32_index.represents_def (D.S D.Z) 1; Hmc_u32_index.represents_def D.Z 0);
    match Hmc_wasm_reservation.reserve (Descriptor.slots ()) 2 base 512 () with
    | None -> failwith "descriptor extent"
    | Some stop ->
      let memory = Descriptor.store before 512 base stop descriptor () in
      let slots = {Load.start = 1; captures = 2; recursive = 3} in
      let local_values = locals [base; 11; 12; 13; 4294967295] in
      let operand_stack = S.Push (S.I64 {Hmc_word64.lo = 37; hi = 43}, S.Empty) in
      let state = {X.memory; machine = {E.locals = local_values; stack = operand_stack}} in
      if not (Load.distinct slots 0 && Load.writable slots local_values) then failwith "descriptor locals" else
      match L.get local_values 0 with
      | Some (S.I32 actual) when actual = base ->
        let out = Load.correct descriptor state base 0 slots () in
        let expected = locals [base; descriptor.R.start; descriptor.R.captures; S.boolean descriptor.R.recursive; 4294967295] in
        if out <> expected || X.run (Load.emit 0 slots) state <> X.Done {X.memory; machine = {E.locals = expected; stack = operand_stack}}
        then failwith "descriptor execution";
        {memory; descriptor; code = Load.emit 0 slots;
         prefix = [I.I32_const base; I.Local_set 0; I.I32_const 11; I.Local_set 1; I.I32_const 12; I.Local_set 2; I.I32_const 13; I.Local_set 3]}
      | _ -> failwith "descriptor base local"
let fixtures () = List.concat_map (fun base -> List.concat_map (fun start -> List.concat_map (fun captures ->
  List.map (fun recursive -> fixture base {R.start; captures; recursive}) [false; true])
  [0; 4294967295]) [0; 4294967295]) [0; 7]
