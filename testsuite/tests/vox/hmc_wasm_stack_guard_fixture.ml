module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module T = Wasm_control
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module Exit = Hmc_wasm_allocation_exit
module Continue = Wasm_control_branch_continue
type fixture = {code : C.t; expected : B.u32}
let rec fuel n = if n = 0 then C.Zero else C.Succ (fuel (n - 1))
let fixture (cursor : B.u32) available =
  if cursor > 4294967247 then failwith "stack guard region" else
  let limit : B.u32 = if available then cursor + 48 else cursor in
  let capacity = if available then Hm_declarative.S Hm_declarative.Z else Hm_declarative.Z in
  let signature = {Hmc_cfg_ir.locals = Hm_declarative.Empty_context; temporaries = Hmc_cfg_ir.Empty_temporaries; accumulator = None} in
  let blocks = Hmc_cfg_ir.Add ({Hmc_cfg_ir.signature; instruction = Hmc_cfg_ir.Jump Hm_declarative.Z}, Hmc_cfg_ir.Empty) in
  let expected : B.u32 = if cursor + 48 <= limit then 10 else 20 in
  let locals = S.Push (S.I32 cursor, S.Push (S.I32 limit, S.Push (S.I32 20, S.Empty))) in
  let state = {X.memory = B.End; machine = {E.locals; stack = S.Empty}} in
  let trap = T.Instruction (I.Plain I.Unreachable, T.Empty) in
  let done_ = T.Instruction (I.Local_get 2, T.Empty) in
  let normal = T.Instruction (I.I32_const 10, T.Instruction (I.Local_set 2, T.Instruction (I.Br 2, T.Empty))) in
  let guarded = Exit.emit 48 0 1 C.Empty 3 normal in
  let loop_body = T.Block (guarded, trap) in
  let code = T.Block (T.Loop (loop_body, trap), done_) in
  let exit_label = T.Label ({T.restart = None; continuation = done_; saved = S.Empty}, T.No_labels) in
  let loop_label = T.Label ({T.restart = Some loop_body; continuation = trap; saved = S.Empty}, exit_label) in
  let inner_label = T.Label ({T.restart = None; continuation = trap; saved = S.Empty}, loop_label) in
  ghost_ (Wasm_locals.get_def locals 0; Wasm_locals.get_def locals 1;
    Wasm_locals.get_def (S.Push (S.I32 limit, S.Push (S.I32 20, S.Empty))) 0;
    X.run_def C.Empty state;
    Hmc_memory_saved_frame.slots_def blocks;
    Hmc_frame_capacity.capacity_def blocks; Hmc_frame_capacity.capacity_def Hmc_cfg_ir.Empty;
    Hmc_frame_codec.size_def signature; Hmc_frame_codec.locals_size_def Hm_declarative.Empty_context;
    Hmc_frame_codec.temporaries_size_def Hmc_cfg_ir.Empty_temporaries;
    Hm_declarative.add_def Hm_declarative.Z Hm_declarative.Z;
    Hmc_frame_capacity.max_def (Hm_declarative.S (Hm_declarative.S Hm_declarative.Z)) Hm_declarative.Z;
    Hmc_memory_stack.zero_def ();
    Hmc_heap_extent.span_def (Hm_declarative.S (Hm_declarative.S (Hm_declarative.S Hm_declarative.Z))) 0 48;
    Hmc_heap_extent.span_def (Hm_declarative.S (Hm_declarative.S Hm_declarative.Z)) 16 48;
    Hmc_heap_extent.span_def (Hm_declarative.S Hm_declarative.Z) 32 48;
    Hmc_heap_extent.span_def Hm_declarative.Z 48 48;
    Hmc_memory_stack_capacity.region_def 48 capacity cursor limit;
    Hmc_memory_stack.previous_def 48 limit;
    Hmc_memory_stack_capacity.region_def 48 Hm_declarative.Z cursor cursor;
    Hmc_memory_stack.related_def blocks 48 B.End cursor cursor Hmc_heap_state.Halt;
    Hmc_wasm_stack_guard.correct blocks 48 cursor cursor limit capacity Hmc_heap_state.Halt C.Empty 0 1 3 normal inner_label state state ();
    Continue.labels_def normal inner_label;
    T.branch_def 3 (Continue.labels normal inner_label) state;
    T.branch_def 2 inner_label state; T.branch_def 1 loop_label state; T.branch_def 0 exit_label state; T.stack_def state S.Empty);
  (match T.run (fuel 100) {T.code; labels = T.No_labels; state} with
  | T.Finished final ->
    if final.X.memory <> B.End || final.X.machine.E.stack <> S.Push (S.I32 expected, S.Empty)
    then failwith "allocation exit result"
  | _ -> failwith "allocation exit did not finish");
  let prefix = C.Next (I.I32_const cursor, C.Next (I.Local_set 0,
    C.Next (I.I32_const limit, C.Next (I.Local_set 1,
    C.Next (I.I32_const 20, C.Next (I.Local_set 2, C.Empty)))))) in
  {code = E.append prefix (T.flatten code C.Empty); expected}
let fixtures () = List.concat_map (fun cursor -> [fixture cursor true; fixture cursor false]) [0; 7; 512; 4294967247]
