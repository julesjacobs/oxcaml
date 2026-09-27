module B = Wasm_u32
module F = Wasm_functions
module T = Wasm_control
module I = Wasm_instruction
module E = Hmc_wasm_program_emit
module A = Hmc_wasm_program_functions
module R = Wasm_global_registers
module S = Hmc_wasm_structured_block
module Capture = Hmc_wasm_cons_capture
module Slots = Hmc_wasm_descriptor_load
let[@def] (local_types @ total) (unit : unit) =
  F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32
    (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32
      (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 F.No_locals)))))))))))))))))
let[@def] (config @ total) (table_base : B.u32) (stack_base : B.u32) =
  {A.locals = {E.structured = {S.frame = 0; heap = 1; limit = 2; object_ = 3;
      scratch = {Capture.head_tag = 14; head_payload = 15; tail_tag = 16; tail_payload = 17}};
    top = 4; stack_limit = 5; code = 6; address = 7;
    descriptor = {Slots.start = 8; captures = 9; recursive = 10}; status = 11; result_tag = 12; result_payload = 13};
    local_types = local_types ();
    loads = R.Binding (0, 0, R.Binding (1, 1, R.Binding (2, 2, R.Binding (3, 4,
      R.Binding (4, 5, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End))))))));
    stores = R.Binding (1, 1, R.Binding (3, 4, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End)))));
    table_base; stack_base}
let[@def] (dispatch_body @ total) (unit : unit) =
  T.Instruction (I.Global_get 0,
    T.Instruction (I.I32_load (0, 8),
      T.Instruction (I.Call_indirect 0,
        T.Instruction (I.Global_get 5,
          T.Instruction (I.Plain I.I32_eqz,
            T.Instruction (I.Br_if 0, T.Empty))))))
let[@def] (loop_code @ total) (unit : unit) = T.Loop (dispatch_body (), T.Instruction (I.Global_get 5, T.Empty))
(* The host passes the input in the exported global [payload] (7). The
   prologue stores it as the payload of the entry frame's first environment
   cell, 56 bytes above the frame base in global 0, and then resets [payload]
   to the payload of [Nil], the initial accumulator. *)
let[@def] (input_offset @ total) (unit : unit) : B.u32 = 56
let[@def] (prologue_code @ total) (unit : unit) : Wasm_code.t @ immutable =
  Wasm_code.Next (I.Global_get 0,
    Wasm_code.Next (I.Global_get 7,
      Wasm_code.Next (I.I64_store (3, 56),
        Wasm_code.Next (I.I64_const {Hmc_word64.lo = 0; hi = 0},
          Wasm_code.Next (I.Global_set 7, Wasm_code.Empty)))))
let[@def] (prologue @ total) (code : T.code @ immutable) =
  T.Instruction (I.Global_get 0,
    T.Instruction (I.Global_get 7,
      T.Instruction (I.I64_store (3, 56),
        T.Instruction (I.I64_const {Hmc_word64.lo = 0; hi = 0},
          T.Instruction (I.Global_set 7, code)))))
let[@def] (dispatcher @ total) (unit : unit) =
  {F.result = F.I32; locals = F.No_locals; code = prologue (loop_code ())}
