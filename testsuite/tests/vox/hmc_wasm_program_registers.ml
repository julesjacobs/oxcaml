module B = Wasm_u32
module W = Hmc_word64
module S = Wasm_scalar
module L = Wasm_locals
module G = Wasm_globals
module E = Wasm_execution
module X = Wasm_memory_execution
module GE = Wasm_global_execution
module F = Wasm_functions
module C = Wasm_code
module I = Wasm_instruction
module R = Wasm_global_registers
module Transfer = Wasm_global_local_transfer
module Load = Wasm_register_load
module Replace = Wasm_local_replace
module Runtime = Hmc_wasm_program_runtime
module Assembly = Hmc_wasm_program_functions
type registers = {frame : B.u32; heap : B.u32; heap_limit : B.u32; top : B.u32; stack_limit : B.u32; status : B.u32; tag : W.t; payload : W.t}
let[@def] (values @ total) (registers : registers @ immutable) = S.Push (S.I32 registers.frame, S.Push (S.I32 registers.heap, S.Push (S.I32 registers.heap_limit, S.Push (S.I32 registers.top, S.Push (S.I32 registers.stack_limit, S.Push (S.I32 registers.status, S.Push (S.I64 registers.tag, S.Push (S.I64 registers.payload, S.Empty))))))))
let[@def] (locals @ total) (registers : registers @ immutable) = S.Push (S.I32 registers.frame, S.Push (S.I32 registers.heap, S.Push (S.I32 registers.heap_limit, S.Push (S.I32 0, S.Push (S.I32 registers.top, S.Push (S.I32 registers.stack_limit, S.Push (S.I32 0, S.Push (S.I32 0, S.Push (S.I32 0, S.Push (S.I32 0, S.Push (S.I32 0, S.Push (S.I32 registers.status, S.Push (S.I64 registers.tag, S.Push (S.I64 registers.payload, S.Push (S.I64 {W.lo = 0; hi = 0}, S.Push (S.I64 {W.lo = 0; hi = 0}, S.Push (S.I64 {W.lo = 0; hi = 0}, S.Push (S.I64 {W.lo = 0; hi = 0}, S.Empty))))))))))))))))))
let (import @ total) : (registers : registers) @ immutable -> (globals : G.t) @ immutable -> (memory : B.bytes) @ immutable ->
    (table_base : B.u32) -> (stack_base : B.u32) ->
    {u : unit | globals.G.values === values registers} ->
    {out : X.state | out.X.memory === memory && out.X.machine.E.stack === S.Empty && out.X.machine.E.locals === locals registers
      && GE.run (R.load_code (Runtime.config table_base stack_base).Assembly.loads)
        {GE.globals; execution = {X.memory; machine = {E.locals = F.zero_locals (Runtime.local_types ()); stack = S.Empty}}}
        === GE.Done {GE.globals; execution = out}} @ immutable =
  fun registers globals memory table_base stack_base premise ->
    ghost_ (
      values_def registers; locals_def registers; Runtime.config_def table_base stack_base; Runtime.local_types_def ();
      let g7 = S.Push (S.I64 registers.payload, S.Empty) in
      let g6 = S.Push (S.I64 registers.tag, g7) in
      let g5 = S.Push (S.I32 registers.status, g6) in
      let g4 = S.Push (S.I32 registers.stack_limit, g5) in
      let g3 = S.Push (S.I32 registers.top, g4) in
      let g2 = S.Push (S.I32 registers.heap_limit, g3) in
      let g1 = S.Push (S.I32 registers.heap, g2) in
      let g0 = S.Push (S.I32 registers.frame, g1) in
      G.get_def globals 0;
      L.get_def g0 0;
      G.get_def globals 1;
      L.get_def g0 1;
      L.get_def g1 0;
      G.get_def globals 2;
      L.get_def g0 2;
      L.get_def g1 1;
      L.get_def g2 0;
      G.get_def globals 3;
      L.get_def g0 3;
      L.get_def g1 2;
      L.get_def g2 1;
      L.get_def g3 0;
      G.get_def globals 4;
      L.get_def g0 4;
      L.get_def g1 3;
      L.get_def g2 2;
      L.get_def g3 1;
      L.get_def g4 0;
      G.get_def globals 5;
      L.get_def g0 5;
      L.get_def g1 4;
      L.get_def g2 3;
      L.get_def g3 2;
      L.get_def g4 1;
      L.get_def g5 0;
      G.get_def globals 6;
      L.get_def g0 6;
      L.get_def g1 5;
      L.get_def g2 4;
      L.get_def g3 3;
      L.get_def g4 2;
      L.get_def g5 1;
      L.get_def g6 0;
      G.get_def globals 7;
      L.get_def g0 7;
      L.get_def g1 6;
      L.get_def g2 5;
      L.get_def g3 4;
      L.get_def g4 3;
      L.get_def g5 2;
      L.get_def g6 1;
      L.get_def g7 0;
      let s0_17 = S.Push (S.I64 {W.lo = 0; hi = 0}, S.Empty) in
      let s0_16 = S.Push (S.I64 {W.lo = 0; hi = 0}, s0_17) in
      let s0_15 = S.Push (S.I64 {W.lo = 0; hi = 0}, s0_16) in
      let s0_14 = S.Push (S.I64 {W.lo = 0; hi = 0}, s0_15) in
      let s0_13 = S.Push (S.I64 {W.lo = 0; hi = 0}, s0_14) in
      let s0_12 = S.Push (S.I64 {W.lo = 0; hi = 0}, s0_13) in
      let s0_11 = S.Push (S.I32 0, s0_12) in
      let s0_10 = S.Push (S.I32 0, s0_11) in
      let s0_9 = S.Push (S.I32 0, s0_10) in
      let s0_8 = S.Push (S.I32 0, s0_9) in
      let s0_7 = S.Push (S.I32 0, s0_8) in
      let s0_6 = S.Push (S.I32 0, s0_7) in
      let s0_5 = S.Push (S.I32 0, s0_6) in
      let s0_4 = S.Push (S.I32 0, s0_5) in
      let s0_3 = S.Push (S.I32 0, s0_4) in
      let s0_2 = S.Push (S.I32 0, s0_3) in
      let s0_1 = S.Push (S.I32 0, s0_2) in
      let s0_0 = S.Push (S.I32 0, s0_1) in
      let state0 = {GE.globals; execution = {X.memory; machine = {E.locals = s0_0; stack = S.Empty}}} in
      let s1_17 = S.Push (S.I64 {W.lo = 0; hi = 0}, S.Empty) in
      let s1_16 = S.Push (S.I64 {W.lo = 0; hi = 0}, s1_17) in
      let s1_15 = S.Push (S.I64 {W.lo = 0; hi = 0}, s1_16) in
      let s1_14 = S.Push (S.I64 {W.lo = 0; hi = 0}, s1_15) in
      let s1_13 = S.Push (S.I64 {W.lo = 0; hi = 0}, s1_14) in
      let s1_12 = S.Push (S.I64 {W.lo = 0; hi = 0}, s1_13) in
      let s1_11 = S.Push (S.I32 0, s1_12) in
      let s1_10 = S.Push (S.I32 0, s1_11) in
      let s1_9 = S.Push (S.I32 0, s1_10) in
      let s1_8 = S.Push (S.I32 0, s1_9) in
      let s1_7 = S.Push (S.I32 0, s1_8) in
      let s1_6 = S.Push (S.I32 0, s1_7) in
      let s1_5 = S.Push (S.I32 0, s1_6) in
      let s1_4 = S.Push (S.I32 0, s1_5) in
      let s1_3 = S.Push (S.I32 0, s1_4) in
      let s1_2 = S.Push (S.I32 0, s1_3) in
      let s1_1 = S.Push (S.I32 0, s1_2) in
      let s1_0 = S.Push (S.I32 registers.frame, s1_1) in
      let state1 = {GE.globals; execution = {X.memory; machine = {E.locals = s1_0; stack = S.Empty}}} in
      let s2_17 = S.Push (S.I64 {W.lo = 0; hi = 0}, S.Empty) in
      let s2_16 = S.Push (S.I64 {W.lo = 0; hi = 0}, s2_17) in
      let s2_15 = S.Push (S.I64 {W.lo = 0; hi = 0}, s2_16) in
      let s2_14 = S.Push (S.I64 {W.lo = 0; hi = 0}, s2_15) in
      let s2_13 = S.Push (S.I64 {W.lo = 0; hi = 0}, s2_14) in
      let s2_12 = S.Push (S.I64 {W.lo = 0; hi = 0}, s2_13) in
      let s2_11 = S.Push (S.I32 0, s2_12) in
      let s2_10 = S.Push (S.I32 0, s2_11) in
      let s2_9 = S.Push (S.I32 0, s2_10) in
      let s2_8 = S.Push (S.I32 0, s2_9) in
      let s2_7 = S.Push (S.I32 0, s2_8) in
      let s2_6 = S.Push (S.I32 0, s2_7) in
      let s2_5 = S.Push (S.I32 0, s2_6) in
      let s2_4 = S.Push (S.I32 0, s2_5) in
      let s2_3 = S.Push (S.I32 0, s2_4) in
      let s2_2 = S.Push (S.I32 0, s2_3) in
      let s2_1 = S.Push (S.I32 registers.heap, s2_2) in
      let s2_0 = S.Push (S.I32 registers.frame, s2_1) in
      let state2 = {GE.globals; execution = {X.memory; machine = {E.locals = s2_0; stack = S.Empty}}} in
      let s3_17 = S.Push (S.I64 {W.lo = 0; hi = 0}, S.Empty) in
      let s3_16 = S.Push (S.I64 {W.lo = 0; hi = 0}, s3_17) in
      let s3_15 = S.Push (S.I64 {W.lo = 0; hi = 0}, s3_16) in
      let s3_14 = S.Push (S.I64 {W.lo = 0; hi = 0}, s3_15) in
      let s3_13 = S.Push (S.I64 {W.lo = 0; hi = 0}, s3_14) in
      let s3_12 = S.Push (S.I64 {W.lo = 0; hi = 0}, s3_13) in
      let s3_11 = S.Push (S.I32 0, s3_12) in
      let s3_10 = S.Push (S.I32 0, s3_11) in
      let s3_9 = S.Push (S.I32 0, s3_10) in
      let s3_8 = S.Push (S.I32 0, s3_9) in
      let s3_7 = S.Push (S.I32 0, s3_8) in
      let s3_6 = S.Push (S.I32 0, s3_7) in
      let s3_5 = S.Push (S.I32 0, s3_6) in
      let s3_4 = S.Push (S.I32 0, s3_5) in
      let s3_3 = S.Push (S.I32 0, s3_4) in
      let s3_2 = S.Push (S.I32 registers.heap_limit, s3_3) in
      let s3_1 = S.Push (S.I32 registers.heap, s3_2) in
      let s3_0 = S.Push (S.I32 registers.frame, s3_1) in
      let state3 = {GE.globals; execution = {X.memory; machine = {E.locals = s3_0; stack = S.Empty}}} in
      let s4_17 = S.Push (S.I64 {W.lo = 0; hi = 0}, S.Empty) in
      let s4_16 = S.Push (S.I64 {W.lo = 0; hi = 0}, s4_17) in
      let s4_15 = S.Push (S.I64 {W.lo = 0; hi = 0}, s4_16) in
      let s4_14 = S.Push (S.I64 {W.lo = 0; hi = 0}, s4_15) in
      let s4_13 = S.Push (S.I64 {W.lo = 0; hi = 0}, s4_14) in
      let s4_12 = S.Push (S.I64 {W.lo = 0; hi = 0}, s4_13) in
      let s4_11 = S.Push (S.I32 0, s4_12) in
      let s4_10 = S.Push (S.I32 0, s4_11) in
      let s4_9 = S.Push (S.I32 0, s4_10) in
      let s4_8 = S.Push (S.I32 0, s4_9) in
      let s4_7 = S.Push (S.I32 0, s4_8) in
      let s4_6 = S.Push (S.I32 0, s4_7) in
      let s4_5 = S.Push (S.I32 0, s4_6) in
      let s4_4 = S.Push (S.I32 registers.top, s4_5) in
      let s4_3 = S.Push (S.I32 0, s4_4) in
      let s4_2 = S.Push (S.I32 registers.heap_limit, s4_3) in
      let s4_1 = S.Push (S.I32 registers.heap, s4_2) in
      let s4_0 = S.Push (S.I32 registers.frame, s4_1) in
      let state4 = {GE.globals; execution = {X.memory; machine = {E.locals = s4_0; stack = S.Empty}}} in
      let s5_17 = S.Push (S.I64 {W.lo = 0; hi = 0}, S.Empty) in
      let s5_16 = S.Push (S.I64 {W.lo = 0; hi = 0}, s5_17) in
      let s5_15 = S.Push (S.I64 {W.lo = 0; hi = 0}, s5_16) in
      let s5_14 = S.Push (S.I64 {W.lo = 0; hi = 0}, s5_15) in
      let s5_13 = S.Push (S.I64 {W.lo = 0; hi = 0}, s5_14) in
      let s5_12 = S.Push (S.I64 {W.lo = 0; hi = 0}, s5_13) in
      let s5_11 = S.Push (S.I32 0, s5_12) in
      let s5_10 = S.Push (S.I32 0, s5_11) in
      let s5_9 = S.Push (S.I32 0, s5_10) in
      let s5_8 = S.Push (S.I32 0, s5_9) in
      let s5_7 = S.Push (S.I32 0, s5_8) in
      let s5_6 = S.Push (S.I32 0, s5_7) in
      let s5_5 = S.Push (S.I32 registers.stack_limit, s5_6) in
      let s5_4 = S.Push (S.I32 registers.top, s5_5) in
      let s5_3 = S.Push (S.I32 0, s5_4) in
      let s5_2 = S.Push (S.I32 registers.heap_limit, s5_3) in
      let s5_1 = S.Push (S.I32 registers.heap, s5_2) in
      let s5_0 = S.Push (S.I32 registers.frame, s5_1) in
      let state5 = {GE.globals; execution = {X.memory; machine = {E.locals = s5_0; stack = S.Empty}}} in
      let s6_17 = S.Push (S.I64 {W.lo = 0; hi = 0}, S.Empty) in
      let s6_16 = S.Push (S.I64 {W.lo = 0; hi = 0}, s6_17) in
      let s6_15 = S.Push (S.I64 {W.lo = 0; hi = 0}, s6_16) in
      let s6_14 = S.Push (S.I64 {W.lo = 0; hi = 0}, s6_15) in
      let s6_13 = S.Push (S.I64 {W.lo = 0; hi = 0}, s6_14) in
      let s6_12 = S.Push (S.I64 {W.lo = 0; hi = 0}, s6_13) in
      let s6_11 = S.Push (S.I32 registers.status, s6_12) in
      let s6_10 = S.Push (S.I32 0, s6_11) in
      let s6_9 = S.Push (S.I32 0, s6_10) in
      let s6_8 = S.Push (S.I32 0, s6_9) in
      let s6_7 = S.Push (S.I32 0, s6_8) in
      let s6_6 = S.Push (S.I32 0, s6_7) in
      let s6_5 = S.Push (S.I32 registers.stack_limit, s6_6) in
      let s6_4 = S.Push (S.I32 registers.top, s6_5) in
      let s6_3 = S.Push (S.I32 0, s6_4) in
      let s6_2 = S.Push (S.I32 registers.heap_limit, s6_3) in
      let s6_1 = S.Push (S.I32 registers.heap, s6_2) in
      let s6_0 = S.Push (S.I32 registers.frame, s6_1) in
      let state6 = {GE.globals; execution = {X.memory; machine = {E.locals = s6_0; stack = S.Empty}}} in
      let s7_17 = S.Push (S.I64 {W.lo = 0; hi = 0}, S.Empty) in
      let s7_16 = S.Push (S.I64 {W.lo = 0; hi = 0}, s7_17) in
      let s7_15 = S.Push (S.I64 {W.lo = 0; hi = 0}, s7_16) in
      let s7_14 = S.Push (S.I64 {W.lo = 0; hi = 0}, s7_15) in
      let s7_13 = S.Push (S.I64 {W.lo = 0; hi = 0}, s7_14) in
      let s7_12 = S.Push (S.I64 registers.tag, s7_13) in
      let s7_11 = S.Push (S.I32 registers.status, s7_12) in
      let s7_10 = S.Push (S.I32 0, s7_11) in
      let s7_9 = S.Push (S.I32 0, s7_10) in
      let s7_8 = S.Push (S.I32 0, s7_9) in
      let s7_7 = S.Push (S.I32 0, s7_8) in
      let s7_6 = S.Push (S.I32 0, s7_7) in
      let s7_5 = S.Push (S.I32 registers.stack_limit, s7_6) in
      let s7_4 = S.Push (S.I32 registers.top, s7_5) in
      let s7_3 = S.Push (S.I32 0, s7_4) in
      let s7_2 = S.Push (S.I32 registers.heap_limit, s7_3) in
      let s7_1 = S.Push (S.I32 registers.heap, s7_2) in
      let s7_0 = S.Push (S.I32 registers.frame, s7_1) in
      let state7 = {GE.globals; execution = {X.memory; machine = {E.locals = s7_0; stack = S.Empty}}} in
      let s8_17 = S.Push (S.I64 {W.lo = 0; hi = 0}, S.Empty) in
      let s8_16 = S.Push (S.I64 {W.lo = 0; hi = 0}, s8_17) in
      let s8_15 = S.Push (S.I64 {W.lo = 0; hi = 0}, s8_16) in
      let s8_14 = S.Push (S.I64 {W.lo = 0; hi = 0}, s8_15) in
      let s8_13 = S.Push (S.I64 registers.payload, s8_14) in
      let s8_12 = S.Push (S.I64 registers.tag, s8_13) in
      let s8_11 = S.Push (S.I32 registers.status, s8_12) in
      let s8_10 = S.Push (S.I32 0, s8_11) in
      let s8_9 = S.Push (S.I32 0, s8_10) in
      let s8_8 = S.Push (S.I32 0, s8_9) in
      let s8_7 = S.Push (S.I32 0, s8_8) in
      let s8_6 = S.Push (S.I32 0, s8_7) in
      let s8_5 = S.Push (S.I32 registers.stack_limit, s8_6) in
      let s8_4 = S.Push (S.I32 registers.top, s8_5) in
      let s8_3 = S.Push (S.I32 0, s8_4) in
      let s8_2 = S.Push (S.I32 registers.heap_limit, s8_3) in
      let s8_1 = S.Push (S.I32 registers.heap, s8_2) in
      let s8_0 = S.Push (S.I32 registers.frame, s8_1) in
      let state8 = {GE.globals; execution = {X.memory; machine = {E.locals = s8_0; stack = S.Empty}}} in
      F.zero_locals_def F.No_locals;
      F.zero_locals_def (F.Local64 (F.No_locals));
      F.zero_locals_def (F.Local64 (F.Local64 (F.No_locals)));
      F.zero_locals_def (F.Local64 (F.Local64 (F.Local64 (F.No_locals))));
      F.zero_locals_def (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.No_locals)))));
      F.zero_locals_def (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.No_locals))))));
      F.zero_locals_def (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.No_locals)))))));
      F.zero_locals_def (F.Local32 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.No_locals))))))));
      F.zero_locals_def (F.Local32 (F.Local32 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.No_locals)))))))));
      F.zero_locals_def (F.Local32 (F.Local32 (F.Local32 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.No_locals))))))))));
      F.zero_locals_def (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.No_locals)))))))))));
      F.zero_locals_def (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.No_locals))))))))))));
      F.zero_locals_def (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.No_locals)))))))))))));
      F.zero_locals_def (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.No_locals))))))))))))));
      F.zero_locals_def (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.No_locals)))))))))))))));
      F.zero_locals_def (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.No_locals))))))))))))))));
      F.zero_locals_def (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.No_locals)))))))))))))))));
      F.zero_locals_def (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.No_locals))))))))))))))))));
      F.zero_locals_def (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.Local64 (F.No_locals)))))))))))))))))));
      L.can_set_def s0_0 0 (S.I32 registers.frame);
      L.get_def s0_0 0; Replace.replace_def s0_0 0 (S.I32 registers.frame);
      S.same_type_def (S.I32 0) (S.I32 registers.frame);
      Load.correct 0 0 (S.I32 registers.frame) state0 ();
      R.load_code_def (R.Binding (0, 0, R.Binding (1, 1, R.Binding (2, 2, R.Binding (3, 4, R.Binding (4, 5, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End))))))))); Transfer.load_code_def 0 0;
      E.append_def (Transfer.load_code 0 0) (R.load_code (R.Binding (1, 1, R.Binding (2, 2, R.Binding (3, 4, R.Binding (4, 5, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End)))))))));
      E.append_def (C.Next (I.Local_set 0, C.Empty)) (R.load_code (R.Binding (1, 1, R.Binding (2, 2, R.Binding (3, 4, R.Binding (4, 5, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End)))))))));
      E.append_def C.Empty (R.load_code (R.Binding (1, 1, R.Binding (2, 2, R.Binding (3, 4, R.Binding (4, 5, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End)))))))));
      GE.append_correct (Transfer.load_code 0 0) (R.load_code (R.Binding (1, 1, R.Binding (2, 2, R.Binding (3, 4, R.Binding (4, 5, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End))))))))) state0;
      L.can_set_def s1_0 1 (S.I32 registers.heap);
      L.get_def s1_0 1; Replace.replace_def s1_0 1 (S.I32 registers.heap);
      L.get_def s1_1 0; Replace.replace_def s1_1 0 (S.I32 registers.heap);
      S.same_type_def (S.I32 0) (S.I32 registers.heap);
      Load.correct 1 1 (S.I32 registers.heap) state1 ();
      R.load_code_def (R.Binding (1, 1, R.Binding (2, 2, R.Binding (3, 4, R.Binding (4, 5, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End)))))))); Transfer.load_code_def 1 1;
      E.append_def (Transfer.load_code 1 1) (R.load_code (R.Binding (2, 2, R.Binding (3, 4, R.Binding (4, 5, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End))))))));
      E.append_def (C.Next (I.Local_set 1, C.Empty)) (R.load_code (R.Binding (2, 2, R.Binding (3, 4, R.Binding (4, 5, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End))))))));
      E.append_def C.Empty (R.load_code (R.Binding (2, 2, R.Binding (3, 4, R.Binding (4, 5, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End))))))));
      GE.append_correct (Transfer.load_code 1 1) (R.load_code (R.Binding (2, 2, R.Binding (3, 4, R.Binding (4, 5, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End)))))))) state1;
      L.can_set_def s2_0 2 (S.I32 registers.heap_limit);
      L.get_def s2_0 2; Replace.replace_def s2_0 2 (S.I32 registers.heap_limit);
      L.get_def s2_1 1; Replace.replace_def s2_1 1 (S.I32 registers.heap_limit);
      L.get_def s2_2 0; Replace.replace_def s2_2 0 (S.I32 registers.heap_limit);
      S.same_type_def (S.I32 0) (S.I32 registers.heap_limit);
      Load.correct 2 2 (S.I32 registers.heap_limit) state2 ();
      R.load_code_def (R.Binding (2, 2, R.Binding (3, 4, R.Binding (4, 5, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End))))))); Transfer.load_code_def 2 2;
      E.append_def (Transfer.load_code 2 2) (R.load_code (R.Binding (3, 4, R.Binding (4, 5, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End)))))));
      E.append_def (C.Next (I.Local_set 2, C.Empty)) (R.load_code (R.Binding (3, 4, R.Binding (4, 5, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End)))))));
      E.append_def C.Empty (R.load_code (R.Binding (3, 4, R.Binding (4, 5, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End)))))));
      GE.append_correct (Transfer.load_code 2 2) (R.load_code (R.Binding (3, 4, R.Binding (4, 5, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End))))))) state2;
      L.can_set_def s3_0 4 (S.I32 registers.top);
      L.get_def s3_0 4; Replace.replace_def s3_0 4 (S.I32 registers.top);
      L.get_def s3_1 3; Replace.replace_def s3_1 3 (S.I32 registers.top);
      L.get_def s3_2 2; Replace.replace_def s3_2 2 (S.I32 registers.top);
      L.get_def s3_3 1; Replace.replace_def s3_3 1 (S.I32 registers.top);
      L.get_def s3_4 0; Replace.replace_def s3_4 0 (S.I32 registers.top);
      S.same_type_def (S.I32 0) (S.I32 registers.top);
      Load.correct 3 4 (S.I32 registers.top) state3 ();
      R.load_code_def (R.Binding (3, 4, R.Binding (4, 5, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End)))))); Transfer.load_code_def 3 4;
      E.append_def (Transfer.load_code 3 4) (R.load_code (R.Binding (4, 5, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End))))));
      E.append_def (C.Next (I.Local_set 4, C.Empty)) (R.load_code (R.Binding (4, 5, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End))))));
      E.append_def C.Empty (R.load_code (R.Binding (4, 5, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End))))));
      GE.append_correct (Transfer.load_code 3 4) (R.load_code (R.Binding (4, 5, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End)))))) state3;
      L.can_set_def s4_0 5 (S.I32 registers.stack_limit);
      L.get_def s4_0 5; Replace.replace_def s4_0 5 (S.I32 registers.stack_limit);
      L.get_def s4_1 4; Replace.replace_def s4_1 4 (S.I32 registers.stack_limit);
      L.get_def s4_2 3; Replace.replace_def s4_2 3 (S.I32 registers.stack_limit);
      L.get_def s4_3 2; Replace.replace_def s4_3 2 (S.I32 registers.stack_limit);
      L.get_def s4_4 1; Replace.replace_def s4_4 1 (S.I32 registers.stack_limit);
      L.get_def s4_5 0; Replace.replace_def s4_5 0 (S.I32 registers.stack_limit);
      S.same_type_def (S.I32 0) (S.I32 registers.stack_limit);
      Load.correct 4 5 (S.I32 registers.stack_limit) state4 ();
      R.load_code_def (R.Binding (4, 5, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End))))); Transfer.load_code_def 4 5;
      E.append_def (Transfer.load_code 4 5) (R.load_code (R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End)))));
      E.append_def (C.Next (I.Local_set 5, C.Empty)) (R.load_code (R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End)))));
      E.append_def C.Empty (R.load_code (R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End)))));
      GE.append_correct (Transfer.load_code 4 5) (R.load_code (R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End))))) state4;
      L.can_set_def s5_0 11 (S.I32 registers.status);
      L.get_def s5_0 11; Replace.replace_def s5_0 11 (S.I32 registers.status);
      L.get_def s5_1 10; Replace.replace_def s5_1 10 (S.I32 registers.status);
      L.get_def s5_2 9; Replace.replace_def s5_2 9 (S.I32 registers.status);
      L.get_def s5_3 8; Replace.replace_def s5_3 8 (S.I32 registers.status);
      L.get_def s5_4 7; Replace.replace_def s5_4 7 (S.I32 registers.status);
      L.get_def s5_5 6; Replace.replace_def s5_5 6 (S.I32 registers.status);
      L.get_def s5_6 5; Replace.replace_def s5_6 5 (S.I32 registers.status);
      L.get_def s5_7 4; Replace.replace_def s5_7 4 (S.I32 registers.status);
      L.get_def s5_8 3; Replace.replace_def s5_8 3 (S.I32 registers.status);
      L.get_def s5_9 2; Replace.replace_def s5_9 2 (S.I32 registers.status);
      L.get_def s5_10 1; Replace.replace_def s5_10 1 (S.I32 registers.status);
      L.get_def s5_11 0; Replace.replace_def s5_11 0 (S.I32 registers.status);
      S.same_type_def (S.I32 0) (S.I32 registers.status);
      Load.correct 5 11 (S.I32 registers.status) state5 ();
      R.load_code_def (R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End)))); Transfer.load_code_def 5 11;
      E.append_def (Transfer.load_code 5 11) (R.load_code (R.Binding (6, 12, R.Binding (7, 13, R.End))));
      E.append_def (C.Next (I.Local_set 11, C.Empty)) (R.load_code (R.Binding (6, 12, R.Binding (7, 13, R.End))));
      E.append_def C.Empty (R.load_code (R.Binding (6, 12, R.Binding (7, 13, R.End))));
      GE.append_correct (Transfer.load_code 5 11) (R.load_code (R.Binding (6, 12, R.Binding (7, 13, R.End)))) state5;
      L.can_set_def s6_0 12 (S.I64 registers.tag);
      L.get_def s6_0 12; Replace.replace_def s6_0 12 (S.I64 registers.tag);
      L.get_def s6_1 11; Replace.replace_def s6_1 11 (S.I64 registers.tag);
      L.get_def s6_2 10; Replace.replace_def s6_2 10 (S.I64 registers.tag);
      L.get_def s6_3 9; Replace.replace_def s6_3 9 (S.I64 registers.tag);
      L.get_def s6_4 8; Replace.replace_def s6_4 8 (S.I64 registers.tag);
      L.get_def s6_5 7; Replace.replace_def s6_5 7 (S.I64 registers.tag);
      L.get_def s6_6 6; Replace.replace_def s6_6 6 (S.I64 registers.tag);
      L.get_def s6_7 5; Replace.replace_def s6_7 5 (S.I64 registers.tag);
      L.get_def s6_8 4; Replace.replace_def s6_8 4 (S.I64 registers.tag);
      L.get_def s6_9 3; Replace.replace_def s6_9 3 (S.I64 registers.tag);
      L.get_def s6_10 2; Replace.replace_def s6_10 2 (S.I64 registers.tag);
      L.get_def s6_11 1; Replace.replace_def s6_11 1 (S.I64 registers.tag);
      L.get_def s6_12 0; Replace.replace_def s6_12 0 (S.I64 registers.tag);
      S.same_type_def (S.I64 {W.lo = 0; hi = 0}) (S.I64 registers.tag);
      Load.correct 6 12 (S.I64 registers.tag) state6 ();
      R.load_code_def (R.Binding (6, 12, R.Binding (7, 13, R.End))); Transfer.load_code_def 6 12;
      E.append_def (Transfer.load_code 6 12) (R.load_code (R.Binding (7, 13, R.End)));
      E.append_def (C.Next (I.Local_set 12, C.Empty)) (R.load_code (R.Binding (7, 13, R.End)));
      E.append_def C.Empty (R.load_code (R.Binding (7, 13, R.End)));
      GE.append_correct (Transfer.load_code 6 12) (R.load_code (R.Binding (7, 13, R.End))) state6;
      L.can_set_def s7_0 13 (S.I64 registers.payload);
      L.get_def s7_0 13; Replace.replace_def s7_0 13 (S.I64 registers.payload);
      L.get_def s7_1 12; Replace.replace_def s7_1 12 (S.I64 registers.payload);
      L.get_def s7_2 11; Replace.replace_def s7_2 11 (S.I64 registers.payload);
      L.get_def s7_3 10; Replace.replace_def s7_3 10 (S.I64 registers.payload);
      L.get_def s7_4 9; Replace.replace_def s7_4 9 (S.I64 registers.payload);
      L.get_def s7_5 8; Replace.replace_def s7_5 8 (S.I64 registers.payload);
      L.get_def s7_6 7; Replace.replace_def s7_6 7 (S.I64 registers.payload);
      L.get_def s7_7 6; Replace.replace_def s7_7 6 (S.I64 registers.payload);
      L.get_def s7_8 5; Replace.replace_def s7_8 5 (S.I64 registers.payload);
      L.get_def s7_9 4; Replace.replace_def s7_9 4 (S.I64 registers.payload);
      L.get_def s7_10 3; Replace.replace_def s7_10 3 (S.I64 registers.payload);
      L.get_def s7_11 2; Replace.replace_def s7_11 2 (S.I64 registers.payload);
      L.get_def s7_12 1; Replace.replace_def s7_12 1 (S.I64 registers.payload);
      L.get_def s7_13 0; Replace.replace_def s7_13 0 (S.I64 registers.payload);
      S.same_type_def (S.I64 {W.lo = 0; hi = 0}) (S.I64 registers.payload);
      Load.correct 7 13 (S.I64 registers.payload) state7 ();
      R.load_code_def (R.Binding (7, 13, R.End)); Transfer.load_code_def 7 13;
      E.append_def (Transfer.load_code 7 13) (R.load_code (R.End));
      E.append_def (C.Next (I.Local_set 13, C.Empty)) (R.load_code (R.End));
      E.append_def C.Empty (R.load_code (R.End));
      GE.append_correct (Transfer.load_code 7 13) (R.load_code (R.End)) state7;
      R.load_code_def R.End; GE.run_def C.Empty state8);
    {X.memory; machine = {E.locals = locals registers; stack = S.Empty}}
module Store = Wasm_register_store
let[@def] (permissions @ total) (unit : unit) = G.Global (false, G.Global (true, G.Global (false, G.Global (true, G.Global (false, G.Global (true, G.Global (true, G.Global (true, G.Empty))))))))
let[@def] (globals @ total) (registers : registers @ immutable) = {G.values = values registers; permissions = permissions ()}
let[@def] (exports @ total) (locals : S.stack @ immutable) (registers : registers @ immutable) = ghost_ (
  L.get locals 1 === Some (S.I32 registers.heap)
  && L.get locals 4 === Some (S.I32 registers.top)
  && L.get locals 11 === Some (S.I32 registers.status)
  && L.get locals 12 === Some (S.I64 registers.tag)
  && L.get locals 13 === Some (S.I64 registers.payload))
let (export @ total) : (before : registers) @ immutable -> (after : registers) @ immutable -> (state : X.state) @ immutable ->
    (table_base : B.u32) -> (stack_base : B.u32) ->
    {u : unit | after.frame = before.frame && after.heap_limit = before.heap_limit && after.stack_limit = before.stack_limit
      && exports state.X.machine.E.locals after} ->
    {out : G.t | out === globals after
      && GE.run (R.store_code (Runtime.config table_base stack_base).Assembly.stores)
        {GE.globals = globals before; execution = state} === GE.Done {GE.globals = out; execution = state}} @ immutable =
  fun before after state table_base stack_base premise ->
    ghost_ (values_def before; values_def after; globals_def before; globals_def after; permissions_def ();
      exports_def state.X.machine.E.locals after; Runtime.config_def table_base stack_base;
      let p7 = G.Global (true, G.Empty) in
      let p6 = G.Global (true, p7) in
      let p5 = G.Global (true, p6) in
      let p4 = G.Global (false, p5) in
      let p3 = G.Global (true, p4) in
      let p2 = G.Global (false, p3) in
      let p1 = G.Global (true, p2) in
      let p0 = G.Global (false, p1) in
      let g0_7 = S.Push (S.I64 before.payload, S.Empty) in
      let g0_6 = S.Push (S.I64 before.tag, g0_7) in
      let g0_5 = S.Push (S.I32 before.status, g0_6) in
      let g0_4 = S.Push (S.I32 before.stack_limit, g0_5) in
      let g0_3 = S.Push (S.I32 before.top, g0_4) in
      let g0_2 = S.Push (S.I32 before.heap_limit, g0_3) in
      let g0_1 = S.Push (S.I32 before.heap, g0_2) in
      let g0_0 = S.Push (S.I32 before.frame, g0_1) in
      let state0 = {GE.globals = {G.values = g0_0; permissions = permissions ()}; execution = state} in
      let g1_7 = S.Push (S.I64 before.payload, S.Empty) in
      let g1_6 = S.Push (S.I64 before.tag, g1_7) in
      let g1_5 = S.Push (S.I32 before.status, g1_6) in
      let g1_4 = S.Push (S.I32 before.stack_limit, g1_5) in
      let g1_3 = S.Push (S.I32 before.top, g1_4) in
      let g1_2 = S.Push (S.I32 before.heap_limit, g1_3) in
      let g1_1 = S.Push (S.I32 after.heap, g1_2) in
      let g1_0 = S.Push (S.I32 before.frame, g1_1) in
      let state1 = {GE.globals = {G.values = g1_0; permissions = permissions ()}; execution = state} in
      let g2_7 = S.Push (S.I64 before.payload, S.Empty) in
      let g2_6 = S.Push (S.I64 before.tag, g2_7) in
      let g2_5 = S.Push (S.I32 before.status, g2_6) in
      let g2_4 = S.Push (S.I32 before.stack_limit, g2_5) in
      let g2_3 = S.Push (S.I32 after.top, g2_4) in
      let g2_2 = S.Push (S.I32 before.heap_limit, g2_3) in
      let g2_1 = S.Push (S.I32 after.heap, g2_2) in
      let g2_0 = S.Push (S.I32 before.frame, g2_1) in
      let state2 = {GE.globals = {G.values = g2_0; permissions = permissions ()}; execution = state} in
      let g3_7 = S.Push (S.I64 before.payload, S.Empty) in
      let g3_6 = S.Push (S.I64 before.tag, g3_7) in
      let g3_5 = S.Push (S.I32 after.status, g3_6) in
      let g3_4 = S.Push (S.I32 before.stack_limit, g3_5) in
      let g3_3 = S.Push (S.I32 after.top, g3_4) in
      let g3_2 = S.Push (S.I32 before.heap_limit, g3_3) in
      let g3_1 = S.Push (S.I32 after.heap, g3_2) in
      let g3_0 = S.Push (S.I32 before.frame, g3_1) in
      let state3 = {GE.globals = {G.values = g3_0; permissions = permissions ()}; execution = state} in
      let g4_7 = S.Push (S.I64 before.payload, S.Empty) in
      let g4_6 = S.Push (S.I64 after.tag, g4_7) in
      let g4_5 = S.Push (S.I32 after.status, g4_6) in
      let g4_4 = S.Push (S.I32 before.stack_limit, g4_5) in
      let g4_3 = S.Push (S.I32 after.top, g4_4) in
      let g4_2 = S.Push (S.I32 before.heap_limit, g4_3) in
      let g4_1 = S.Push (S.I32 after.heap, g4_2) in
      let g4_0 = S.Push (S.I32 before.frame, g4_1) in
      let state4 = {GE.globals = {G.values = g4_0; permissions = permissions ()}; execution = state} in
      let g5_7 = S.Push (S.I64 after.payload, S.Empty) in
      let g5_6 = S.Push (S.I64 after.tag, g5_7) in
      let g5_5 = S.Push (S.I32 after.status, g5_6) in
      let g5_4 = S.Push (S.I32 before.stack_limit, g5_5) in
      let g5_3 = S.Push (S.I32 after.top, g5_4) in
      let g5_2 = S.Push (S.I32 before.heap_limit, g5_3) in
      let g5_1 = S.Push (S.I32 after.heap, g5_2) in
      let g5_0 = S.Push (S.I32 before.frame, g5_1) in
      let state5 = {GE.globals = {G.values = g5_0; permissions = permissions ()}; execution = state} in
      G.can_set_def state0.GE.globals 1 (S.I32 after.heap); L.can_set_def g0_0 1 (S.I32 after.heap);
      G.writable_def p0 1; L.get_def g0_0 1; Replace.replace_def g0_0 1 (S.I32 after.heap);
      G.writable_def p1 0; L.get_def g0_1 0; Replace.replace_def g0_1 0 (S.I32 after.heap);
      S.same_type_def (S.I32 before.heap) (S.I32 after.heap);
      Store.correct 1 1 (S.I32 after.heap) state0 ();
      R.store_code_def (R.Binding (1, 1, R.Binding (3, 4, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End)))))); Transfer.store_code_def 1 1;
      E.append_def (Transfer.store_code 1 1) (R.store_code (R.Binding (3, 4, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End))))));
      E.append_def (C.Next (I.Global_set 1, C.Empty)) (R.store_code (R.Binding (3, 4, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End))))));
      E.append_def C.Empty (R.store_code (R.Binding (3, 4, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End))))));
      GE.append_correct (Transfer.store_code 1 1) (R.store_code (R.Binding (3, 4, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End)))))) state0;
      G.can_set_def state1.GE.globals 3 (S.I32 after.top); L.can_set_def g1_0 3 (S.I32 after.top);
      G.writable_def p0 3; L.get_def g1_0 3; Replace.replace_def g1_0 3 (S.I32 after.top);
      G.writable_def p1 2; L.get_def g1_1 2; Replace.replace_def g1_1 2 (S.I32 after.top);
      G.writable_def p2 1; L.get_def g1_2 1; Replace.replace_def g1_2 1 (S.I32 after.top);
      G.writable_def p3 0; L.get_def g1_3 0; Replace.replace_def g1_3 0 (S.I32 after.top);
      S.same_type_def (S.I32 before.top) (S.I32 after.top);
      Store.correct 3 4 (S.I32 after.top) state1 ();
      R.store_code_def (R.Binding (3, 4, R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End))))); Transfer.store_code_def 3 4;
      E.append_def (Transfer.store_code 3 4) (R.store_code (R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End)))));
      E.append_def (C.Next (I.Global_set 3, C.Empty)) (R.store_code (R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End)))));
      E.append_def C.Empty (R.store_code (R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End)))));
      GE.append_correct (Transfer.store_code 3 4) (R.store_code (R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End))))) state1;
      G.can_set_def state2.GE.globals 5 (S.I32 after.status); L.can_set_def g2_0 5 (S.I32 after.status);
      G.writable_def p0 5; L.get_def g2_0 5; Replace.replace_def g2_0 5 (S.I32 after.status);
      G.writable_def p1 4; L.get_def g2_1 4; Replace.replace_def g2_1 4 (S.I32 after.status);
      G.writable_def p2 3; L.get_def g2_2 3; Replace.replace_def g2_2 3 (S.I32 after.status);
      G.writable_def p3 2; L.get_def g2_3 2; Replace.replace_def g2_3 2 (S.I32 after.status);
      G.writable_def p4 1; L.get_def g2_4 1; Replace.replace_def g2_4 1 (S.I32 after.status);
      G.writable_def p5 0; L.get_def g2_5 0; Replace.replace_def g2_5 0 (S.I32 after.status);
      S.same_type_def (S.I32 before.status) (S.I32 after.status);
      Store.correct 5 11 (S.I32 after.status) state2 ();
      R.store_code_def (R.Binding (5, 11, R.Binding (6, 12, R.Binding (7, 13, R.End)))); Transfer.store_code_def 5 11;
      E.append_def (Transfer.store_code 5 11) (R.store_code (R.Binding (6, 12, R.Binding (7, 13, R.End))));
      E.append_def (C.Next (I.Global_set 5, C.Empty)) (R.store_code (R.Binding (6, 12, R.Binding (7, 13, R.End))));
      E.append_def C.Empty (R.store_code (R.Binding (6, 12, R.Binding (7, 13, R.End))));
      GE.append_correct (Transfer.store_code 5 11) (R.store_code (R.Binding (6, 12, R.Binding (7, 13, R.End)))) state2;
      G.can_set_def state3.GE.globals 6 (S.I64 after.tag); L.can_set_def g3_0 6 (S.I64 after.tag);
      G.writable_def p0 6; L.get_def g3_0 6; Replace.replace_def g3_0 6 (S.I64 after.tag);
      G.writable_def p1 5; L.get_def g3_1 5; Replace.replace_def g3_1 5 (S.I64 after.tag);
      G.writable_def p2 4; L.get_def g3_2 4; Replace.replace_def g3_2 4 (S.I64 after.tag);
      G.writable_def p3 3; L.get_def g3_3 3; Replace.replace_def g3_3 3 (S.I64 after.tag);
      G.writable_def p4 2; L.get_def g3_4 2; Replace.replace_def g3_4 2 (S.I64 after.tag);
      G.writable_def p5 1; L.get_def g3_5 1; Replace.replace_def g3_5 1 (S.I64 after.tag);
      G.writable_def p6 0; L.get_def g3_6 0; Replace.replace_def g3_6 0 (S.I64 after.tag);
      S.same_type_def (S.I64 before.tag) (S.I64 after.tag);
      Store.correct 6 12 (S.I64 after.tag) state3 ();
      R.store_code_def (R.Binding (6, 12, R.Binding (7, 13, R.End))); Transfer.store_code_def 6 12;
      E.append_def (Transfer.store_code 6 12) (R.store_code (R.Binding (7, 13, R.End)));
      E.append_def (C.Next (I.Global_set 6, C.Empty)) (R.store_code (R.Binding (7, 13, R.End)));
      E.append_def C.Empty (R.store_code (R.Binding (7, 13, R.End)));
      GE.append_correct (Transfer.store_code 6 12) (R.store_code (R.Binding (7, 13, R.End))) state3;
      G.can_set_def state4.GE.globals 7 (S.I64 after.payload); L.can_set_def g4_0 7 (S.I64 after.payload);
      G.writable_def p0 7; L.get_def g4_0 7; Replace.replace_def g4_0 7 (S.I64 after.payload);
      G.writable_def p1 6; L.get_def g4_1 6; Replace.replace_def g4_1 6 (S.I64 after.payload);
      G.writable_def p2 5; L.get_def g4_2 5; Replace.replace_def g4_2 5 (S.I64 after.payload);
      G.writable_def p3 4; L.get_def g4_3 4; Replace.replace_def g4_3 4 (S.I64 after.payload);
      G.writable_def p4 3; L.get_def g4_4 3; Replace.replace_def g4_4 3 (S.I64 after.payload);
      G.writable_def p5 2; L.get_def g4_5 2; Replace.replace_def g4_5 2 (S.I64 after.payload);
      G.writable_def p6 1; L.get_def g4_6 1; Replace.replace_def g4_6 1 (S.I64 after.payload);
      G.writable_def p7 0; L.get_def g4_7 0; Replace.replace_def g4_7 0 (S.I64 after.payload);
      S.same_type_def (S.I64 before.payload) (S.I64 after.payload);
      Store.correct 7 13 (S.I64 after.payload) state4 ();
      R.store_code_def (R.Binding (7, 13, R.End)); Transfer.store_code_def 7 13;
      E.append_def (Transfer.store_code 7 13) (R.store_code (R.End));
      E.append_def (C.Next (I.Global_set 7, C.Empty)) (R.store_code (R.End));
      E.append_def C.Empty (R.store_code (R.End));
      GE.append_correct (Transfer.store_code 7 13) (R.store_code (R.End)) state4;
      R.store_code_def R.End; GE.run_def C.Empty state5);
    globals after
let[@def] (matches @ total) (actual : S.stack @ immutable) (registers : registers @ immutable) = ghost_ (
  L.get actual 0 === Some (S.I32 registers.frame)
  && L.get actual 1 === Some (S.I32 registers.heap)
  && L.get actual 2 === Some (S.I32 registers.heap_limit)
  && L.get actual 3 === Some (S.I32 0)
  && L.get actual 4 === Some (S.I32 registers.top)
  && L.get actual 5 === Some (S.I32 registers.stack_limit)
  && L.get actual 6 === Some (S.I32 0)
  && L.get actual 7 === Some (S.I32 0)
  && L.get actual 8 === Some (S.I32 0)
  && L.get actual 9 === Some (S.I32 0)
  && L.get actual 10 === Some (S.I32 0)
  && L.get actual 11 === Some (S.I32 registers.status)
  && L.get actual 12 === Some (S.I64 registers.tag)
  && L.get actual 13 === Some (S.I64 registers.payload)
  && L.get actual 14 === Some (S.I64 {W.lo = 0; hi = 0})
  && L.get actual 15 === Some (S.I64 {W.lo = 0; hi = 0})
  && L.get actual 16 === Some (S.I64 {W.lo = 0; hi = 0})
  && L.get actual 17 === Some (S.I64 {W.lo = 0; hi = 0}))
let (local_values @ total) : (registers : registers) @ immutable ->
    {u : unit | matches (locals registers) registers && exports (locals registers) registers} @ ghost = fun registers -> ghost_ (
    locals_def registers; matches_def (locals registers) registers; exports_def (locals registers) registers;
    let l17 = S.Push (S.I64 {W.lo = 0; hi = 0}, S.Empty) in
    let l16 = S.Push (S.I64 {W.lo = 0; hi = 0}, l17) in
    let l15 = S.Push (S.I64 {W.lo = 0; hi = 0}, l16) in
    let l14 = S.Push (S.I64 {W.lo = 0; hi = 0}, l15) in
    let l13 = S.Push (S.I64 registers.payload, l14) in
    let l12 = S.Push (S.I64 registers.tag, l13) in
    let l11 = S.Push (S.I32 registers.status, l12) in
    let l10 = S.Push (S.I32 0, l11) in
    let l9 = S.Push (S.I32 0, l10) in
    let l8 = S.Push (S.I32 0, l9) in
    let l7 = S.Push (S.I32 0, l8) in
    let l6 = S.Push (S.I32 0, l7) in
    let l5 = S.Push (S.I32 registers.stack_limit, l6) in
    let l4 = S.Push (S.I32 registers.top, l5) in
    let l3 = S.Push (S.I32 0, l4) in
    let l2 = S.Push (S.I32 registers.heap_limit, l3) in
    let l1 = S.Push (S.I32 registers.heap, l2) in
    let l0 = S.Push (S.I32 registers.frame, l1) in
    L.get_def l0 0;
    L.get_def l0 1;
    L.get_def l1 0;
    L.get_def l0 2;
    L.get_def l1 1;
    L.get_def l2 0;
    L.get_def l0 3;
    L.get_def l1 2;
    L.get_def l2 1;
    L.get_def l3 0;
    L.get_def l0 4;
    L.get_def l1 3;
    L.get_def l2 2;
    L.get_def l3 1;
    L.get_def l4 0;
    L.get_def l0 5;
    L.get_def l1 4;
    L.get_def l2 3;
    L.get_def l3 2;
    L.get_def l4 1;
    L.get_def l5 0;
    L.get_def l0 6;
    L.get_def l1 5;
    L.get_def l2 4;
    L.get_def l3 3;
    L.get_def l4 2;
    L.get_def l5 1;
    L.get_def l6 0;
    L.get_def l0 7;
    L.get_def l1 6;
    L.get_def l2 5;
    L.get_def l3 4;
    L.get_def l4 3;
    L.get_def l5 2;
    L.get_def l6 1;
    L.get_def l7 0;
    L.get_def l0 8;
    L.get_def l1 7;
    L.get_def l2 6;
    L.get_def l3 5;
    L.get_def l4 4;
    L.get_def l5 3;
    L.get_def l6 2;
    L.get_def l7 1;
    L.get_def l8 0;
    L.get_def l0 9;
    L.get_def l1 8;
    L.get_def l2 7;
    L.get_def l3 6;
    L.get_def l4 5;
    L.get_def l5 4;
    L.get_def l6 3;
    L.get_def l7 2;
    L.get_def l8 1;
    L.get_def l9 0;
    L.get_def l0 10;
    L.get_def l1 9;
    L.get_def l2 8;
    L.get_def l3 7;
    L.get_def l4 6;
    L.get_def l5 5;
    L.get_def l6 4;
    L.get_def l7 3;
    L.get_def l8 2;
    L.get_def l9 1;
    L.get_def l10 0;
    L.get_def l0 11;
    L.get_def l1 10;
    L.get_def l2 9;
    L.get_def l3 8;
    L.get_def l4 7;
    L.get_def l5 6;
    L.get_def l6 5;
    L.get_def l7 4;
    L.get_def l8 3;
    L.get_def l9 2;
    L.get_def l10 1;
    L.get_def l11 0;
    L.get_def l0 12;
    L.get_def l1 11;
    L.get_def l2 10;
    L.get_def l3 9;
    L.get_def l4 8;
    L.get_def l5 7;
    L.get_def l6 6;
    L.get_def l7 5;
    L.get_def l8 4;
    L.get_def l9 3;
    L.get_def l10 2;
    L.get_def l11 1;
    L.get_def l12 0;
    L.get_def l0 13;
    L.get_def l1 12;
    L.get_def l2 11;
    L.get_def l3 10;
    L.get_def l4 9;
    L.get_def l5 8;
    L.get_def l6 7;
    L.get_def l7 6;
    L.get_def l8 5;
    L.get_def l9 4;
    L.get_def l10 3;
    L.get_def l11 2;
    L.get_def l12 1;
    L.get_def l13 0;
    L.get_def l0 14;
    L.get_def l1 13;
    L.get_def l2 12;
    L.get_def l3 11;
    L.get_def l4 10;
    L.get_def l5 9;
    L.get_def l6 8;
    L.get_def l7 7;
    L.get_def l8 6;
    L.get_def l9 5;
    L.get_def l10 4;
    L.get_def l11 3;
    L.get_def l12 2;
    L.get_def l13 1;
    L.get_def l14 0;
    L.get_def l0 15;
    L.get_def l1 14;
    L.get_def l2 13;
    L.get_def l3 12;
    L.get_def l4 11;
    L.get_def l5 10;
    L.get_def l6 9;
    L.get_def l7 8;
    L.get_def l8 7;
    L.get_def l9 6;
    L.get_def l10 5;
    L.get_def l11 4;
    L.get_def l12 3;
    L.get_def l13 2;
    L.get_def l14 1;
    L.get_def l15 0;
    L.get_def l0 16;
    L.get_def l1 15;
    L.get_def l2 14;
    L.get_def l3 13;
    L.get_def l4 12;
    L.get_def l5 11;
    L.get_def l6 10;
    L.get_def l7 9;
    L.get_def l8 8;
    L.get_def l9 7;
    L.get_def l10 6;
    L.get_def l11 5;
    L.get_def l12 4;
    L.get_def l13 3;
    L.get_def l14 2;
    L.get_def l15 1;
    L.get_def l16 0;
    L.get_def l0 17;
    L.get_def l1 16;
    L.get_def l2 15;
    L.get_def l3 14;
    L.get_def l4 13;
    L.get_def l5 12;
    L.get_def l6 11;
    L.get_def l7 10;
    L.get_def l8 9;
    L.get_def l9 8;
    L.get_def l10 7;
    L.get_def l11 6;
    L.get_def l12 5;
    L.get_def l13 4;
    L.get_def l14 3;
    L.get_def l15 2;
    L.get_def l16 1;
    L.get_def l17 0)
let (exports_preserved @ total) : (before : S.stack) @ immutable -> (after : S.stack) @ immutable ->
    (index : B.u32) -> (value : S.value) @ immutable -> (registers : registers) @ immutable ->
    {u : unit | exports before registers && L.replaced before index value after
      && index <> 1 && index <> 4 && index <> 11 && index <> 12 && index <> 13} ->
    {u : unit | exports after registers} @ ghost = fun before after index value registers premise -> ghost_ (
    exports_def before registers; exports_def after registers;
    L.other_local before index value after 1 (); L.other_local before index value after 4 ();
    L.other_local before index value after 11 (); L.other_local before index value after 12 (); L.other_local before index value after 13 ())
let (read @ total) : (image : G.t) @ immutable ->
    {out : registers option | match out with None -> true | Some registers -> image === globals registers} @ immutable = fun image ->
  match image.G.values, image.G.permissions with
  | S.Push (S.I32 frame, S.Push (S.I32 heap, S.Push (S.I32 heap_limit, S.Push (S.I32 top,
      S.Push (S.I32 stack_limit, S.Push (S.I32 status, S.Push (S.I64 tag, S.Push (S.I64 payload, S.Empty)))))))),
    G.Global (false, G.Global (true, G.Global (false, G.Global (true, G.Global (false, G.Global (true, G.Global (true, G.Global (true, G.Empty)))))))) ->
    let registers = {frame; heap; heap_limit; top; stack_limit; status; tag; payload} in
    ghost_ (globals_def registers; values_def registers; permissions_def ());
    Some registers
  | _ -> None
let (read_exports @ total) : (before : registers) @ immutable -> (actual : S.stack) @ immutable ->
    {out : registers option | match out with None -> true | Some after ->
      after.frame = before.frame && after.heap_limit = before.heap_limit && after.stack_limit = before.stack_limit && exports actual after} @ immutable =
  fun before actual ->
    match L.get actual 1, L.get actual 4, L.get actual 11, L.get actual 12, L.get actual 13 with
    | Some (S.I32 heap), Some (S.I32 top), Some (S.I32 status), Some (S.I64 tag), Some (S.I64 payload) ->
      let after = {before with heap; top; status; tag; payload} in
      ghost_ (exports_def actual after); Some after
    | _ -> None
