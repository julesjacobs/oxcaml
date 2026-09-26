module B = Wasm_u32
module I = Wasm_instruction
module T = Wasm_control
module F = Wasm_functions
module G = Wasm_globals
module V = Wasm_static_types
module Step = Wasm_static_steps
module Check = Wasm_static_control
module Runtime = Hmc_wasm_program_runtime
let[@def] (zero @ total) (unit : unit) : B.u32 = 0
let[@def] (status_index @ total) (unit : unit) : B.u32 = 5
let (dispatcher @ total) : (module_ : F.module_) @ immutable -> (globals : G.t) @ immutable ->
    {u : unit | F.signature module_.F.signatures (zero ()) === Some F.Void
      && V.global globals (zero ()) === Some V.I32 && V.global globals (status_index ()) === Some V.I32} ->
    {u : unit | Check.function_ module_ globals (Runtime.dispatcher ())} @ ghost =
  fun module_ globals premise -> ghost_ (
    zero_def (); status_index_def (); Runtime.dispatcher_def (); Runtime.dispatch_body_def ();
    let context = {V.module_; globals; locals = F.No_locals; result = F.I32} in
    let empty = V.initial () in
    let value = V.push V.I32 empty in
    let labels = Check.Label (Check.Root F.I32) in
    let branch = T.Instruction (I.Br_if 0, T.Empty) in
    let test = T.Instruction (I.Plain I.I32_eqz, branch) in
    let status = T.Instruction (I.Global_get 5, test) in
    let call = T.Instruction (I.Call_indirect 0, status) in
    let read = T.Instruction (I.I32_load (0, 8), call) in
    Step.global_get context 0 V.I32 empty ();
    Step.load32 context 0 8 empty;
    Step.take_push V.I32 empty; V.produce_result_def F.Void empty;
    V.instruction_def context (I.Call_indirect 0) value;
    Step.global_get context 5 V.I32 empty ();
    Step.unary_push V.I32 V.I32 empty; V.plain_def I.I32_eqz value;
    V.instruction_def context (I.Plain I.I32_eqz) value;
    Check.label_def labels 0; V.consume_result_def F.Void empty;
    Check.check_def context labels T.Empty empty; Check.check_def context labels branch value;
    Check.check_def context labels test value; Check.check_def context labels status empty;
    Check.check_def context labels call value; Check.check_def context labels read value;
    Check.check_def context labels (Runtime.dispatch_body ()) empty;
    V.finish_def F.Void empty; V.initial_def ();
    let finish = T.Instruction (I.Global_get 5, T.Empty) in
    Check.check_def context (Check.Root F.I32) T.Empty value;
    Check.check_def context (Check.Root F.I32) finish empty;
    Check.check_def context (Check.Root F.I32) (Runtime.dispatcher ()).F.code empty;
    V.finish_def F.I32 value; V.consume_result_def F.I32 value;
    Check.function__def module_ globals (Runtime.dispatcher ()))
