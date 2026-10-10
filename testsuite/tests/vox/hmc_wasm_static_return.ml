module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module E = Wasm_execution
module T = Wasm_control
module V = Wasm_static_types
module Q = Wasm_static_sequence
module Step = Wasm_static_steps
module Check = Wasm_static_control
module Plan = Wasm_parallel_copy
module Copy = Wasm_cross_copy
module Retreat = Hmc_wasm_stack_retreat
module Result = Hmc_wasm_return_result
module Frame = Hmc_wasm_return_frame
module Probe = Wasm_local_probe
let (retreat @ total) : (context : V.context) @ immutable -> (bytes : B.u32) -> (cursor : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals cursor === Some V.I32} ->
    {u : unit | Q.check context (Retreat.emit bytes cursor) state === Some state} @ ghost =
  fun context bytes cursor state premise -> ghost_ (
    let a = V.push V.I32 state in let b = V.push V.I32 a in
    let c3 = C.Next (I.Local_set cursor, C.Empty) in
    let c2 = C.Next (I.Plain I.I32_sub, c3) in let c1 = C.Next (I.I32_const bytes, c2) in
    Step.local_get context cursor V.I32 state ();
    V.instruction_def context (I.I32_const bytes) a;
    Step.take_push V.I32 a; Step.unary_push V.I32 V.I32 state;
    V.binary_def V.I32 V.I32 b; V.plain_def I.I32_sub b; V.instruction_def context (I.Plain I.I32_sub) b;
    Step.local_set context cursor V.I32 state ();
    Q.check_def context C.Empty state; Q.check_def context c3 a;
    Q.check_def context c2 b; Q.check_def context c1 a;
    Retreat.emit_def bytes cursor; Q.check_def context (Retreat.emit bytes cursor) state)
let (frame @ total) : (context : V.context) @ immutable -> (plan : Plan.plan) @ immutable -> (width : B.u32) ->
    (active : B.u32) -> (top : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals active === Some V.I32 && V.local context.V.locals top === Some V.I32} ->
    {u : unit | Q.check context (Frame.emit plan width active top) state === Some state} @ ghost =
  fun context plan width active top state premise -> ghost_ (
    retreat context width top state ();
    Wasm_static_copy.cross context (Result.plan ()) active top state ();
    Result.emit_def active top;
    Wasm_static_copy.cross context plan top active state ();
    Q.append context (Result.emit active top) (Copy.emit plan top active) state;
    Q.append context (Retreat.emit width top) (E.append (Result.emit active top) (Copy.emit plan top active)) state;
    Frame.emit_def plan width active top)
let (probe @ total) : (context : V.context) @ immutable -> (label : B.u32) -> (local : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals local === Some V.I32} ->
    {u : unit | Q.check context (Probe.emit label local) state === Some (V.push V.I32 state)} @ ghost =
  fun context label local state premise -> ghost_ (
    let a = V.push V.I32 state in let b = V.push V.I32 a in
    let c2 = C.Next (I.Plain I.I32_eq, C.Empty) in let c1 = C.Next (I.I32_const label, c2) in
    Step.local_get context local V.I32 state (); V.instruction_def context (I.I32_const label) a;
    Step.take_push V.I32 a; Step.unary_push V.I32 V.I32 state;
    V.binary_def V.I32 V.I32 b; V.plain_def I.I32_eq b; V.instruction_def context (I.Plain I.I32_eq) b;
    Q.check_def context C.Empty a; Q.check_def context c2 b; Q.check_def context c1 a;
    Probe.emit_def label local; Q.check_def context (Probe.emit label local) state)
let (caller @ total) : (context : V.context) @ immutable -> (labels : Check.labels) @ immutable ->
    (plan : Plan.plan) @ immutable -> (count : Hmc_wasm_relayout.count) -> (active : B.u32) -> (top : B.u32) -> (base : B.u32) ->
    (root : T.code) @ immutable -> (tail : T.code) @ immutable -> (root_state : V.state) @ immutable -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals active === Some V.I32 && V.local context.V.locals top === Some V.I32
      && Check.check context (Check.Label labels) root (V.initial ()) === Some root_state && V.finish Wasm_functions.Void root_state} ->
    {u : unit | Check.check context labels (Hmc_wasm_caller_return.emit plan count active top base root tail) state === Check.check context labels tail state} @ ghost =
  fun context labels plan count active top base root tail root_state state premise -> ghost_ (
    let empty = V.initial () in
    let code = Frame.emit plan (Hmc_wasm_frame_restore.width count) active top in
    let restored = Wasm_control_lift.embed code T.Empty in
    frame context plan (Hmc_wasm_frame_restore.width count) active top empty ();
    Check.check_def context (Check.Label labels) T.Empty empty;
    Q.embed_checked context (Check.Label labels) code T.Empty empty empty ();
    V.initial_def (); V.consume_result_def Wasm_functions.Void empty; V.finish_def Wasm_functions.Void empty;
    probe context base top state (); Step.take_push V.I32 state;
    Check.check_def context labels (T.If (root, restored, tail)) (V.push V.I32 state);
    Q.embed_checked context labels (Probe.emit base top) (T.If (root, restored, tail)) state (V.push V.I32 state) ();
    Hmc_wasm_return_select.emit_def base top root restored tail;
    Hmc_wasm_caller_return.emit_def plan count active top base root tail)
let (root_reads @ total) : (context : V.context) @ immutable -> (active : B.u32) -> (tag : B.u32) -> (payload : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals active === Some V.I32 && V.local context.V.locals tag === Some V.I64 && V.local context.V.locals payload === Some V.I64} ->
    {u : unit | Q.check context (Wasm_frame_snapshot.emit (Hmc_wasm_root_return.reads tag payload) active) state === Some state} @ ghost =
  fun context active tag payload state premise -> ghost_ (
    Hmc_wasm_root_return.reads_def tag payload;
    Hmc_wasm_static_cons.destinations_def context Wasm_frame_snapshot.End;
    Hmc_wasm_static_cons.destinations_def context (Wasm_frame_snapshot.Read (40, payload, Wasm_frame_snapshot.End));
    Hmc_wasm_static_cons.destinations_def context (Hmc_wasm_root_return.reads tag payload);
    Hmc_wasm_static_cons.snapshot context (Hmc_wasm_root_return.reads tag payload) active state ())
module Emit = Hmc_wasm_program_emit
let (status @ total) : (context : V.context) @ immutable -> (labels : Check.labels) @ immutable -> (local : B.u32) -> (value : B.u32) ->
    (tail : T.code) @ immutable -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals local === Some V.I32} ->
    {u : unit | Check.check context labels (Emit.status local value tail) state === Check.check context labels tail state} @ ghost =
  fun context labels local value tail state premise -> ghost_ (
    V.instruction_def context (I.I32_const value) state; Step.local_set context local V.I32 state ();
    Check.check_def context labels (T.Instruction (I.Local_set local, tail)) (V.push V.I32 state);
    Emit.status_def local value tail;
    Check.check_def context labels (Emit.status local value tail) state)
let (protected @ total) : (context : V.context) @ immutable -> (labels : Check.labels) @ immutable ->
    (local : B.u32) -> (failure : B.u32) -> (body : T.code) @ immutable -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals local === Some V.I32
      && Check.check context (Check.Label (Check.Label labels)) body (V.initial ()) === Some (V.initial ())} ->
    {u : unit | Check.check context labels (Emit.protected local failure body) state === Some state} @ ghost =
  fun context labels local failure body state premise -> ghost_ (
    let empty = V.initial () in
    let tail = Emit.status local 0 T.Empty in
    let inner = T.Block (body, tail) in
    Check.check_def context (Check.Label labels) T.Empty empty;
    status context (Check.Label labels) local 0 T.Empty empty ();
    V.initial_def (); V.consume_result_def Wasm_functions.Void empty; V.finish_def Wasm_functions.Void empty;
    Check.check_def context (Check.Label labels) inner empty;
    status context (Check.Label labels) local failure inner empty ();
    Check.check_def context labels T.Empty state;
    Emit.protected_def local failure body;
    Check.check_def context labels (Emit.protected local failure body) state)
let (return @ total) : (context : V.context) @ immutable -> (labels : Check.labels) @ immutable ->
    (program : Hmc_wasm_program_lower.program) @ immutable -> (locals : Emit.locals) @ immutable ->
    (table_base : B.u32) -> (stack_base : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals locals.Emit.structured.Hmc_wasm_structured_block.frame === Some V.I32
      && V.local context.V.locals locals.Emit.top === Some V.I32 && V.local context.V.locals locals.Emit.status === Some V.I32
      && V.local context.V.locals locals.Emit.result_tag === Some V.I64 && V.local context.V.locals locals.Emit.result_payload === Some V.I64} ->
    {u : unit | Check.check context labels (Emit.emit program Hmc_wasm_program_block.Return locals table_base stack_base) state === Some state} @ ghost =
  fun context labels program locals table_base stack_base state premise -> ghost_ (
    let active = locals.Emit.structured.Hmc_wasm_structured_block.frame in
    let empty = V.initial () in
    let reads = Wasm_frame_snapshot.emit (Hmc_wasm_root_return.reads locals.Emit.result_tag locals.Emit.result_payload) active in
    let done_ = Emit.status locals.Emit.status 1 T.Empty in
    let root = Wasm_control_lift.embed reads done_ in
    root_reads context active locals.Emit.result_tag locals.Emit.result_payload empty ();
    Check.check_def context (Check.Label labels) T.Empty empty;
    status context (Check.Label labels) locals.Emit.status 1 T.Empty empty ();
    Q.embed_checked context (Check.Label labels) reads done_ empty empty ();
    V.initial_def (); V.consume_result_def Wasm_functions.Void empty; V.finish_def Wasm_functions.Void empty;
    caller context labels program.Hmc_wasm_program_lower.restore program.Hmc_wasm_program_lower.capacity active locals.Emit.top stack_base root T.Empty empty state ();
    Check.check_def context labels T.Empty state;
    status context labels locals.Emit.status 0
      (Hmc_wasm_caller_return.emit program.Hmc_wasm_program_lower.restore program.Hmc_wasm_program_lower.capacity active locals.Emit.top stack_base root T.Empty) state ();
    Emit.emit_def program Hmc_wasm_program_block.Return locals table_base stack_base)
let (protected_finished @ total) : (context : V.context) @ immutable -> (labels : Check.labels) @ immutable ->
    (local : B.u32) -> (failure : B.u32) -> (body : T.code) @ immutable -> (body_state : V.state) @ immutable -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals local === Some V.I32
      && Check.check context (Check.Label (Check.Label labels)) body (V.initial ()) === Some body_state
      && V.finish Wasm_functions.Void body_state} ->
    {u : unit | Check.check context labels (Emit.protected local failure body) state === Some state} @ ghost =
  fun context labels local failure body body_state state premise -> ghost_ (
    let empty = V.initial () in let tail = Emit.status local 0 T.Empty in let inner = T.Block (body, tail) in
    Check.check_def context (Check.Label labels) T.Empty empty;
    status context (Check.Label labels) local 0 T.Empty empty ();
    V.initial_def (); V.consume_result_def Wasm_functions.Void empty; V.finish_def Wasm_functions.Void empty;
    Check.check_def context (Check.Label labels) inner empty;
    status context (Check.Label labels) local failure inner empty ();
    Check.check_def context labels T.Empty state;
    Emit.protected_def local failure body;
    Check.check_def context labels (Emit.protected local failure body) state)
