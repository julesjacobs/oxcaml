include Hmc_layout
module W = Hmc_word64
module S = Wasm_scalar
module G = Wasm_globals
module GE = Wasm_global_execution
module X = Wasm_memory_execution
module E = Wasm_execution

let[@def] (valid_layout @ total) (layout : layout @ immutable) (memory : B.bytes @ immutable) = ghost_ (
  layout.table_base <= layout.frame_base && layout.frame_base <= 4294967216
  && layout.frame_base <= layout.stack_base && layout.stack_base <= layout.heap_base
  && layout.heap_base <= layout.heap_limit
  && not (Hmc_linear_bytes.drop memory layout.heap_limit === None))

let[@def] (returned @ total) (after : GE.state @ immutable) (word : W.t @ immutable) = ghost_ (
  after.GE.execution.X.machine.E.stack === S.Push (S.I32 1, S.Empty)
  && match after.GE.globals.G.values, after.GE.globals.G.permissions with
  | S.Push (S.I32 _, S.Push (S.I32 _, S.Push (S.I32 _, S.Push (S.I32 _,
      S.Push (S.I32 _, S.Push (S.I32 status, S.Push (S.I64 tag, S.Push (S.I64 payload, S.Empty)))))))),
    G.Global (false, G.Global (true, G.Global (false, G.Global (true, G.Global (false,
      G.Global (true, G.Global (true, G.Global (true, G.Empty)))))))) ->
    status = 1 && tag === {W.lo = 1; hi = 0} && payload === word
  | _ -> false)

let[@def] (source_returns @ total) (source : D.term @ immutable) (input : W.t @ immutable)
    (fuel : D.index @ immutable) (word : W.t @ immutable) = ghost_ (
  Hmc_source_semantics.advance fuel (Hmc_source_semantics.initial (D.Apply (source, D.Word input)))
    === Hmc_source_semantics.Done (Hm_interpreter_typing.Word word))

let[@def] (exhausted @ total) (after : GE.state @ immutable) = ghost_ (
  match after.GE.execution.X.machine.E.stack with
  | S.Push (S.I32 status, S.Empty) -> status = 2 || status = 3
  | _ -> false)

type execution = {fuel : C.count; after : GE.state}

let[@def] rec (stack_fits @ total) (frames : D.index @ immutable) (capacity : D.index @ immutable) =
  match frames, capacity with
  | D.Z, _ -> true | D.S _, D.Z -> false
  | D.S frames, D.S capacity -> stack_fits frames capacity

let[@def] rec (target_budget @ total) (source_fuel : D.index @ immutable) =
  match source_fuel with D.Z -> D.S (D.S D.Z) | D.S rest -> D.S (D.S (target_budget rest))

let[@def] (frame_room @ total) (layout : layout @ immutable) : W.limb =
  if layout.frame_base <= layout.stack_base then layout.stack_base - layout.frame_base else 0

let[@def] rec (heap_budget @ total) (steps : D.index @ immutable) (width : W.limb)
    (start : W.limb) (limit : W.limb) =
  if start > limit then false else match steps with
  | D.Z -> true
  | D.S rest ->
    if width > 268435455 || 16 * width > limit - start then false
    else heap_budget rest width (start + 16 * width) limit

let[@def] (sufficient @ total) (layout : layout @ immutable) (bytes : B.bytes @ immutable)
    (source_fuel : D.index @ immutable) =
  match Wasm_binary_module.decode bytes with
  | Some (image, B.End) -> (match image.Wasm_binary_module.globals.G.values with
    | S.Push (_, S.Push (S.I32 heap_start, _)) ->
      heap_budget (target_budget source_fuel) (frame_room layout) heap_start layout.heap_limit
      && stack_fits (target_budget source_fuel) layout.stack_capacity
    | _ -> false)
  | _ -> false

type exhaustion = {prefix : C.count; before : Wasm_calls.configuration; remaining : C.count}
let[@def] (honest_exhaustion @ total) (bytes : B.bytes @ immutable) (capacity : C.count @ immutable)
    (after : GE.state @ immutable) (witness : exhaustion @ immutable) = ghost_ (
  Wasm_binary_execution.run witness.prefix bytes capacity === Wasm_binary_execution.Result (Wasm_calls.Running witness.before)
  && (match after.GE.execution.X.machine.E.stack with
    | S.Push (S.I32 2, S.Empty) -> Hmc_failed_guard_model.failed Hmc_failed_guard_model.Heap witness.before.Wasm_calls.current.Wasm_instance_control.body
    | S.Push (S.I32 3, S.Empty) -> Hmc_failed_guard_model.failed Hmc_failed_guard_model.Stack witness.before.Wasm_calls.current.Wasm_instance_control.body
    | _ -> false)
  && match Wasm_binary_module.decode bytes with
    | Some (image, B.End) -> Wasm_calls.run witness.remaining image.Wasm_binary_module.module_ witness.before === Wasm_calls.Finished after
    | _ -> false)
