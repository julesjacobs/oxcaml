include Hmc_layout
module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
module H = Hmc_heap_objects
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module Machine = Hmc_heap_machine
module Init = Hmc_heap_initialize
module Lower = Hmc_wasm_program_lower
module State = Hmc_wasm_program_state
module Registers = Hmc_wasm_program_registers
module Index = Hmc_u32_index
module Cap = Hmc_frame_capacity
module Codec = Hmc_pointer_frame_codec
module Pad = Hmc_wasm_frame_padding
module Bounds = Hmc_linear_bounds
module Runtime = Hmc_runtime_closures
module Frame = Hmc_wasm_program_frame
module Store = Hmc_wasm_program_frame_store
module Descriptors = Hmc_wasm_program_descriptors
module Capacity = Hmc_memory_stack_capacity
module V = Hmc_tagged_cell
let[@def] (valid_layout @ total) (layout : layout @ immutable) (memory : B.bytes @ immutable) = ghost_ (
  layout.table_base <= layout.frame_base && layout.frame_base <= 4294967216
  && layout.frame_base <= layout.stack_base && layout.stack_base <= layout.heap_base && layout.heap_base <= layout.heap_limit
  && Bounds.covers memory layout.heap_limit)
type prepared = {lowered : Lower.program; context : State.context; state : State.running}
let[@def] (installed @ total) (program : I.program @ immutable) (layout : layout @ immutable)
    (input : Hmc_word64.t @ immutable) (memory : B.bytes @ immutable) (start : Init.start @ immutable) (out : prepared @ immutable) = ghost_ (
  State.valid program start.Init.globals out.lowered out.context out.state
  && State.configuration out.state === start.Init.configuration
  && out.state.State.abstract === U.initial program input && out.state.State.elapsed === D.Z
  && out.context.State.input === input && out.context.State.max_pc = layout.max_pc
  && out.context.State.table_base = layout.table_base && out.context.State.stack_base = layout.stack_base
  && out.context.State.stack_capacity === layout.stack_capacity && out.context.State.host_capacity === layout.host_capacity
  && out.state.State.registers.Registers.frame = layout.frame_base
  && out.state.State.registers.Registers.heap_limit = layout.heap_limit
  && V.length out.state.State.memory === V.length memory)
let[@def] (installable @ total) (program : I.program @ immutable) (layout : layout @ immutable) (start : Init.start @ immutable) = ghost_ (
  match start.Init.configuration.Machine.state with
  | Q.Running (activation, Q.Halt) ->
    let blocks = program.I.origin.Hmc_cfg_program.blocks in
    let table = program.I.origin.Hmc_cfg_program.origin.Hmc_closure_program.table in
    let capacity = Hmc_wasm_relayout_geometry.size (Cap.capacity blocks) in
    let table_count = Hmc_wasm_relayout_geometry.size (Hmc_closure_ir.size table) in
    Lower.encodable program start.Init.globals layout.max_pc
    && (match G.lookup blocks activation.F.pc with None -> false | Some _ -> true)
    && Index.fits activation.F.pc layout.max_pc && Index.fits (G.size blocks) layout.max_pc
    && Index.fits (Hmc_closure_ir.size table) layout.max_pc
    && Runtime.encodable layout.max_pc table program.I.origin.Hmc_cfg_program.functions
    && Index.fits (Hmc_closure_ir.size table) 134217727
    && if capacity < 0 || capacity > 268435451 || table_count < 0 || table_count > 134217727 then false else
      layout.frame_base + 16 * (capacity + 1) <= layout.stack_base
      && not (Capacity.remaining_capacity (16 + 16 * capacity) layout.stack_capacity (layout.heap_base - layout.stack_base) === None)
      && layout.table_base + 32 * table_count <= layout.frame_base
  | _ -> false)
let (install @ total) : (program : I.program) @ immutable -> (layout : layout) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (memory : B.bytes) @ immutable -> (start : Init.start) @ immutable ->
    {u : unit | valid_layout layout memory && Init.correct program layout.heap_base layout.heap_limit input (Init.Initialized start)} ->
    {out : prepared option | match out with None -> not (installable program layout start)
      | Some out -> installable program layout start && installed program layout input memory start out} @ immutable =
  fun program layout input memory start premise ->
    ghost_ (valid_layout_def layout memory; Init.correct_def program layout.heap_base layout.heap_limit input (Init.Initialized start);
      installable_def program layout start);
    match start.Init.configuration.Machine.state with
    | Q.Running (activation, Q.Halt) ->
      let globals = start.Init.globals in
      let heap = start.Init.configuration.Machine.heap in
      let abstract = U.initial program input in
      let blocks = program.I.origin.Hmc_cfg_program.blocks in
      let table = program.I.origin.Hmc_cfg_program.origin.Hmc_closure_program.table in
      (match Lower.lower program globals layout.max_pc, G.lookup blocks activation.F.pc,
        Index.encode layout.max_pc activation.F.pc, Index.encode layout.max_pc (G.size blocks),
        Index.encode layout.max_pc (Hmc_closure_ir.size table), Runtime.lower layout.max_pc table program.I.origin.Hmc_cfg_program.functions with
      | Some lowered, Some block, Some pc, Some block_count, Some _, Some runtime ->
        ghost_ (Lower.corresponds_def program globals layout.max_pc lowered;
          Hmc_wasm_relayout_geometry.size_represents (Cap.capacity blocks) lowered.Lower.capacity ();
          Runtime.same_size table program.I.origin.Hmc_cfg_program.functions runtime ();
          Hmc_wasm_program_selection.frame_shape program globals layout.heap_limit start.Init.configuration abstract activation Q.Halt block ();
          Cap.lookup blocks activation.F.pc block (); Hmc_pointer_frame_shape.size block.G.signature);
        let remaining = Cap.remaining (Cap.capacity blocks) (Codec.size block.G.signature) () in
        let padding = Pad.cells remaining in
        let count : Hmc_wasm_reservation.count = lowered.Lower.capacity + 1 in
        ghost_ (Pad.length remaining; Index.represents_def (D.S (Cap.capacity blocks)) count);
        (match Hmc_wasm_reservation.reserve (D.S (Cap.capacity blocks)) count layout.frame_base layout.stack_base (),
          Capacity.reserve lowered.Lower.width layout.stack_capacity layout.stack_base layout.heap_base (),
          Index.encode 134217727 (Runtime.size runtime) with
        | Some frame_end, Some stack_limit, Some table_count ->
          ghost_ (Hmc_wasm_relayout_geometry.size_represents (Runtime.size runtime) table_count ());
          if layout.table_base + 32 * table_count > layout.frame_base then None else
          let registers = {Registers.frame = layout.frame_base; heap = H.used heap; heap_limit = layout.heap_limit;
            top = layout.stack_base; stack_limit; status = 0;
            tag = V.tag V.Nil; payload = V.payload V.Nil} in
          ghost_ (Capacity.ordered lowered.Lower.width layout.stack_capacity layout.stack_base stack_limit ();
            let _ = Bounds.suffix memory layout.heap_limit stack_limit () in
            let _ = Bounds.suffix memory layout.heap_limit frame_end () in
            Bounds.covers_def memory stack_limit; Bounds.covers_def memory frame_end);
          let stored = Hmc_wasm_program_memory_initialize.initialize program globals lowered.Lower.width layout.stack_base frame_end
            layout.heap_base layout.max_pc abstract heap activation registers memory block.G.signature padding pc count () in
          let installed_memory = Descriptors.install program globals lowered.Lower.width layout.stack_base frame_end abstract heap activation Q.Halt
            registers stored block.G.signature padding pc count runtime layout.table_base table_count () in
          let context = {State.input; max_pc = layout.max_pc; block_count; table_base = layout.table_base; table_count; runtime;
            stack_base = layout.stack_base; stack_capacity = layout.stack_capacity; host_capacity = layout.host_capacity} in
          let state = {State.abstract; elapsed = D.Z; heap; activation; frames = Q.Halt; block; registers;
            memory = installed_memory.Store.memory; pc; cells = installed_memory.Store.cells; padding;
            bytes = installed_memory.Store.bytes; suffix = installed_memory.Store.suffix; frame_end; cell_count = count} in
          ghost_ (Frame.valid_def block.G.signature activation registers state.State.memory frame_end pc state.State.cells padding
              state.State.bytes state.State.suffix count;
            Index.represents_def (D.S (H.length state.State.cells)) count;
            Index.injective (Cap.capacity blocks) (H.length state.State.cells) lowered.Lower.capacity ();
            U.advance_def program D.Z abstract;
            State.valid_def program globals lowered context state; State.configuration_def state);
          let out = {lowered; context; state} in
          ghost_ (installed_def program layout input memory start out); Some out
        | _ -> None)
      | _ -> None)
    | _ -> unreachable_ ()
let[@def] (layout_accepts @ total) (program : I.program @ immutable) (layout : layout @ immutable) (input : Hmc_word64.t @ immutable) = ghost_ (
  if layout.heap_base > layout.heap_limit then false else
  match Init.initialize program layout.heap_base layout.heap_limit input () with
  | Init.Heap_exhausted _ -> false
  | Init.Initialized start -> installable program layout start)
type result = Initialized of Init.start * prepared | Heap_exhausted of H.heap * D.index | Layout_rejected
let[@def] (correct @ total) (program : I.program @ immutable) (layout : layout @ immutable)
    (input : Hmc_word64.t @ immutable) (memory : B.bytes @ immutable) (out : result @ immutable) = ghost_ (
  match out with
  | Initialized (start, prepared) -> Init.correct program layout.heap_base layout.heap_limit input (Init.Initialized start)
    && installed program layout input memory start prepared && layout_accepts program layout input
  | Heap_exhausted (heap, code) -> Init.correct program layout.heap_base layout.heap_limit input (Init.Heap_exhausted (heap, code))
    && not (layout_accepts program layout input)
  | Layout_rejected -> not (layout_accepts program layout input))
let (initialize @ total) : (program : I.program) @ immutable -> (layout : layout) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (memory : B.bytes) @ immutable -> {u : unit | valid_layout layout memory} ->
    {out : result | correct program layout input memory out} @ immutable =
  fun program layout input memory premise ->
    ghost_ (valid_layout_def layout memory; layout_accepts_def program layout input);
    let source = Init.initialize program layout.heap_base layout.heap_limit input () in
    match source with
    | Init.Heap_exhausted (heap, code) ->
      let out = Heap_exhausted (heap, code) in ghost_ (correct_def program layout input memory out); out
    | Init.Initialized start ->
      match install program layout input memory start () with
      | None -> ghost_ (correct_def program layout input memory Layout_rejected); Layout_rejected
      | Some prepared ->
        let out = Initialized (start, prepared) in ghost_ (correct_def program layout input memory out); out

let (sufficient @ total) : (program : I.program) @ immutable -> (layout : layout) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (memory : B.bytes) @ immutable ->
    {u : unit | valid_layout layout memory && layout_accepts program layout input} ->
    {out : Init.start * prepared | let start, prepared = out in correct program layout input memory (Initialized (start, prepared))} @ immutable =
  fun program layout input memory premise ->
    let out = initialize program layout input memory () in
    ghost_ (correct_def program layout input memory out);
    match out with Initialized (start, prepared) -> start, prepared | _ -> unreachable_ ()
