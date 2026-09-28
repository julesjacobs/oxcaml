module D = Hm_declarative
module W = Hmc_word64
module Index = Hmc_u32_index

let rec (bounded @ total) : (index : D.index) @ immutable -> (number : W.limb) -> (limit : W.limb) ->
    {u : unit | Index.represents index number && number <= limit} ->
    {u : unit | Index.fits index limit} @ ghost = fun index number limit premise -> ghost_ (
  Index.represents_def index number; Index.fits_def index limit;
  match index with D.Z -> () | D.S rest -> bounded rest (number - 1) (limit - 1) ())

let[@def] (frame_room @ total) (start : W.limb) (limit : W.limb) : W.limb =
  if start <= limit then limit - start else 0

module I = Hmc_tail_ir
module G = Hmc_cfg_program
module State = Hmc_wasm_program_state
module Lower = Hmc_wasm_program_lower
module Registers = Hmc_wasm_program_registers
module Frame = Hmc_wasm_program_frame
module Resources = Hmc_wasm_program_resources
module Heap = Hmc_heap_objects
module Cap = Hmc_frame_capacity

let (frame_capacity @ total) : (program : I.program) @ immutable ->
    (globals : Hmc_heap_machine.globals) @ immutable -> (lowered : Lower.program) @ immutable ->
    (context : State.context) @ immutable -> (state : State.running) @ immutable ->
    {u : unit | State.valid program globals lowered context state} ->
    {u : unit | state.State.registers.Registers.frame <= context.State.stack_base
      && Index.fits (Cap.capacity program.I.origin.G.blocks)
        (frame_room state.State.registers.Registers.frame context.State.stack_base)} @ ghost =
  fun program globals lowered context state premise -> ghost_ (
    State.valid_def program globals lowered context state;
    Lower.corresponds_def program globals context.State.max_pc lowered;
    Frame.valid_def state.State.block.Hmc_cfg_ir.signature state.State.activation state.State.registers
      state.State.memory state.State.frame_end state.State.pc state.State.cells state.State.padding
      state.State.bytes state.State.suffix state.State.cell_count;
    Resources.valid_def program globals lowered.Lower.width context.State.stack_base state.State.frame_end
      state.State.abstract state.State.heap state.State.activation state.State.frames state.State.registers state.State.memory;
    Index.represents_def (D.S (Heap.length state.State.cells)) state.State.cell_count;
    Index.unique (Heap.length state.State.cells) lowered.Lower.capacity (state.State.cell_count - 1) ();
    frame_room_def state.State.registers.Registers.frame context.State.stack_base;
    bounded (Cap.capacity program.I.origin.G.blocks) lowered.Lower.capacity
      (frame_room state.State.registers.Registers.frame context.State.stack_base) ())

module Extent = Hmc_heap_extent
module Bound = Hmc_heap_bound
module Height = Hmc_cfg_height

let[@def] rec (heap_budget @ total) (steps : D.index @ immutable) (width : W.limb)
    (start : W.limb) (limit : W.limb) = ghost_ (
  if start > limit then false else match steps with
  | D.Z -> true
  | D.S rest ->
    if width > 268435455 || 16 * width > limit - start then false
    else heap_budget rest width (start + 16 * width) limit)

let rec (fits_earlier @ total) : (cells : D.index) @ immutable -> (start : W.limb) ->
    (later : W.limb) -> (limit : W.limb) ->
    {u : unit | start <= later && Extent.fits cells later limit} ->
    {u : unit | Extent.fits cells start limit} @ ghost = fun cells start later limit premise -> ghost_ (
  Extent.fits_def cells start limit; Extent.fits_def cells later limit;
  match cells with D.Z -> () | D.S rest -> fits_earlier rest (start + 16) (later + 16) limit ())

let rec (fits_fewer @ total) : (cells : D.index) @ immutable -> (bound : D.index) @ immutable ->
    (start : W.limb) -> (limit : W.limb) ->
    {u : unit | Height.le cells bound && Extent.fits bound start limit} ->
    {u : unit | Extent.fits cells start limit} @ ghost = fun cells bound start limit premise -> ghost_ (
  Height.le_def cells bound; Extent.fits_def cells start limit; Extent.fits_def bound start limit;
  match cells, bound with D.S cells, D.S bound -> fits_fewer cells bound (start + 16) limit () | _ -> ())

let rec (consume @ total) : (cells : D.index) @ immutable -> (rest : D.index) @ immutable ->
    (width : W.limb) -> (start : W.limb) -> (stop : W.limb) -> (limit : W.limb) ->
    {u : unit | Index.fits cells width && width <= 268435455 && 16 * width <= limit - start
      && stop = start + 16 * width && Extent.fits rest stop limit} ->
    {u : unit | Extent.fits (D.add cells rest) start limit} @ ghost = fun cells rest width start stop limit premise -> ghost_ (
  Index.fits_def cells width; D.add_def cells rest;
  match cells with
  | D.Z -> fits_earlier rest start stop limit ()
  | D.S tail ->
    Extent.fits_def (D.add cells rest) start limit;
    consume tail rest (width - 1) (start + 16) stop limit ())

let rec (product_fits @ total) : (steps : D.index) @ immutable -> (cells : D.index) @ immutable ->
    (width : W.limb) -> (start : W.limb) -> (limit : W.limb) ->
    {u : unit | Index.fits cells width && heap_budget steps width start limit} ->
    {u : unit | Extent.fits (Bound.product steps cells) start limit} @ ghost = fun steps cells width start limit premise -> ghost_ (
  heap_budget_def steps width start limit; Bound.product_def steps cells;
  match steps with
  | D.Z -> Extent.fits_def D.Z start limit
  | D.S rest ->
    product_fits rest cells width (start + 16 * width) limit ();
    consume cells (Bound.product rest cells) width start (start + 16 * width) limit ())
