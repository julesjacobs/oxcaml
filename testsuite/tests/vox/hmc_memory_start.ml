module D = Hm_declarative
module W = Hmc_word64
module B = Wasm_u32
module V = Hmc_tagged_cell
module G = Hmc_cfg_ir
module K = Hmc_closure_ir
module C = Hmc_cfg_program
module O = Hmc_closure_program
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
module M = Hmc_heap_objects
module X = Hmc_heap_machine
module Q = Hmc_heap_state
module H = Hmc_heap_invariant
module E = Hmc_heap_extent
module Index = Hmc_u32_index
module Bounds = Hmc_linear_bounds
module Image = Hmc_heap_image
module Saved = Hmc_memory_saved_frame
module Stack = Hmc_memory_stack
module Machine = Hmc_memory_active_machine
module Init = Hmc_memory_initialize
module Old = Hmc_heap_initialize
module Static = Hmc_memory_static

type layout = {code_capacity : W.limb; width : W.limb; heap_base : W.limb; heap_limit : W.limb;
  stack_base : W.limb; stack_limit : W.limb; active : W.limb; active_end : W.limb; memory_limit : W.limb}
let[@def] (valid @ total) (program : I.program @ immutable) (layout : layout @ immutable) = ghost_ (
  Index.fits (K.size program.I.origin.C.origin.O.table) layout.code_capacity
  && Index.fits (G.size program.I.origin.C.blocks) layout.code_capacity
  && layout.width > 0 && E.span (Saved.slots program.I.origin.C.blocks) (Stack.zero ()) layout.width
  && layout.heap_base <= layout.heap_limit && layout.heap_limit <= layout.stack_base && layout.stack_base <= layout.stack_limit
  && layout.stack_limit <= layout.active && E.span (Saved.slots program.I.origin.C.blocks) layout.active layout.active_end
  && layout.active_end <= layout.memory_limit)
type start = {globals : X.globals; configuration : Machine.configuration; model : X.configuration @@ ghost}
type result = Initialized of start | Heap_exhausted of Static.image * D.index [@@inductive]
let[@def] (correct @ total) (program : I.program @ immutable) (layout : layout @ immutable) (before : B.bytes @ immutable)
    (input : W.t @ immutable) (out : result @ immutable) = ghost_ (match out with
  | Initialized start ->
    Old.correct program layout.heap_base layout.heap_limit input (Old.Initialized {Old.globals = start.globals; configuration = start.model})
    && Machine.related program.I.origin.C.blocks layout.width layout.stack_base layout.active start.configuration start.model
    && start.configuration.Machine.top = layout.stack_base && start.configuration.Machine.status === Machine.Running
    && Bounds.covers start.configuration.Machine.memory layout.heap_limit && Bounds.covers start.configuration.Machine.memory layout.stack_limit
    && Bounds.covers start.configuration.Machine.memory layout.memory_limit && V.length start.configuration.Machine.memory === V.length before
  | Heap_exhausted (image, code) ->
    Old.correct program layout.heap_base layout.heap_limit input (Old.Heap_exhausted (image.Static.heap, code))
    && image.Static.frontier = M.used image.Static.heap && Image.related image.Static.memory image.Static.heap
    && Bounds.covers image.Static.memory layout.memory_limit && V.length image.Static.memory === V.length before)
let (initialize @ total) : (program : I.program) @ immutable -> (layout : layout) @ immutable -> (memory : B.bytes) @ immutable ->
    (input : W.t) @ immutable -> {u : unit | valid program layout && Bounds.covers memory layout.memory_limit} ->
    {out : result | correct program layout memory input out} @ immutable = fun program layout memory input premise ->
  ghost_ (valid_def program layout; E.ordered (Saved.slots program.I.origin.C.blocks) layout.active layout.active_end ();
    let _ = Bounds.suffix memory layout.memory_limit layout.heap_limit () in Bounds.covers_def memory layout.heap_limit;
    let _ = Bounds.suffix memory layout.memory_limit layout.stack_limit () in Bounds.covers_def memory layout.stack_limit);
  let initialized = Init.initialize program layout.code_capacity memory layout.heap_base layout.heap_limit input () in
  ghost_ (Init.correct_def program memory layout.heap_base layout.heap_limit input initialized);
  match initialized with
  | Init.Heap_exhausted (image, code) ->
    let out = Heap_exhausted (image, code) in
    ghost_ (Bounds.same_length memory image.Static.memory layout.memory_limit (); correct_def program layout memory input out); out
  | Init.Initialized start ->
    ghost_ (Old.correct_def program layout.heap_base layout.heap_limit input (Old.Initialized {Old.globals = start.Init.globals; configuration = start.Init.model});
      H.valid_def program start.Init.globals layout.heap_limit start.Init.model (U.initial program input);
      Hmc_memory_current_shape.current program start.Init.globals layout.heap_limit start.Init.model (U.initial program input) ();
      Bounds.same_length memory start.Init.memory layout.memory_limit ());
    let updated = Saved.store program.I.origin.C.blocks layout.code_capacity start.Init.memory layout.memory_limit layout.active layout.active_end start.Init.entered () in
    ghost_ (Image.preserve program.I.origin.C.origin.O.table start.Init.memory updated start.Init.model.X.heap layout.active ();
      Bounds.same_length memory updated layout.heap_limit (); Bounds.same_length memory updated layout.stack_limit ();
      Stack.related_def program.I.origin.C.blocks layout.width updated layout.stack_base layout.stack_base Q.Halt);
    let configuration = {Machine.memory = updated; frontier = start.Init.frontier; top = layout.stack_base; status = Machine.Running} in
    let out = Initialized {globals = start.Init.globals; configuration; model = ghost_ start.Init.model} in
    ghost_ (Machine.related_def program.I.origin.C.blocks layout.width layout.stack_base layout.active configuration start.Init.model;
      correct_def program layout memory input out); out
