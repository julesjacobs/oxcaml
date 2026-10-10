module D = Hm_declarative
module W = Hmc_word64
module M = Hmc_heap_objects
module V = Hmc_tagged_cell
module R = Hmc_closure_semantics
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module X = Hmc_heap_machine
module L = Hmc_heap_machine_proofs
module B = Hmc_heap_static
module Model_alloc = Hmc_heap_allocate
module A = Hmc_memory_allocate
module P = Hmc_heap_preservation
module J = Hmc_heap_globals
module H = Hmc_heap_invariant
module K = Hmc_closure_ir
module C = Hmc_cfg_program
module O = Hmc_closure_program
module N = Hmc_monomorphic
module T = Hmc_templates
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
module S = Hmc_cfg_semantics
module E = Hmc_heap_extent
module Math = Hmc_heap_extent_math

module Old = Hmc_heap_initialize
module Static = Hmc_memory_static
module Bytes = Wasm_u32
module Image = Hmc_heap_image
module Bounds = Hmc_linear_bounds
module Index = Hmc_u32_index
module Invoke = Hmc_memory_invoke

type start = {memory : Bytes.bytes; frontier : W.limb; globals : X.globals; entered : F.activation; model : X.configuration @@ ghost}
type result = Initialized of start | Heap_exhausted of Static.image * D.index [@@inductive]
let[@def] (correct @ total) (program : I.program @ immutable) (before : Bytes.bytes @ immutable) (base : W.limb) (limit : W.limb)
    (input : W.t @ immutable) (out : result @ immutable) = ghost_ (match out with
  | Initialized start ->
    Old.correct program base limit input (Old.Initialized {Old.globals = start.globals; configuration = start.model})
    && start.frontier = M.used start.model.X.heap && start.model.X.state === Q.Running (start.entered, Q.Halt)
    && Image.related start.memory start.model.X.heap && Bounds.covers start.memory limit
    && V.length start.memory === V.length before && Hmc_linear_bytes.drop before limit === Hmc_linear_bytes.drop start.memory limit
  | Heap_exhausted (image, code) ->
    Old.correct program base limit input (Old.Heap_exhausted (image.Static.heap, code))
    && image.Static.frontier = M.used image.Static.heap && Image.related image.Static.memory image.Static.heap
    && Bounds.covers image.Static.memory limit && V.length image.Static.memory === V.length before
    && Hmc_linear_bytes.drop before limit === Hmc_linear_bytes.drop image.Static.memory limit)
let (initialize @ total) : (program : I.program) @ immutable -> (capacity : W.limb) -> (memory : Bytes.bytes) @ immutable ->
    (base : W.limb) -> (limit : W.limb) -> (input : W.t) @ immutable ->
    {u : unit | base <= limit && Index.fits (K.size program.I.origin.C.origin.O.table) capacity && Bounds.covers memory limit} ->
    {out : result | correct program memory base limit input out} @ immutable = fun program capacity memory base limit input premise ->
  let closure = program.I.origin.C.origin in
  let table = closure.O.table in
  let heap = ghost_ (M.Empty_heap base) in
  ghost_ (Old.cells_def program; Math.add_one (O.size closure.O.globals); O.valid_def closure; M.valid_def table heap; M.used_def heap;
    Image.related_def memory heap;
    B.mapped_closed (N.manifest closure.O.origin.N.definitions) table closure.O.origin.N.definitions closure.O.globals ());
  let globals = Static.initialize table capacity closure.O.globals memory base heap limit () in
  ghost_ (Static.correct_def table closure.O.globals heap memory limit globals);
  match globals with
  | Static.Full (image, code) ->
    let out = Heap_exhausted (image, code) in ghost_ (
      B.correct_def table closure.O.globals heap limit (B.Full (image.Static.heap, code));
      if E.fits (Old.cells program) base limit then Math.prefix (O.size closure.O.globals) (D.S D.Z) base limit () else ();
      Old.correct_def program base limit input (Old.Heap_exhausted (image.Static.heap, code));
      correct_def program memory base limit input out); out
  | Static.Ready (image, globals) ->
    let partial = ghost_ image.Static.heap in
    let id = closure.O.entry in
    ghost_ (B.correct_def table closure.O.globals heap limit (B.Ready (partial, globals));
      Hmc_frame_values.closure (N.manifest closure.O.origin.N.definitions) table D.Empty_context id
      (D.Function (D.Word64, D.Word64)) closure.O.origin.N.source.T.derivation ();
      Hmc_frame_shape.value_def table (R.V.Closure (id, R.V.Empty));
      Hmc_frame_shape.valid_def table (R.V.Closure (id, R.V.Empty)); Hmc_frame_shape.first_class_def (R.V.Closure (id, R.V.Empty));
      Hmc_frame_shape.valid_def table R.V.Empty; Hmc_frame_shape.environment_def D.Empty_context R.V.Empty;
      B.object_valid table partial id ());
    let allocation = A.allocate table capacity image.Static.memory partial image.Static.frontier limit (M.Closure (id, M.Empty)) () in
    ghost_ (A.correct_def table image.Static.memory partial limit (M.Closure (id, M.Empty)) allocation);
    match allocation with
    | A.Exhausted ->
      let out = Heap_exhausted (image, id) in ghost_ (
        M.slots_def (M.Closure (id, M.Empty)); M.length_def M.Empty;
        if E.fits (Old.cells program) base limit then Math.consume (O.size closure.O.globals) (D.S D.Z) base (M.used partial) limit () else ();
        Old.correct_def program base limit input (Old.Heap_exhausted (partial, id)); correct_def program memory base limit input out); out
    | A.Allocated allocated ->
      let next = ghost_ (M.Allocate ({M.address = image.Static.frontier; stop = allocated.A.frontier; object_ = M.Closure (id, M.Empty)}, partial)) in
      ghost_ (Model_alloc.correct_def table partial limit (M.Closure (id, M.Empty)) (Model_alloc.Allocated {Model_alloc.heap = next; reference = allocated.A.reference});
        M.slots_def (M.Closure (id, M.Empty)); M.length_def M.Empty;
        Math.join (O.size closure.O.globals) (D.S D.Z) base (M.used partial) (M.used next) ();
        M.decode_object_def (M.view partial) (M.Closure (id, M.Empty)); M.decode_environment_def (M.view partial) M.Empty;
        M.decode_def next (V.Word input); M.decode_value_def (M.view next) (V.Word input);
        L.invoke program next allocated.A.reference (V.Word input) (R.V.Closure (id, R.V.Empty)) (R.V.Word input) ();
        Invoke.correct program allocated.A.memory next allocated.A.reference (V.Word input) (R.V.Closure (id, R.V.Empty)) ();
        L.abstract_invoke_def program (R.V.Closure (id, R.V.Empty)) (R.V.Word input);
        C.valid_def program.I.origin;
        (match K.lookup table id with None -> () | Some entry ->
          let _ = C.lookup_origin program.I.origin.C.blocks table program.I.origin.C.functions id entry () in ());
        U.initial_def program input; S.initial_def program.I.origin input);
      match Invoke.invoke program allocated.A.memory allocated.A.reference (V.Word input) with
      | None -> unreachable_ ()
      | Some entered ->
        let model = ghost_ ({X.heap = next; state = Q.Running (entered, Q.Halt)}) in
        ghost_ (Q.decode_def next model.X.state; Q.decode_frames_def next Q.Halt;
          J.preserve table next partial closure.O.globals globals ();
          P.transitive next partial heap ();
          Hmc_frame_reachable.initial program input;
          H.valid_def program globals limit model (U.initial program input);
          Old.correct_def program base limit input (Old.Initialized {Old.globals; configuration = model}));
        let out = Initialized {memory = allocated.A.memory; frontier = allocated.A.frontier; globals; entered; model} in
        ghost_ (correct_def program memory base limit input out); out
